# Company A - Production Planning LP (R)
# Goal: Meet weekly due demand with capacity constraints; minimize holding + stockout penalties.
# Uses RAW data tables to derive parameters (unit machine-hours per SKU, weekly demand, capacity).

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# Optional solver packages (auto-install if missing)
ensure_pkg <- function(p){
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
ensure_pkg("lpSolve")
library(lpSolve)
# ---- Path helper: make scripts runnable from ANY working directory (Windows-friendly) ----
get_script_dir <- function(){
  # Works for: Rscript, source(), and RStudio
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArgName <- "--file="
  m <- grep(fileArgName, cmdArgs)
  if (length(m) > 0) return(dirname(normalizePath(sub(fileArgName, "", cmdArgs[m]), winslash="/")))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash="/")))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p, winslash="/")))
  }
  return(normalizePath(getwd(), winslash="/"))
}

PROJECT_ROOT <- get_script_dir()
base_dir <- file.path(PROJECT_ROOT, "companyA_synthetic")

scenario <- "before"  # optimize from BEFORE baseline

# ---------- Load ----------
d <- file.path(base_dir, scenario)
prod  <- read_csv(file.path(d,"production_events.csv"), show_col_types = FALSE) %>%
  mutate(step_start_time = ymd_hms(step_start_time),
         step_end_time   = ymd_hms(step_end_time))
lines <- read_csv(file.path(d,"production_lines.csv"), show_col_types = FALSE)
skus  <- read_csv(file.path(d,"skus.csv"), show_col_types = FALSE)
dem   <- read_csv(file.path(d,"demand_orders.csv"), show_col_types = FALSE) %>%
  mutate(order_date = ymd(order_date),
         due_date   = ymd(due_date))

inv_snap <- read_csv(file.path(d,"inventory_snapshots.csv"), show_col_types = FALSE) %>%
  mutate(snapshot_date = ymd(snapshot_date))

# ---------- Build weeks ----------
dem <- dem %>%
  mutate(week = format(floor_date(due_date, unit="week", week_start=1), "%Y-%m-%d"))
weeks <- sort(unique(dem$week))

# Weekly demand by SKU (due-based)
demand_wk <- dem %>%
  group_by(week, sku_id) %>%
  summarise(demand_qty = sum(demand_qty), avg_penalty = mean(penalty_cost_late), .groups="drop")

# Fill missing (week,sku)=0 for stable matrix
sku_list <- sort(unique(skus$sku_id))
grid <- expand_grid(week = weeks, sku_id = sku_list) %>%
  left_join(demand_wk, by=c("week","sku_id")) %>%
  mutate(demand_qty = replace_na(demand_qty, 0),
         avg_penalty = replace_na(avg_penalty, median(dem$penalty_cost_late)))

# ---------- Derive unit machine-hours per SKU (from raw events) ----------
unit_mh <- prod %>%
  filter(produced_qty > 0) %>%
  group_by(sku_id) %>%
  summarise(
    mh_per_unit = sum(machine_hours, na.rm=TRUE) / sum(produced_qty, na.rm=TRUE),
    .groups="drop"
  )

unit_mh <- unit_mh %>%
  right_join(skus %>% select(sku_id, product_category), by="sku_id") %>%
  group_by(product_category) %>%
  mutate(mh_per_unit = if_else(is.na(mh_per_unit),
                               median(mh_per_unit, na.rm=TRUE),
                               mh_per_unit)) %>%
  ungroup() %>%
  mutate(mh_per_unit = pmax(0.01, mh_per_unit)) %>%
  select(sku_id, mh_per_unit)

# ---------- Capacity by line-week ----------
shift_hours <- c("2x8h"=16, "3x8h"=24, "1x10h"=10)
lines_cap <- lines %>%
  mutate(
    daily_capacity_machine_hours = if_else(is.na(daily_capacity_machine_hours),
                                           as.numeric(shift_hours[shift_pattern])*0.85,
                                           daily_capacity_machine_hours),
    weekly_capacity_machine_hours = daily_capacity_machine_hours * 6
  ) %>%
  select(factory_id, production_line_id, weekly_capacity_machine_hours)

line_list <- sort(unique(lines_cap$production_line_id))

# ---------- SKU eligibility by factory ----------
sku_factory <- prod %>%
  count(sku_id, factory_id, sort=TRUE) %>%
  group_by(sku_id) %>%
  slice_max(n, n=1, with_ties=FALSE) %>%
  ungroup() %>%
  select(sku_id, factory_id)

fallback <- skus %>%
  mutate(factory_id = if_else(product_category %in% c("Tee","Polo","Short","Dress"), "F1", "F2")) %>%
  select(sku_id, factory_id)

sku_factory <- fallback %>% left_join(sku_factory, by="sku_id", suffix=c("_fb","_hist")) %>%
  mutate(factory_id = coalesce(factory_id_hist, factory_id_fb)) %>%
  select(sku_id, factory_id)

eligible <- sku_factory %>%
  left_join(lines_cap %>% select(factory_id, production_line_id), by="factory_id", relationship="many-to-many") %>%
  distinct(sku_id, production_line_id)

# ---------- Costs & initial inventory ----------
costs <- inv_snap %>%
  group_by(sku_id) %>%
  summarise(
    hold_cost_day = mean(holding_cost_per_unit_per_day, na.rm=TRUE),
    stockout_cost = mean(stockout_cost_per_unit, na.rm=TRUE),
    .groups="drop"
  )

global_hold <- mean(costs$hold_cost_day, na.rm=TRUE)
global_stockout <- mean(costs$stockout_cost, na.rm=TRUE)
costs <- tibble(sku_id=sku_list) %>%
  left_join(costs, by="sku_id") %>%
  mutate(
    hold_cost_day = replace_na(hold_cost_day, global_hold),
    stockout_cost = replace_na(stockout_cost, global_stockout),
    hold_cost_week = hold_cost_day * 7
  ) %>%
  select(sku_id, hold_cost_week, stockout_cost)

inv0 <- inv_snap %>%
  filter(location_id %in% c("WH_NORTH","WH_SOUTH")) %>%
  arrange(snapshot_date)
first_date <- if (nrow(inv0)>0) min(inv0$snapshot_date) else NA

inv0_by_sku <- inv_snap %>%
  filter(location_id %in% c("WH_NORTH","WH_SOUTH")) %>%
  filter(snapshot_date == first_date) %>%
  group_by(sku_id) %>%
  summarise(inv0 = sum(on_hand_qty, na.rm=TRUE), .groups="drop")

inv0_by_sku <- tibble(sku_id=sku_list) %>%
  left_join(inv0_by_sku, by="sku_id") %>%
  mutate(inv0 = replace_na(inv0, 0))

# ---------- LP ----------
W <- length(weeks); S <- length(sku_list); L <- length(line_list)
n_x <- W*S*L
n_I <- W*S
n_B <- W*S
n_var <- n_x + n_I + n_B

mh_map <- unit_mh %>% deframe()
hold_map <- costs %>% select(sku_id, hold_cost_week) %>% deframe()
stock_map <- costs %>% select(sku_id, stockout_cost) %>% deframe()
penalty_mat <- grid %>% select(week, sku_id, avg_penalty) %>%
  mutate(key=paste0(week,"|",sku_id)) %>% select(key, avg_penalty) %>% deframe()

obj <- rep(0, n_var)
offset_I <- n_x
offset_B <- n_x + n_I

for (w in seq_len(W)){
  for (s in seq_len(S)){
    Ipos <- offset_I + (w-1)*S + s
    Bpos <- offset_B + (w-1)*S + s
    sku <- sku_list[s]
    obj[Ipos] <- hold_map[[sku]]
    key <- paste0(weeks[w],"|",sku)
    obj[Bpos] <- stock_map[[sku]] + penalty_mat[[key]]
  }
}


zero_pos <- c()
elig_set <- eligible %>% mutate(key=paste0(sku_id,"|",production_line_id)) %>% pull(key) %>% unique()
for (w in seq_len(W)){
  for (s in seq_len(S)){
    for (l in seq_len(L)){
      sku <- sku_list[s]; line <- line_list[l]
      key <- paste0(sku,"|",line)
      if (!(key %in% elig_set)){
        pos <- (w-1)*S*L + (s-1)*L + l
        zero_pos <- c(zero_pos, pos)
      }
    }
  }
}

con_mat <- list()
con_dir <- c()
con_rhs <- c()

cap_map <- lines_cap %>% select(production_line_id, weekly_capacity_machine_hours) %>% deframe()

# Capacity constraints
for (w in seq_len(W)){
  for (l in seq_len(L)){
    row <- rep(0, n_var)
    for (s in seq_len(S)){
      pos <- (w-1)*S*L + (s-1)*L + l
      sku <- sku_list[s]
      row[pos] <- mh_map[[sku]]
    }
    con_mat[[length(con_mat)+1]] <- row
    con_dir <- c(con_dir, "<=")
    con_rhs <- c(con_rhs, cap_map[[line_list[l]]])
  }
}

demand_map <- grid %>% mutate(key=paste0(week,"|",sku_id)) %>% select(key, demand_qty) %>% deframe()


# Ineligible variable constraints: force x[w,s,l] = 0 where SKU cannot run on that line
# (implemented as x <= 0 since x is non-negative)
if (length(zero_pos) > 0){
  for (p in zero_pos){
    row <- rep(0, n_var)
    row[p] <- 1
    con_mat[[length(con_mat)+1]] <- row
    con_dir <- c(con_dir, "<=")
    con_rhs <- c(con_rhs, 0)
  }
}

# Balance constraints
for (w in seq_len(W)){
  for (s in seq_len(S)){
    row <- rep(0, n_var)
    Ipos <- offset_I + (w-1)*S + s
    Bpos <- offset_B + (w-1)*S + s
    row[Ipos] <- 1
    row[Bpos] <- -1

    rhs0 <- 0
    if (w > 1){
      Iprev <- offset_I + (w-2)*S + s
      Bprev <- offset_B + (w-2)*S + s
      row[Iprev] <- -1
      row[Bprev] <- 1
    } else {
      inv0_val <- inv0_by_sku$inv0[inv0_by_sku$sku_id==sku_list[s]]
      rhs0 <- inv0_val
    }

    for (l in seq_len(L)){
      x_pos <- (w-1)*S*L + (s-1)*L + l
      row[x_pos] <- row[x_pos] - 1
    }

    dem_key <- paste0(weeks[w],"|",sku_list[s])
    dem_qty <- demand_map[[dem_key]]
    rhs <- if (w==1) (rhs0 - dem_qty) else (0 - dem_qty)

    con_mat[[length(con_mat)+1]] <- row
    con_dir <- c(con_dir, "=")
    con_rhs <- c(con_rhs, rhs)
  }
}

con <- do.call(rbind, con_mat)

sol <- lp(direction="min",
          objective.in=obj,
          const.mat=con,
          const.dir=con_dir,
          const.rhs=con_rhs,
          all.int=FALSE)

stopifnot(sol$status == 0)

x_sol <- sol$solution[1:n_x]
I_sol <- sol$solution[(n_x+1):(n_x+n_I)]
B_sol <- sol$solution[(n_x+n_I+1):n_var]

plan <- expand_grid(w = seq_len(W), s=seq_len(S), l=seq_len(L)) %>%
  mutate(pos = (w-1)*S*L + (s-1)*L + l,
         week = weeks[w],
         sku_id = sku_list[s],
         production_line_id = line_list[l],
         planned_qty = x_sol[pos]) %>%
  filter(planned_qty > 1e-6) %>%
  mutate(planned_qty = round(planned_qty, 0))

proj <- expand_grid(w=seq_len(W), s=seq_len(S)) %>%
  mutate(week=weeks[w], sku_id=sku_list[s],
         ending_inventory = I_sol[(w-1)*S + s],
         ending_backorder = B_sol[(w-1)*S + s]) %>%
  mutate(ending_inventory = round(ending_inventory,2),
         ending_backorder = round(ending_backorder,2)) %>%
  filter(ending_inventory > 0 | ending_backorder > 0)

obj_val <- sol$objval
hold_cost <- sum(obj[(n_x+1):(n_x+n_I)] * I_sol)
back_cost <- sum(obj[(n_x+n_I+1):n_var] * B_sol)

costs_out <- tibble(
  total_objective = obj_val,
  holding_cost = hold_cost,
  shortage_cost = back_cost
)

outdir <- file.path(base_dir, "optimization_outputs_r")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

write_csv(plan, file.path(outdir, "production_plan_weekly_lp.csv"))
write_csv(proj, file.path(outdir, "inventory_backorder_projection_lp.csv"))
write_csv(costs_out, file.path(outdir, "lp_cost_breakdown.csv"))

message("Wrote production LP outputs to: ", outdir)
