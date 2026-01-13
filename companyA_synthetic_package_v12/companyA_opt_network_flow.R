# Company A - Distribution Network Flow (R)
# Min-cost weekly shipments WH -> DC to satisfy DC demand, subject to lane capacity and WH supply.
# Implemented as LP (min-cost flow).

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

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

scenario <- "before"

d <- file.path(base_dir, scenario)
lanes <- read_csv(file.path(d,"transportation_lanes.csv"), show_col_types = FALSE) %>%
  filter(from_node %in% c("WH_NORTH","WH_SOUTH"),
         str_detect(to_node, "^DC_")) %>%
  mutate(weekly_capacity = capacity_units_per_day * 6)

customers <- read_csv(file.path(d,"customers.csv"), show_col_types = FALSE)
dem <- read_csv(file.path(d,"demand_orders.csv"), show_col_types = FALSE) %>%
  mutate(due_date = ymd(due_date))

city_to_dc <- c("Hanoi"="DC_HN","Hai Phong"="DC_HN","Da Nang"="DC_DN","Nha Trang"="DC_DN","HCMC"="DC_HCMC","Can Tho"="DC_CT")
dem <- dem %>%
  left_join(customers %>% select(customer_id, city), by="customer_id") %>%
  mutate(dc_id = city_to_dc[city],
         week = format(floor_date(due_date, unit="week", week_start=1), "%Y-%m-%d"))

weeks <- sort(unique(dem$week))
dcs <- sort(unique(dem$dc_id))
origins <- sort(unique(lanes$from_node))

dem_dc <- dem %>%
  group_by(week, dc_id) %>%
  summarise(demand_units = sum(demand_qty), .groups="drop")

opt_dir <- file.path(base_dir, "optimization_outputs_r")
plan_path <- file.path(opt_dir, "production_plan_weekly_lp.csv")

if (file.exists(plan_path)) {
  plan <- read_csv(plan_path, show_col_types = FALSE) %>%
    mutate(week = as.character(week))
  lines <- read_csv(file.path(d,"production_lines.csv"), show_col_types = FALSE)
  plan2 <- plan %>%
    left_join(lines %>% select(production_line_id, factory_id), by="production_line_id") %>%
    mutate(wh = if_else(factory_id=="F1","WH_NORTH","WH_SOUTH")) %>%
    group_by(week, wh) %>%
    summarise(supply_units = sum(planned_qty), .groups="drop") %>%
    mutate(week = as.character(week))
} else {
  sh <- read_csv(file.path(d,"shipments.csv"), show_col_types = FALSE) %>%
    mutate(ship_date = ymd(ship_date),
           week = format(floor_date(ship_date, unit="week", week_start=1), "%Y-%m-%d"))
  plan2 <- sh %>%
    group_by(week, from_node) %>%
    summarise(supply_units = sum(shipped_qty)*1.05, .groups="drop") %>%
    rename(wh = from_node)
}

supply_grid <- expand_grid(week=weeks, wh=origins) %>%
  left_join(plan2, by=c("week","wh")) %>%
  mutate(supply_units = replace_na(supply_units, 0))

dem_grid <- expand_grid(week=weeks, dc_id=dcs) %>%
  left_join(dem_dc, by=c("week","dc_id")) %>%
  mutate(demand_units = replace_na(demand_units, 0))

out_rows <- list()
cost_rows <- list()


for (wk in weeks){
  supply_wk <- supply_grid %>% filter(week==wk)
  demand_wk <- dem_grid %>% filter(week==wk)

  # Lane variables x[o,dc] for all origin-dc pairs (0 if lane missing)
  pairs <- expand_grid(from_node=origins, to_node=dcs) %>%
    mutate(key=paste0(from_node,"|",to_node)) %>%
    left_join(lanes %>% mutate(key=paste0(from_node,"|",to_node)) %>%
                select(key, transport_cost_per_unit, weekly_capacity), by="key")

  n_x <- nrow(pairs)
  # Slack variables u[dc] = unmet demand (always >=0) so model is always feasible
  n_u <- length(dcs)
  n <- n_x + n_u

  # Objective: transport cost + big penalty for unmet
  obj <- c(
    (pairs$transport_cost_per_unit %>% replace_na(1e6)),
    rep(1e4, n_u)  # Big-M penalty per unmet unit
  )

  cap_vec <- pairs$weekly_capacity
  cap_vec[is.na(cap_vec)] <- 0

  con <- list(); dir <- c(); rhs <- c()

  # (1) Origin supply: sum_dc x[o,dc] <= supply[o]
  for (o in origins){
    row <- rep(0, n)
    row[which(pairs$from_node==o)] <- 1
    con[[length(con)+1]] <- row
    dir <- c(dir, "<=")
    rhs <- c(rhs, supply_wk$supply_units[supply_wk$wh==o])
  }

  # (2) DC demand with slack: sum_o x[o,dc] + u[dc] >= demand[dc]
  for (i in seq_along(dcs)){
    dc <- dcs[i]
    row <- rep(0, n)
    row[which(pairs$to_node==dc)] <- 1
    row[n_x + i] <- 1  # unmet variable for this DC
    con[[length(con)+1]] <- row
    dir <- c(dir, ">=")
    rhs <- c(rhs, demand_wk$demand_units[demand_wk$dc_id==dc])
  }

  # (3) Lane capacity: x_i <= cap_i
  for (i in seq_len(n_x)){
    row <- rep(0, n)
    row[i] <- 1
    con[[length(con)+1]] <- row
    dir <- c(dir, "<=")
    rhs <- c(rhs, cap_vec[i])
  }

  con_mat <- do.call(rbind, con)

  sol <- lp(direction="min",
            objective.in=obj,
            const.mat=con_mat,
            const.dir=dir,
            const.rhs=rhs,
            all.int=FALSE)

  stopifnot(sol$status==0)

  x <- sol$solution[1:n_x]
  u <- sol$solution[(n_x+1):n]

  shipped <- pairs %>%
    mutate(week=wk, ship_units=x) %>%
    filter(ship_units > 1e-6) %>%
    mutate(ship_units = round(ship_units,0))

  out_rows[[length(out_rows)+1]] <- shipped

  unmet_units_total <- sum(u)

  cost_rows[[length(cost_rows)+1]] <- tibble(
    week = wk,
    transport_cost = sum((pairs$transport_cost_per_unit %>% replace_na(1e6)) * x),
    total_demand = sum(demand_wk$demand_units),
    total_supply = sum(supply_wk$supply_units),
    unmet_units = unmet_units_total
  )
}

ship_plan <- bind_rows(out_rows)
cost_summary <- bind_rows(cost_rows)

outdir <- file.path(base_dir, "optimization_outputs_r")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

write_csv(ship_plan, file.path(outdir, "network_flow_ship_plan_weekly.csv"))
write_csv(cost_summary, file.path(outdir, "network_flow_cost_summary_weekly.csv"))

message("Wrote network flow outputs to: ", outdir)
