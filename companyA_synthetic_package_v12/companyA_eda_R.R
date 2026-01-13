# Company A - EDA in R (Lean + Supply Chain metrics from RAW events)
# No KPIs are assumed in raw data; everything is derived from timestamps/transactions/snapshots.
# Outputs CSVs into: companyA_synthetic/analysis_outputs_r/

library(tidyverse)
library(lubridate)
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

load_scenario <- function(scn){
  d <- file.path(base_dir, scn)
  list(
    prod = read_csv(file.path(d,"production_events.csv"), show_col_types = FALSE) %>%
      mutate(step_start_time = ymd_hms(step_start_time),
             step_end_time   = ymd_hms(step_end_time)),
    wo   = read_csv(file.path(d,"work_orders.csv"), show_col_types = FALSE),
    qi   = read_csv(file.path(d,"quality_inspections.csv"), show_col_types = FALSE) %>%
      mutate(inspection_time = ymd_hms(inspection_time)),
    sh   = read_csv(file.path(d,"shipments.csv"), show_col_types = FALSE) %>%
      mutate(ship_date = ymd(ship_date),
             planned_delivery_date = ymd(planned_delivery_date),
             actual_delivery_date  = ymd(actual_delivery_date)),
    dem  = read_csv(file.path(d,"demand_orders.csv"), show_col_types = FALSE) %>%
      mutate(order_date = ymd(order_date),
             due_date   = ymd(due_date)),
    inv  = read_csv(file.path(d,"inventory_snapshots.csv"), show_col_types = FALSE) %>%
      mutate(snapshot_date = ymd(snapshot_date)),
    lines= read_csv(file.path(d,"production_lines.csv"), show_col_types = FALSE),
    skus = read_csv(file.path(d,"skus.csv"), show_col_types = FALSE),
    scenario = scn
  )
}

compute_weekly_metrics <- function(x){
  prod <- x$prod %>%
    mutate(
      cycle_time_min = as.numeric(difftime(step_end_time, step_start_time, units="mins")),
      week = format(floor_date(step_start_time, unit="week", week_start=1), "%Y-%m-%d")
    )

  wait_df <- prod %>%
    arrange(work_order_id, step_start_time) %>%
    group_by(work_order_id) %>%
    mutate(prev_end = lag(step_end_time),
           waiting_min = pmax(0, as.numeric(difftime(step_start_time, prev_end, units="mins")))) %>%
    ungroup()

  qi <- x$qi %>%
    mutate(
      week = format(floor_date(inspection_time, unit="week", week_start=1), "%Y-%m-%d"),
      is_fail = if_else(result %in% c("FAIL","HOLD"), 1L, 0L)
    )

  sh <- x$sh %>%
    mutate(
      week = format(floor_date(ship_date, unit="week", week_start=1), "%Y-%m-%d"),
      lateness_days = pmax(0, as.numeric(difftime(actual_delivery_date, planned_delivery_date, units="days")))
    )

  dem <- x$dem %>%
    mutate(week = format(floor_date(order_date, unit="week", week_start=1), "%Y-%m-%d"))

  shift_hours <- c("2x8h"=16, "3x8h"=24, "1x10h"=10)
  lines_cap <- x$lines %>%
    mutate(
      daily_capacity_machine_hours = if_else(is.na(daily_capacity_machine_hours),
                                             as.numeric(shift_hours[shift_pattern])*0.85, daily_capacity_machine_hours),
      daily_capacity_labor_hours   = if_else(is.na(daily_capacity_labor_hours),
                                             as.numeric(shift_hours[shift_pattern])*1.25, daily_capacity_labor_hours),
      weekly_capacity_machine_hours = daily_capacity_machine_hours * 6,
      weekly_capacity_labor_hours   = daily_capacity_labor_hours * 6
    ) %>%
    select(production_line_id, weekly_capacity_machine_hours, weekly_capacity_labor_hours)

  util <- prod %>%
    group_by(week, production_line_id) %>%
    summarise(machine_hours = sum(machine_hours, na.rm=TRUE),
              labor_hours = sum(labor_hours, na.rm=TRUE),
              .groups="drop") %>%
    left_join(lines_cap, by="production_line_id") %>%
    mutate(utilization_machine = machine_hours / weekly_capacity_machine_hours,
           utilization_labor   = labor_hours / weekly_capacity_labor_hours)

  wk_prod <- prod %>%
    group_by(week) %>%
    summarise(
      produced_qty = sum(produced_qty, na.rm=TRUE),
      rejected_qty = sum(rejected_qty, na.rm=TRUE),
      rework_qty   = sum(rework_qty, na.rm=TRUE),
      avg_cycle_time_min = mean(cycle_time_min, na.rm=TRUE),
      p95_cycle_time_min = quantile(cycle_time_min, 0.95, na.rm=TRUE),
      .groups="drop"
    )

  wk_wait <- wait_df %>%
    group_by(week) %>%
    summarise(
      avg_waiting_min = mean(waiting_min, na.rm=TRUE),
      p95_waiting_min = quantile(waiting_min, 0.95, na.rm=TRUE),
      .groups="drop"
    )

  wk_qi <- qi %>%
    group_by(week) %>%
    summarise(
      inspections = n(),
      fail_or_hold = sum(is_fail, na.rm=TRUE),
      defects_found_qty = sum(defects_found_qty, na.rm=TRUE),
      .groups="drop"
    )

  wk_ship <- sh %>%
    group_by(week) %>%
    summarise(
      shipments = n(),
      avg_lateness_days = mean(lateness_days, na.rm=TRUE),
      p95_lateness_days = quantile(lateness_days, 0.95, na.rm=TRUE),
      .groups="drop"
    )

  wk_dem <- dem %>%
    group_by(week) %>%
    summarise(
      demand_qty = sum(demand_qty, na.rm=TRUE),
      orders = n(),
      .groups="drop"
    )

  inv <- x$inv %>%
    mutate(week = format(floor_date(snapshot_date, unit="week", week_start=1), "%Y-%m-%d"))

  inv_wk <- inv %>%
    group_by(week) %>%
    summarise(
      avg_on_hand = mean(on_hand_qty, na.rm=TRUE),
      avg_backorder = mean(backorder_qty, na.rm=TRUE),
      .groups="drop"
    )

  ship_wk_units <- sh %>%
    group_by(week) %>%
    summarise(shipped_units = sum(shipped_qty, na.rm=TRUE), .groups="drop")

  inv_wk <- inv_wk %>%
    left_join(ship_wk_units, by="week") %>%
    mutate(inventory_turnover_proxy = if_else(avg_on_hand > 0, shipped_units / avg_on_hand, NA_real_))

  metrics <- wk_prod %>%
    left_join(wk_wait, by="week") %>%
    left_join(wk_qi, by="week") %>%
    left_join(wk_ship, by="week") %>%
    left_join(wk_dem, by="week") %>%
    left_join(inv_wk, by="week") %>%
    mutate(scenario = x$scenario)

  list(metrics_weekly = metrics, line_utilization_weekly = util)
}

outdir <- file.path(base_dir, "analysis_outputs_r")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

sc_before <- load_scenario("before")
sc_after  <- load_scenario("after")

m_before <- compute_weekly_metrics(sc_before)
m_after  <- compute_weekly_metrics(sc_after)

metrics_weekly <- bind_rows(m_before$metrics_weekly, m_after$metrics_weekly) %>%
  arrange(week, scenario)

util_weekly <- bind_rows(
  m_before$line_utilization_weekly %>% mutate(scenario="before"),
  m_after$line_utilization_weekly %>% mutate(scenario="after")
) %>% arrange(week, production_line_id, scenario)

write_csv(metrics_weekly, file.path(outdir, "metrics_weekly_r.csv"))
write_csv(util_weekly, file.path(outdir, "line_utilization_weekly_r.csv"))

bottlenecks <- util_weekly %>%
  filter(scenario=="before") %>%
  group_by(production_line_id) %>%
  summarise(median_util = median(utilization_machine, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(median_util))
write_csv(bottlenecks, file.path(outdir, "bottleneck_lines_r.csv"))

spike <- sc_before$prod %>%
  mutate(date = as_date(step_start_time)) %>%
  filter(date >= ymd("2025-04-10"), date <= ymd("2025-04-20")) %>%
  group_by(sku_id, operation_step) %>%
  summarise(
    produced = sum(produced_qty, na.rm=TRUE),
    rejected = sum(rejected_qty, na.rm=TRUE),
    rework   = sum(rework_qty, na.rm=TRUE),
    .groups="drop"
  )
write_csv(spike, file.path(outdir, "defect_spike_summary_r.csv"))

message("Wrote R EDA outputs to: ", outdir)
