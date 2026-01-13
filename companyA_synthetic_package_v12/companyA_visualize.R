
# Company A - Before vs After Lean Visualization
# Style inspired by highcharter patterns (see group13 project2.R)

library(tidyverse)
library(highcharter)

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

base_dir <- "companyA_synthetic/analysis_outputs"
metrics <- read_csv(file.path(base_dir, "metrics_weekly.csv"), show_col_types = FALSE)
util <- read_csv(file.path(base_dir, "line_utilization_weekly.csv"), show_col_types = FALSE)

# Ensure order
metrics <- metrics %>% arrange(week, scenario)

# 1) Throughput vs Defects (bar + line dual axis)
colors <- c("before"="#DB4437", "after"="#38A86F")

weekly_throughput <- metrics %>%
  mutate(good_qty = produced_qty - rejected_qty) %>%
  select(week, scenario, good_qty, rejected_qty, avg_waiting_min)

# Column chart for good_qty (stacked by scenario) + line for rejected
weekly_wide <- weekly_throughput %>%
  select(week, scenario, good_qty) %>%
  pivot_wider(names_from = scenario, values_from = good_qty)

highchart() %>%
  hc_title(text = "Weekly Good Output (Before vs After)") %>%
  hc_subtitle(text = "Company A (synthetic raw events)") %>%
  hc_xAxis(categories = weekly_wide$week, title=list(text="Week")) %>%
  hc_add_series(name="Before (Good Qty)", type="column", data=weekly_wide$before, color=colors["before"]) %>%
  hc_add_series(name="After (Good Qty)", type="column", data=weekly_wide$after,  color=colors["after"]) %>%
  hc_plotOptions(column=list(grouping=TRUE)) %>%
  hc_yAxis(title=list(text="Units")) %>%
  hc_tooltip(shared=TRUE, crosshairs=TRUE) %>%
  hc_exporting(enabled = TRUE)

# 2) Waiting time reduction (line)
wait_df <- metrics %>% select(week, scenario, avg_waiting_min)

wait_wide <- wait_df %>% pivot_wider(names_from=scenario, values_from=avg_waiting_min)

highchart() %>%
  hc_title(text="Average Waiting Time Between Steps") %>%
  hc_xAxis(categories = wait_wide$week, title=list(text="Week")) %>%
  hc_add_series(name="Before", type="line", data=wait_wide$before, color=colors["before"]) %>%
  hc_add_series(name="After", type="line", data=wait_wide$after,  color=colors["after"]) %>%
  hc_yAxis(title=list(text="Minutes")) %>%
  hc_tooltip(shared=TRUE, crosshairs=TRUE) %>%
  hc_exporting(enabled = TRUE)

# 3) Boxplot-ish view using quantiles of cycle time (p95 vs avg) (line)
ct <- metrics %>% select(week, scenario, avg_cycle_time_min, p95_cycle_time_min)

ct_wide_avg <- ct %>% select(week, scenario, avg_cycle_time_min) %>% pivot_wider(names_from=scenario, values_from=avg_cycle_time_min)
ct_wide_p95 <- ct %>% select(week, scenario, p95_cycle_time_min) %>% pivot_wider(names_from=scenario, values_from=p95_cycle_time_min)

highchart() %>%
  hc_title(text="Cycle Time (Avg vs P95)") %>%
  hc_xAxis(categories = ct_wide_avg$week, title=list(text="Week")) %>%
  hc_add_series(name="Before Avg", type="line", data=ct_wide_avg$before, color=colors["before"]) %>%
  hc_add_series(name="After Avg", type="line", data=ct_wide_avg$after,  color=colors["after"]) %>%
  hc_add_series(name="Before P95", type="line", data=ct_wide_p95$before, dashStyle="Dash", color=colors["before"]) %>%
  hc_add_series(name="After P95", type="line", data=ct_wide_p95$after,  dashStyle="Dash", color=colors["after"]) %>%
  hc_yAxis(title=list(text="Minutes")) %>%
  hc_tooltip(shared=TRUE, crosshairs=TRUE) %>%
  hc_exporting(enabled = TRUE)

# 4) Heatmap: line utilization (to spot bottleneck)
util2 <- util %>%
  mutate(util_pct = pmin(1.5, utilization_machine)) %>%
  mutate(line = production_line_id)

# heatmap requires numeric x/y
util_mat <- util2 %>%
  select(week, line, util_pct) %>%
  mutate(week_i = as.integer(factor(week)),
         line_i = as.integer(factor(line))) %>%
  distinct()

highchart() %>%
  hc_chart(type = "heatmap") %>%
  hc_title(text = "Line Utilization Heatmap (Machine Hours / Capacity)") %>%
  hc_xAxis(categories = sort(unique(util2$week)), title=list(text="Week")) %>%
  hc_yAxis(categories = sort(unique(util2$line)), title=list(text="Line"), reversed = TRUE) %>%
  hc_add_series(
    name = "Utilization",
    data = purrr::pmap(util_mat, function(week, line, util_pct, week_i, line_i) {
      list(x=week_i-1, y=line_i-1, value=util_pct)
    }),
    borderWidth = 0
  ) %>%
  hc_colorAxis(min=0, max=1.2) %>%
  hc_tooltip(pointFormat = "Utilization: {point.value:.2f}") %>%
  hc_exporting(enabled = TRUE)
