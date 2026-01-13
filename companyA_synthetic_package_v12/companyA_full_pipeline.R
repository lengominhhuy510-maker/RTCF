# ---- Run-from-anywhere helper ----
get_script_dir <- function(){
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
setwd(PROJECT_ROOT)

# Company A - Full Pipeline Runner (R)
# 1) EDA metrics from raw
# 2) Production LP
# 3) Network flow
# 4) Visualize (optional)

source("companyA_eda_R.R")
source("companyA_opt_production_lp.R")
source("companyA_opt_network_flow.R")
# source("companyA_visualize.R") # optional
