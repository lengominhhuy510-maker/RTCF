make_base_decisions <- function(){
  list(
    sales = list(
      customer_product_level = sales_decision_cp,
      shortage_rule = "proportional"  # default game mode
    ),
    purchasing = list(
      supplier_params = supplier_params_default
    ),
    supply_chain = list(
      rm_safety_stock_w = decisions_round$supply_chain$rm_safety_stock_w,
      rm_lot_size_w     = decisions_round$supply_chain$rm_lot_size_w,
      fg_safety_stock_w = decisions_round$supply_chain$fg_safety_stock_w
    ),
    operations = list(
      num_shifts = 1  # default Swiss Fill 2
    )
  )
}

# -------------------------
# 2) Manual update helpers
# -------------------------

# 2A) Update sales decisions by customer/sku (mass edit)
# Example:
#   dec <- set_sales_decisions(dec,
#             customer = c("Food & Groceries","LAND Market"),
#             sku = NULL,
#             promotional_pressure = "heavy",
#             service_level = 98)

set_sales_decisions <- function(decisions,
                                customer = NULL,
                                sku = NULL,
                                promotional_pressure = NULL,
                                order_deadline = NULL,
                                service_level = NULL,
                                trade_unit = NULL,
                                payment_term = NULL,
                                promotion_horizon = NULL,
                                shelf_life = NULL){
  
  tbl <- decisions$sales$customer_product_level
  
  if (!is.null(customer)) tbl <- tbl %>% filter(customer %in% customer)
  if (!is.null(sku))      tbl <- tbl %>% filter(sku %in% sku)
  
  idx <- paste(tbl$customer, tbl$sku)
  full_tbl <- decisions$sales$customer_product_level
  full_idx <- paste(full_tbl$customer, full_tbl$sku)
  mask <- full_idx %in% idx
  
  # Apply updates only to masked rows
  if (!is.null(promotional_pressure))
    full_tbl$promotional_pressure[mask] <- tolower(promotional_pressure)
  if (!is.null(order_deadline))
    full_tbl$order_deadline[mask] <- str_replace_all(order_deadline, "\\s+","")
  if (!is.null(service_level))
    full_tbl$service_level[mask] <- as.numeric(service_level)
  if (!is.null(trade_unit))
    full_tbl$trade_unit[mask] <- tolower(trade_unit)
  if (!is.null(payment_term))
    full_tbl$payment_term[mask] <- as.character(payment_term)
  if (!is.null(promotion_horizon))
    full_tbl$promotion_horizon[mask] <- tolower(promotion_horizon)
  if (!is.null(shelf_life))
    full_tbl$shelf_life[mask] <- as.character(shelf_life)
  
  # Re-join CI promised after edits (keep game-consistent)
  sales_ci_key <- c("customer","promotional_pressure","order_deadline","service_level",
                    "trade_unit","payment_term","promotion_horizon","shelf_life")
  
  full_tbl <- full_tbl %>%
    select(-ci_promised) %>%
    left_join(CIsale_lookup, by = sales_ci_key) %>%
    mutate(ci_promised = replace_na(ci_promised, 1))
  
  decisions$sales$customer_product_level <- full_tbl
  decisions
}

# 2B) Set shortage rule quickly
set_shortage_rule <- function(decisions, rule=c("proportional","fcfs","priority")){
  decisions$sales$shortage_rule <- match.arg(rule)
  decisions
}

# 2C) Update RM policy quickly
# rm_ss, rm_lot should be named vectors already normalized by norm_component()
set_rm_policy <- function(decisions, rm_ss=NULL, rm_lot=NULL){
  if (!is.null(rm_ss))  decisions$supply_chain$rm_safety_stock_w <- rm_ss
  if (!is.null(rm_lot)) decisions$supply_chain$rm_lot_size_w     <- rm_lot
  decisions
}

# 2D) Update supplier params (CI purchasing knobs)
# Example:
# dec <- set_supplier_ci(dec, component="mango", ci_purch=0.95)
set_supplier_ci <- function(decisions, component=NULL, vendor=NULL,
                            quality=NULL, delivery_window=NULL,
                            delivery_reliability=NULL, trade_unit=NULL,
                            payment_term=NULL, ci_purch=NULL){
  
  tbl <- decisions$purchasing$supplier_params
  
  if (!is.null(component)) tbl <- tbl %>% filter(component %in% norm_component(component))
  if (!is.null(vendor))    tbl <- tbl %>% filter(vendor %in% vendor)
  
  idx <- tbl$component
  full_tbl <- decisions$purchasing$supplier_params
  mask <- full_tbl$component %in% idx
  
  if (!is.null(quality)) full_tbl$quality[mask] <- tolower(quality)
  if (!is.null(delivery_window)) full_tbl$delivery_window[mask] <- tolower(delivery_window)
  if (!is.null(delivery_reliability)) full_tbl$delivery_reliability[mask] <- as.numeric(delivery_reliability)
  if (!is.null(trade_unit)) full_tbl$trade_unit[mask] <- tolower(trade_unit)
  if (!is.null(payment_term)) full_tbl$payment_term[mask] <- as.character(payment_term)
  
  # if ci_purch is given, override directly
  if (!is.null(ci_purch)) full_tbl$ci_purch[mask] <- as.numeric(ci_purch)
  
  # otherwise re-lookup CI from table (optional)
  # (only do if you changed fields but didn't set ci_purch)
  if (is.null(ci_purch)){
    pur_ci_key <- c("vendor","quality","delivery_window","delivery_reliability",
                    "trade_unit","payment_term")
    
    full_tbl <- full_tbl %>%
      select(-ci_purch) %>%
      left_join(CIpur_lookup, by=pur_ci_key) %>%
      mutate(ci_purch = replace_na(ci_purch, 1))
  }
  
  decisions$purchasing$supplier_params <- full_tbl
  decisions
}

# 2E) Set operations knobs
set_operations <- function(decisions, num_shifts=NULL){
  if (!is.null(num_shifts)) decisions$operations$num_shifts <- num_shifts
  decisions
}

# -------------------------
# 3) Run a manual scenario quickly
# -------------------------
run_scenario <- function(decisions, n_weeks=26, label="manual_test"){
  
  out <- engine_round(
    state0 = state0_round,
    decisions = decisions,
    constants = constants,
    lookups = lookups,
    exogenous = exo,
    n_weeks = n_weeks
  )
  
  cat("\n=========================\n")
  cat("SCENARIO:", label, "\n")
  cat("=========================\n")
  print(out$finance_round)
  
  # quick KPIs
  kpi_sales <- out$flows$sales %>%
    summarise(
      total_demand = sum(demand_units),
      total_delivered = sum(delivered_units),
      fill_rate = total_delivered/total_demand
    )
  
  kpi_prod <- out$flows$production %>%
    summarise(
      total_liters = sum(liters_required),
      total_produced = sum(produced_units)
    )
  
  cat("\n-- Sales KPI --\n"); print(kpi_sales)
  cat("\n-- Production KPI --\n"); print(kpi_prod)
  
  invisible(out)
}

# -------------------------
# EXAMPLE USAGE
# -------------------------

# 0) init base
dec <- make_base_decisions()

# 1) thử đổi promo cho tất cả customer của 1 SKU
# dec <- set_sales_decisions(dec, sku="Orange 1L", promotional_pressure="heavy")

# 2) đổi shortage rule
# dec <- set_shortage_rule(dec, "priority")

# 3) đổi RM safety stock / lot size
# new_ss <- decisions_round$supply_chain$rm_safety_stock_w
# new_ss["mango"] <- 4
# dec <- set_rm_policy(dec, rm_ss=new_ss)

# 4) đổi CI mua hàng cho supplier Mango
# dec <- set_supplier_ci(dec, component="mango", ci_purch=0.9)

# 5) đổi số shifts
# dec <- set_operations(dec, num_shifts=2)

# 6) run
# out_test <- run_scenario(dec, label="test_heavy_promo_priority")