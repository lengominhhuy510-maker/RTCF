str(CIsale_lookup)
str(CIpur_lookup)
library(writexl)
library(tibble)
library(openxlsx)
library(readxl)
library(highcharter)
library(dplyr)
library(data.table)
library(DT)
library(tidyverse)
library(purrr)
library(tibble)
library(janitor)
# =========================
# 0) LOAD DATA
# =========================
df_component  <- read_excel("df_component.xlsx")  %>% clean_names()
sales_area_cp <- read_excel("sale_area_cp.xlsx") %>% clean_names()
df_sku        <- read_excel("df_sku.xlsx")        %>% clean_names()
CIpur_lookup  <- read_excel("CIpurlookup.xlsx")
CIsale_lookup <- read_excel("CIsalelookup.xlsx")

# =========================
# 1) NORMALIZE LOOKUPS
# =========================

# ---- (1A) Sales CI lookup normalize ----
CIsale_lookup <- CIsale_lookup %>%
  clean_names() %>% 
  rename(
    promotional_pressure = promotional_pressure,
    order_deadline       = oder_deadline,
    service_level        = service_level,
    trade_unit           = trade_unit,
    payment_term         = payment_term,
    promotion_horizon    = promotion_horizon,
    shelf_life           = shelflife,
    ci_promised          = contract_index,
    customer_lookup      = customer
  ) %>%
  mutate(
    customer = recode(customer_lookup,
                      "Groceries" = "Food & Groceries",
                      "Landmark"  = "LAND Market",
                      "Dominick"  = "Dominick's",
                      .default = customer_lookup),
    
    promotional_pressure = str_to_lower(promotional_pressure),
    trade_unit           = str_to_lower(trade_unit),
    promotion_horizon    = str_to_lower(promotion_horizon),
    
    # FIX typo "middel" -> "middle"
    promotion_horizon = recode(promotion_horizon, "middel"="middle", .default=promotion_horizon),
    
    # order_deadline normalize: "17pm" etc
    order_deadline = str_replace_all(order_deadline, "\\s+",""),
    
    # payment_term force character để join
    payment_term = as.character(payment_term)
  ) %>%
  select(customer, promotional_pressure, order_deadline, service_level,
         trade_unit, payment_term, promotion_horizon, shelf_life, ci_promised)

# check
# unique(CIsale_lookup$customer)

# ---- (1B) Purchasing CI lookup normalize ----
CIpur_lookup <- CIpur_lookup %>%
  janitor::clean_names() %>%
  rename(
    quality              = quality,
    delivery_window      = delivery_window,
    delivery_reliability = delivery_reliability,
    trade_unit           = trade_unit,
    payment_term         = payment_term,
    vendor_lookup        = vendor,
    ci_purch             = contract_index
  ) %>%
  mutate(
    # chuẩn hoá chữ thường cho text fields
    quality = str_to_lower(quality),
    delivery_window = str_to_lower(delivery_window),
    trade_unit = str_to_lower(trade_unit),
    payment_term = as.character(payment_term),
    
    # reliability phải numeric
    delivery_reliability = as.numeric(delivery_reliability),
    
    # map vendor name cho khớp supplier_master / decisions
    vendor = recode(vendor_lookup,
                    "MonoPackaging" = "Mono Packaging Materials",
                    "TrioPet"       = "Trio PET PLC",
                    "MiamiOrange"   = "Miami Oranges",
                    "NO8DOMango"    = "NO8DO Mango",
                    "SeitanVitamin" = "Seitan Vitamins",
                    .default = vendor_lookup
    )
  ) %>%
  select(vendor, quality, delivery_window, delivery_reliability,
         trade_unit, payment_term, ci_purch)

# =========================
# 2) CONSTANTS (giữ nguyên của bạn)
# =========================

operations_constants <- list(
  mixer = list(
    technical_min_liters = 8000,
    max_capacity_liters  = 12000,
    operating_time_hours = 2.0,
    cleaning_time_hours  = 2.0,
    fixed_annual_cost    = 62500.00,
    variable_cost_per_hour = 135.00,
    investment_cost        = 312500.00
  ),
  bottling_line = list(
    hours_per_shift = 40,
    shift_hours = c(`1`=40,`2`=80,`3`=120,`4`=144,`5`=168),
    operator_cost_per_year = 40000,
    operator_hours_per_week = 40,
    formula_changeover_hours = NULL,
    size_changeover_hours    = NULL,
    startup_loss_hours = 1,
    base_breakdown_rate = NULL,
    breakdown_hours_loss = NULL,
    packaging_quality_sensitivity = NULL
  ),
  labor_production=list(
    fte_cost_per_year  = 40000,
    flex_cost_per_hour = 42,
    fte_hours_per_week = 40,
    max_internal_hours = 168,
    outsourcing_factor = 2
  ),
  fg_warehouse=list(
    pallet_location_cost_year=200,
    pallet_overflow_cost_per_day=3,
    pallet_overflow_cost_per_week=15,
    fte_cost_per_year=40000, fte_hours_per_week=40, flex_cost_per_hour=42,
    store_pallet_minutes=6, pick_orderline_minutes=10, pick_pallet_minutes=6,
    pick_outerbox_minutes=3, move_overflow_minutes=6, obsolete_pallet_minutes=6,
    housekeeping_hours_per_day=4, housekeeping_hours_per_week=20
  ),
  rm_warehouse=list(
    pallet_location_cost_year=200,
    pallet_overflow_cost_day=3,
    pallet_overflow_cost_week=15,
    intake_hours_per_orderline=1,
    intake_minutes_per_pallet=6,
    make_available_minutes_pallet=6,
    make_available_minutes_tank=12,
    daily_housekeeping_hours=4,
    weekly_housekeeping_hours=20,
    overflow_handling_minutes_per_pallet=6,
    ibc_filling_hours=1,
    fte_cost_per_year=40000, fte_hours_per_week=40, flex_cost_per_hour=42
  ),
  tank_yard=list(
    tank_use_cost_day=25,
    tank_use_cost_week=125,
    intake_handling_per_delivery=10,
    delivery_to_TFC=100
  ),
  raw_material_inspection=list(
    add_hours_per_orderline=2,
    reduce_breakdowns_factor=0.6
  ),
  time=list(week_days=5, hours_per_day=8)
)

supplier_master <- tribble(
  ~supplier, ~product_range, ~country, ~payment_term,
  ~lead_time_days, ~mode, ~shipment_cost,
  ~transport_cost_per_pallet, ~transport_cost_per_ftl,
  ~free_capacity_excl_tfc, ~free_capacity_incl_tfc,
  ~certified,
  "Mono Packaging Materials","Pack","France","At delivery",15,NA,100,20,500,26,18,FALSE,
  "Trio PET PLC","PET","Spain","At delivery",10,NA,150,30,750,10,3,FALSE,
  "Miami Oranges","Orange","United States","When ordering",30,NA,125,40,1000,40,38,TRUE,
  "NO8DO Mango","Mango","Spain","When ordering",10,"Truck",100,30,750,9,4,TRUE,
  "NO8DO Mango","Mango","Spain","When ordering",25,"Boat",100,20,500,9,4,TRUE,
  "Seitan Vitamins","Vitamin C","China","When ordering",60,NA,100,50,1250,52,50,FALSE
)

purchasing_constants <- list(
  dual_source_cost_per_supplier=40000,
  emergency_order_weeks=1,
  interest_rate=0.15,
  admin=list(inbound_order=50,inbound_orderline=10,supplier_maintenance=40000),
  supplier_master=supplier_master
)

sc_constants <- list(
  time=list(working_days_per_week=5),
  logistics=list(drum_liters=250, ibc_liters=1000, tanktruck_liters=30000, pallets_per_FTL=30),
  rm_stock_policy=list(safety_stock_weeks=NULL, lot_size_weeks=NULL),
  fg_stock_policy=list(safety_stock_weeks=NULL, production_interval_days=NULL),
  planning=list(frozen_period_weeks=NULL)
)

customer_master <- tibble(
  customer=c("Food & Groceries","LAND Market","Dominick's"),
  value_for_money=c(TRUE,TRUE,FALSE),
  market_share=c(0.10,0.08,0.02),
  pref_small_pack=c(FALSE,FALSE,TRUE),
  maturity=c("high","low","medium")
)

sales_area_cp <- sales_area_cp %>%
  mutate(sales_price = as.numeric(str_remove_all(sales_price, "[€ ]")))

sales_area_cp <- sales_area_cp %>%
  rename(
    customer=customer_sales_area,
    sku=product,
    demand_week_pieces=demand_per_week_pieces,
    promo_additional_pct=additional_sales_as_a_result_of_promotions,
    sl_pieces=service_level_pieces,
    sl_orderlines=service_level_order_lines,
    demand_value_per_w=demand_value_per_week,
    gross_margin_per_w=gross_margin_per_week_6
  )

benchmark_demand <- sales_area_cp %>% select(customer, sku, demand_week_pieces)

sales_constants <- list(
  sla_rules=list(
    shelf_life_min_pct=0.40,
    shelf_life_max_pct=0.85,
    service_level_types=c("units","order_lines","orders"),
    trade_unit_types=c("outer_box","pallet_layer","pallet"),
    promo_pressure_levels=c("none","light","middle","heavy")
  ),
  promotion_uplift=list(
    basic=tribble(
      ~pressure,~uplift_min,~uplift_max,
      "none",0,0,
      "light",0.005,0.010,
      "middle",0.015,0.020,
      "heavy",0.040,0.055
    ),
    value_for_money_multiplier=2,
    slowmover_multiplier=2,
    uplift_method="midpoint"
  ),
  market_forces=list(volume_factor_min=0.90, volume_factor_max=1.10),
  shortage_rules=list(methods=c("proportional","fcfs","priority")),
  customer_master=customer_master,
  observed=list(benchmark_demand=benchmark_demand),
  decisions=list(customer_level=NULL, customer_product_level=NULL)
)

finance_constants <- list(
  distribution=list(
    base_cost_per_pallet=tribble(~dc,~base_cost,"DC Netherlands",31),
    provider_tiers=tribble(
      ~tier,~pallet_min,~pallet_max,~rate_per_pallet,
      "small",0,9,65,
      "medium",10,20,60,
      "large",21,Inf,55
    )
  ),
  investment=list(building_fixed=2500000)
)

# =========================
# 3) HELPERS
# =========================

calc_distribution_cost <- function(pallets_shipped, dc="DC Netherlands", finance_constants){
  base <- finance_constants$distribution$base_cost_per_pallet %>%
    filter(dc==!!dc) %>% pull(base_cost)
  tier <- finance_constants$distribution$provider_tiers %>%
    filter(pallets_shipped>=pallet_min, pallets_shipped<=pallet_max) %>% slice(1)
  cost_per_pallet <- base + tier$rate_per_pallet
  pallets_shipped * cost_per_pallet
}

make_empty_flows <- function(){
  list(
    sales=tibble(), production=tibble(), purchasing=tibble(),
    inventory=tibble(), warehousing=tibble(), distribution=tibble()
  )
}

# promo uplift midpoint
get_promo_uplift <- function(pressure){
  tab <- sales_constants$promotion_uplift$basic
  row <- tab %>% filter(pressure==!!pressure)
  (row$uplift_min + row$uplift_max)/2
}

# =========================
# 4) DECISIONS: BUILD SALES & PURCH PARAMS DEFAULT
# =========================

# ---- sales decision default (không cần sku để lookup CI) ----
make_sales_decisions <- function(customers, skus){
  expand_grid(customer=customers, sku=skus) %>%
    mutate(
      promotional_pressure="middle",
      order_deadline="14pm",
      service_level=95,              # %
      trade_unit="box",
      payment_term="4",                # weeks
      promotion_horizon="short",
      shelf_life="40"                  # %
    )
}

sales_decision_cp <- make_sales_decisions(
  customers = unique(customer_master$customer),
  skus      = unique(df_sku$sku)
)%>%
  mutate(
    promotional_pressure = str_to_lower(promotional_pressure),
    trade_unit           = str_to_lower(trade_unit),
    promotion_horizon    = str_to_lower(promotion_horizon),
    order_deadline       = str_replace_all(order_deadline, "\\s+",""),
    payment_term         = as.character(payment_term)
  )

# lookup CI theo đúng key của lookup (không join sku!)
sales_ci_key <- c("customer","promotional_pressure","order_deadline","service_level",
                  "trade_unit","payment_term","promotion_horizon","shelf_life")

sales_decision_cp <- sales_decision_cp %>%
  left_join(CIsale_lookup, by=sales_ci_key) %>%
  mutate(ci_promised=ifelse(is.na(ci_promised),1,ci_promised))

# ---- purchasing supplier_params default ----
# Phase 1: nếu bạn chưa fill chất lượng / reliability, pick 1 baseline.
# Bạn có thể thay bằng decision thật sau.
supplier_params_default <- tibble(
  component = c("Pack","PET","Orange","Mango","Vitamin C"),
  vendor    = c("Mono Packaging Materials","Trio PET PLC","Miami Oranges","NO8DO Mango","Seitan Vitamins"),
  quality   = "standard",
  delivery_window = "standard",
  delivery_reliability = "standard",
  trade_unit = "pallet",
  payment_term = "at delivery"
)

# lookup CI purchasing
pur_ci_key <- c("quality","delivery_window","delivery_reliability","trade_unit",
                "payment_term","vendor")

supplier_params_default <- supplier_params_default %>%
  left_join(CIpur_lookup, by=pur_ci_key) %>%
  mutate(ci_purch=ifelse(is.na(ci_purch),1,ci_purch))

# =========================
# 5) PHASE 1 FUNCTIONS
# =========================

sales_demand_week <- function(week, state, sales_decision_cp, sales_constants, exo){
  bench <- sales_constants$observed$benchmark_demand
  
  vf <- exo$volume_factor
  if (is.null(vf)) vf <- tibble(customer=unique(bench$customer), volume_factor=1)
  
  dem <- bench %>%
    left_join(vf, by="customer") %>%
    left_join(sales_decision_cp %>% select(customer, sku, promotional_pressure, ci_promised),
              by=c("customer","sku")) %>%
    mutate(
      promotional_pressure = replace_na(promotional_pressure,"none"),
      promo_uplift = map_dbl(promotional_pressure, get_promo_uplift),
      demand_units = demand_week_pieces * volume_factor * (1 + promo_uplift),
      ci_promised  = replace_na(ci_promised,1)
    ) %>%
    select(customer, sku, demand_units, ci_promised, promotional_pressure, volume_factor)
  
  dem
}

planning_week <- function(week, state, demand_w, decisions_sc){
  fg_stock <- state$fg_stock %>% select(sku, fg_units=units)
  
  ssfg_w <- decisions_sc$fg_safety_stock_w
  if (is.null(ssfg_w)) ssfg_w <- setNames(rep(0,length(unique(demand_w$sku))), unique(demand_w$sku))
  
  demand_sku <- demand_w %>% group_by(sku) %>% summarise(demand_units=sum(demand_units),.groups="drop")
  
  plan <- demand_sku %>%
    left_join(fg_stock, by="sku") %>%
    mutate(
      fg_units=replace_na(fg_units,0),
      target_ss_units = ssfg_w[sku] * demand_units,
      planned_units = pmax(0, demand_units + target_ss_units - fg_units)
    ) %>% select(sku, demand_units, planned_units)
  
  plan
}

rm_replenishment_week <- function(week, state, plan_w, decisions_sc,
                                  supplier_params, df_sku, df_component){
  bom_long <- df_sku %>%
    select(sku, starts_with("rm_")) %>%
    pivot_longer(starts_with("rm_"),
                 names_to="component_raw", values_to="qty_per_unit") %>%
    mutate(component = str_replace(component_raw,"rm_","")) %>%
    select(sku, component, qty_per_unit)
  
  rm_need <- plan_w %>%
    left_join(bom_long, by="sku") %>%
    mutate(rm_week_need = planned_units * qty_per_unit) %>%
    group_by(component) %>%
    summarise(rm_week_need=sum(rm_week_need,na.rm=TRUE),.groups="drop")
  
  rm_stock <- state$rm_stock %>% select(component, rm_units=units)
  
  ssrm_w <- decisions_sc$rm_safety_stock_w
  lot_w  <- decisions_sc$rm_lot_size_w
  
  if (is.null(ssrm_w)) ssrm_w <- setNames(rep(0,nrow(rm_need)), rm_need$component)
  if (is.null(lot_w))  lot_w  <- setNames(rep(1,nrow(rm_need)), rm_need$component)
  
  reorder_tbl <- rm_need %>%
    left_join(rm_stock, by="component") %>%
    mutate(
      rm_units=replace_na(rm_units,0),
      target_ss_units = ssrm_w[component] * rm_week_need,
      lot_units = lot_w[component] * rm_week_need,
      order_units = ifelse(rm_units < target_ss_units, lot_units, 0)
    ) %>%
    left_join(df_component %>% select(component, basic_price), by="component") %>%
    left_join(supplier_params %>% select(component, ci_purch), by="component") %>%
    mutate(
      ci_purch=replace_na(ci_purch,1),
      purchase_price = basic_price * ci_purch,
      purchase_cost_total = order_units * purchase_price
    )
  
  orders <- reorder_tbl %>%
    filter(order_units>0) %>%
    transmute(week, component, order_qty_units=order_units,
              purchase_price, purchase_cost_total)
  
  receipts <- orders %>% transmute(week, component, received_qty_units=order_qty_units)
  
  list(orders=orders, receipts=receipts)
}

execute_production_week <- function(week, state, plan_w, rm_receipts_w, df_sku){
  produced <- plan_w %>%
    mutate(produced_units=planned_units) %>%
    transmute(week, sku, planned_units, produced_units, production_cost=0)
  
  bom_long <- df_sku %>%
    select(sku, starts_with("rm_")) %>%
    pivot_longer(starts_with("rm_"),
                 names_to="component_raw", values_to="qty_per_unit") %>%
    mutate(component=str_replace(component_raw,"rm_","")) %>%
    select(sku, component, qty_per_unit)
  
  rm_used <- produced %>%
    left_join(bom_long, by="sku") %>%
    mutate(used_units = produced_units * qty_per_unit) %>%
    group_by(component) %>%
    summarise(used_units=sum(used_units,na.rm=TRUE),.groups="drop") %>%
    mutate(week=week)
  
  list(production=produced, rm_used=rm_used)
}

fulfill_demand_week <- function(week, state, demand_w, produced_w, df_sku){
  fg_stock <- state$fg_stock %>% select(sku, fg_units=units)
  avail <- fg_stock %>%
    full_join(produced_w %>% select(sku, produced_units), by="sku") %>%
    mutate(
      fg_units=replace_na(fg_units,0),
      produced_units=replace_na(produced_units,0),
      available_units=fg_units+produced_units
    )
  
  sales_exec <- demand_w %>%
    left_join(avail, by="sku") %>%
    mutate(
      delivered_units=pmin(demand_units,available_units),
      backorder_units=pmax(0,demand_units-available_units),
      attained_ci=ci_promised,
      penalty_factor=1
    ) %>%
    left_join(df_sku %>% select(sku, basic_sales_price, units_per_pallet), by="sku") %>%
    mutate(
      sales_price = basic_sales_price * attained_ci,
      revenue = delivered_units * sales_price,
      pallets_shipped = delivered_units / units_per_pallet
    ) %>%
    select(week, customer, sku, demand_units, delivered_units,
           backorder_units, sales_price, revenue, pallets_shipped)
  
  sales_exec
}

update_state_week <- function(state, receipts_w, rm_used_w, produced_w, sales_exec){
  # RM
  rm_stock <- state$rm_stock %>%
    full_join(receipts_w %>% group_by(component) %>% summarise(received=sum(received_qty_units),.groups="drop"),
              by="component") %>%
    full_join(rm_used_w %>% group_by(component) %>% summarise(used=sum(used_units),.groups="drop"),
              by="component") %>%
    mutate(
      units=replace_na(units,0),
      received=replace_na(received,0),
      used=replace_na(used,0),
      units=pmax(0,units+received-used)
    ) %>% select(component, units)
  
  # FG
  fg_stock <- state$fg_stock %>%
    full_join(produced_w %>% group_by(sku) %>% summarise(produced=sum(produced_units),.groups="drop"),
              by="sku") %>%
    full_join(sales_exec %>% group_by(sku) %>% summarise(shipped=sum(delivered_units),.groups="drop"),
              by="sku") %>%
    mutate(
      units=replace_na(units,0),
      produced=replace_na(produced,0),
      shipped=replace_na(shipped,0),
      units=pmax(0,units+produced-shipped),
      age_w=replace_na(age_w,0)+1
    ) %>% select(sku, units, age_w)
  
  state$rm_stock <- rm_stock
  state$fg_stock <- fg_stock
  state
}

finance_aggregate_round <- function(flows, finance_constants){
  revenue <- sum(flows$sales$revenue, na.rm=TRUE)
  purchase_costs <- sum(flows$purchasing$purchase_cost_total, na.rm=TRUE)
  cogs <- purchase_costs
  gross_margin <- revenue - cogs
  
  dist_costs <- sum(map_dbl(flows$sales$pallets_shipped,
                            ~calc_distribution_cost(.x, finance_constants=finance_constants)),
                    na.rm=TRUE)
  
  operating_profit <- gross_margin - dist_costs
  investment_total <- finance_constants$investment$building_fixed
  ROI_pred <- operating_profit / investment_total
  
  list(ROI_pred=ROI_pred,
       revenue=revenue, gross_margin=gross_margin,
       operating_profit=operating_profit, distribution_costs=dist_costs)
}

# =========================
# 6) ENGINE ROUND (PHASE 1)
# =========================

engine_round <- function(state0, decisions, constants, lookups, exogenous, n_weeks=26){
  
  flows <- make_empty_flows()
  state <- state0
  
  for (w in 1:n_weeks){
    
    demand_w <- sales_demand_week(
      week=w, state=state,
      sales_decision_cp = decisions$sales$customer_product_level,
      sales_constants = constants$sales,
      exo = exogenous
    )
    
    plan_w <- planning_week(
      week=w, state=state, demand_w=demand_w,
      decisions_sc = decisions$supply_chain
    )
    
    rm_rep <- rm_replenishment_week(
      week=w, state=state, plan_w=plan_w,
      decisions_sc = decisions$supply_chain,
      supplier_params = decisions$purchasing$supplier_params,
      df_sku = lookups$df_sku,
      df_component = lookups$df_component
    )
    
    prod_rep <- execute_production_week(
      week=w, state=state, plan_w=plan_w,
      rm_receipts_w = rm_rep$receipts,
      df_sku = lookups$df_sku
    )
    
    sales_exec <- fulfill_demand_week(
      week=w, state=state, demand_w=demand_w,
      produced_w = prod_rep$production,
      df_sku = lookups$df_sku
    )
    
    state <- update_state_week(
      state, receipts_w=rm_rep$receipts,
      rm_used_w=prod_rep$rm_used,
      produced_w=prod_rep$production,
      sales_exec=sales_exec
    )
    
    # append flows
    flows$sales       <- bind_rows(flows$sales, sales_exec)
    flows$production  <- bind_rows(flows$production, prod_rep$production)
    flows$purchasing  <- bind_rows(flows$purchasing, rm_rep$orders)
  }
  
  finance_round <- finance_aggregate_round(flows, constants$finance)
  
  list(flows=flows, finance_round=finance_round, state_end=state)
}

# =========================
# 7) STATE0 + DECISIONS ROUND (PHASE 1 DEFAULT)
# =========================

state0_round <- list(
  rm_stock = tibble(component = unique(df_component$component), units=0),
  fg_stock = tibble(sku = unique(df_sku$sku), units=0, age_w=0)
)

decisions_round <- list(
  sales = list(
    customer_product_level = sales_decision_cp
  ),
  purchasing = list(
    supplier_params = supplier_params_default
  ),
  supply_chain = list(
    rm_safety_stock_w = c(Pack=2, PET=3, Orange=2, Mango=2, `Vitamin C`=2.5),
    rm_lot_size_w     = c(Pack=4, PET=4, Orange=4, Mango=4, `Vitamin C`=4),
    fg_safety_stock_w = setNames(rep(3,length(unique(df_sku$sku))), unique(df_sku$sku))
  ),
  operations = list()
)

lookups <- list(df_sku=df_sku, df_component=df_component)

constants <- list(
  sales=sales_constants,
  purchasing=purchasing_constants,
  supply_chain=sc_constants,
  operations=operations_constants,
  finance=finance_constants
)

exo <- list(
  volume_factor = tibble(customer=unique(customer_master$customer), volume_factor=1)
)

# =========================
# 8) RUN PHASE 1
# =========================
out <- engine_round(
  state0 = state0_round,
  decisions = decisions_round,
  constants = constants,
  lookups = lookups,
  exogenous = exo,
  n_weeks = 26
)

out$finance_round$ROI_pred

unique(CIsale_lookup$promotional_pressure)
unique(CIsale_lookup$order_deadline)
unique(CIsale_lookup$trade_unit)
unique(CIsale_lookup$promotion_horizon)