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
getwd()
df_component <- read_excel("df_component.xlsx")
sales_area_cp <- read_excel("sale_area_cp.xlsx")
df_sku <- read_excel("df_sku.xlsx")
CIpur_lookup <- read_excel("CIpurlookup.xlsx")
CIsale_lookup <-  read_excel("CIsalelookup.xlsx")
##build constant
##operation
operations_constants <- list(
  
  # ============================================================
  # 1. MIXER CHARACTERISTICS
  # ============================================================
  mixer = list(
    technical_min_liters = 8000,    # decision-specific, fill from mixer table
    max_capacity_liters  = 12000,
    operating_time_hours = 2.0,    # mixing time per batch
    cleaning_time_hours  = 2.0,    # flavor change
    fixed_annual_cost    = 62500.00,
    variable_cost_per_hour = 135.00,
    investment_cost        = 312500.00
  ),
  
  # ============================================================
  # 2. BOTTLING LINE CHARACTERISTICS
  # ============================================================
  bottling_line = list(
    
    # base capacities
    hours_per_shift = 40,       # 1 shift = 40h per week
    shift_hours = c(`1` = 40, `2` = 80, `3` = 120, `4` = 144, `5` = 168),
    
    operator_cost_per_year = 40000,
    operator_hours_per_week = 40,
    
    # changeovers
    formula_changeover_hours =NULL,   # depends on line (from API / static table)
    size_changeover_hours    = NULL,
    
    startup_loss_hours = 1,            # always 1 hour after changeover
    
    # breakdowns
    base_breakdown_rate = NULL,        # hourly probability
    breakdown_hours_loss = NULL,       # typical downtime hours
    packaging_quality_sensitivity = NULL
  ),
  
  # ============================================================
  # 3. SHIFT & CAPACITY MANAGEMENT
  # ============================================================
  labor_production = list(
    fte_cost_per_year  = 40000,
    flex_cost_per_hour = 42,
    fte_hours_per_week = 40,
    max_internal_hours = 168,   # 5 shifts
    outsourcing_factor = 2      # outsourcing = 2 × internal cost
  ),
  
  # ============================================================
  # 4. FINISHED GOODS (FG) WAREHOUSE
  # ============================================================
  fg_warehouse = list(
    
    # pallet location cost
    pallet_location_cost_year = 200, # €200 per pallet location each round change
    pallet_overflow_cost_per_day = 3,
    pallet_overflow_cost_per_week = 3 * 5,
    
    # labor
    fte_cost_per_year  = 40000,
    fte_hours_per_week = 40,
    flex_cost_per_hour = 42,
    
    # storing & picking
    store_pallet_minutes        = 6,   # put-away
    pick_orderline_minutes      = 10,
    pick_pallet_minutes         = 6,
    pick_outerbox_minutes       = 3,
    
    # overflow handling
    move_overflow_minutes = 6,
    
    # obsolete processing
    obsolete_pallet_minutes = 6,
    
    # housekeeping
    housekeeping_hours_per_day = 4,
    housekeeping_hours_per_week = 4 * 5
  ),
  
  # ============================================================
  # 5. RAW MATERIAL WAREHOUSE (bạn đã tạo nhưng ghi chung vào đây)
  # ============================================================
  rm_warehouse = list(
    pallet_location_cost_year   = 200,   # inbound warehouse
    pallet_overflow_cost_day   = 3,
    pallet_overflow_cost_week  = 3 * 5,
    intake_hours_per_orderline = 1,         # 1h per order line
    intake_minutes_per_pallet = 6,          # 6 minutes per pallet
    make_available_minutes_pallet = 6,      # picking pallet
    make_available_minutes_tank = 12,       # picking tank
    daily_housekeeping_hours = 4,           # lost time per day
    weekly_housekeeping_hours = 4 * 5,      # per week
    overflow_handling_minutes_per_pallet = 6, # moving pallet to overflow
    ibc_filling_hours = 1,                 # 1h per IBC fill
    fte_cost_per_year  = 40000, #labor rm warehouse
    fte_hours_per_week = 40,
    flex_cost_per_hour = 42
  ),
  
  # ============================================================
  # 6. TANK YARD (OUTSOURCED)
  # ============================================================
  tank_yard = list(
    tank_use_cost_day = 25,
    tank_use_cost_week = 25 * 5,
    intake_handling_per_delivery = 10,
    delivery_to_TFC = 100
  ),
  
  # ============================================================
  # 7. INSPECTION EFFECTS
  # ============================================================
  raw_material_inspection = list(
    add_hours_per_orderline = 2,
    reduce_breakdowns_factor = 0.6    # e.g. reduce breakdowns by 40%
  ),
  
  # ============================================================
  # 8. TIME CONVERSIONS
  # ============================================================
  time = list(
    week_days = 5,
    hours_per_day = 8
  )
)
##Purchase 
supplier_master <- tribble(
  ~supplier, ~product_range, ~country, ~payment_term,
  ~lead_time_days, ~mode, ~shipment_cost,
  ~transport_cost_per_pallet, ~transport_cost_per_ftl,
  ~free_capacity_excl_tfc, ~free_capacity_incl_tfc,
  ~certified,
  
  # Mono Packaging Materials
  "Mono Packaging Materials", "Pack", "France", "At delivery",
  15, NA, 100,
  20, 500,
  26.0, 18.0,
  FALSE,
  
  # Trio PET PLC
  "Trio PET PLC", "PET", "Spain", "At delivery",
  10, NA, 150,
  30, 750,
  10.0, 3.0,
  FALSE,
  
  # Miami Oranges
  "Miami Oranges", "Orange", "United States", "When ordering",
  30, NA, 125,
  40, 1000,
  40.0, 38.0,
  TRUE,
  
  # NO8DO Mango, Truck
  "NO8DO Mango", "Mango", "Spain", "When ordering",
  10, "Truck", 100,
  30, 750,
  9.0, 4.0,
  TRUE,
  
  # NO8DO Mango, Boat
  "NO8DO Mango", "Mango", "Spain", "When ordering",
  25, "Boat", 100,
  20, 500,
  9.0, 4.0,
  TRUE,
  
  # Seitan Vitamins
  "Seitan Vitamins", "Vitamin C", "China", "When ordering",
  60, NA, 100,
  50, 1250,
  52.0, 50.0,
  FALSE
)
purchasing_constants <- list(
  # Global rules
  dual_source_cost_per_supplier = 40000,   # €/year
  emergency_order_weeks = 1,              # dual source order size = 1 week planned demand
  interest_rate = 0.15,                   # 15%/year trên working capital
  
  admin = list(
    inbound_order      = 50,
    inbound_orderline  = 10,
    supplier_maintenance = 40000
  ),
  
  # Supplier-specific master data
  supplier_master = supplier_master
)

##Supplychain constrant
sc_constants <- list(
  
  # ============================================================
  # Working week (TFC)
  # ============================================================
  time = list(
    working_days_per_week = 5
  ),
  
  # ============================================================
  # Logistics / packaging fixed sizes (InfoCenter)
  # ============================================================
  logistics = list(
    drum_liters      = 250,
    ibc_liters       = 1000,
    tanktruck_liters = 30000,
    pallets_per_FTL  = 30
  ),
  
  # ============================================================
  # Decisions: RM stock management
  # ============================================================
  rm_stock_policy = list(
    safety_stock_weeks = NULL,    # named numeric vector per component
    lot_size_weeks     = NULL     # named numeric vector per component
  ),
  
  # ============================================================
  # Decisions: FG stock management
  # ============================================================
  fg_stock_policy = list(
    safety_stock_weeks = NULL,    # named numeric vector per SKU
    
    # production interval is a SC decision in game,
    # but already scrape it via Ops API.
    production_interval_days = NULL  # named numeric vector per SKU
  ),
  
  # ============================================================
  # Decisions: production planning responsiveness
  # ============================================================
  planning = list(
    frozen_period_weeks = NULL   # scalar, decision variable
  )
)

##Sale constrant
unique(sales_area_cp)
colnames(sales_area_cp)
str(sales_area_cp$sales_price)
sales_area_cp <- sales_area_cp %>%
  mutate(sales_price = as.numeric(str_remove_all(sales_price, pattern = "[€ ]")))
customer_master <- tibble::tribble(
  ~customer, ~value_for_money, ~market_share, ~pref_small_pack, ~maturity,
  "Food & Groceries", TRUE, 0.10, FALSE, "high",
  "LAND Market", TRUE, 0.08, FALSE, "low",
  "Dominick's", FALSE, 0.02, TRUE, "medium"
)
sales_area_cp <- sales_area_cp %>%
  rename(
    customer = `customer_sales_area`,
    sku = `product`,
    demand_week_pieces =`demand_per_week_pieces`,
    promo_additional_pct = `additional_sales_as_a_result_of_promotions`,
    sl_pieces = `service_level_pieces`,
    sl_orderlines = `service_level_order_lines`,
    sales_price = `sales_price`,
    demand_value_per_w = `demand_value_per_week`,
    percen_gross_margin_each_piece= `gross_margin_per_piece`,
    gross_margin_per_w = `gross_margin_per_week_6`)
# ============================================================
# Benchmark demand (base demand): use to predoct demand next round
# ============================================================
benchmark_demand <- sales_area_cp %>%
  select(customer, sku, demand_week_pieces)
# ============================================================
# Promo uplift observed
# ============================================================
promo_uplift_observed <- sales_area_cp %>%
  select(customer, sku, promo_additional_pct)
# ============================================================
# Attained CI observed(fit penalty/bonus)
# ============================================================
attained_ci_obs <- sales_area_cp %>%
  left_join(df_sku %>% select(sku, basic_sales_price), by = "sku") %>%
  mutate(attained_ci = sales_price / basic_sales_price) %>%
  select(customer, sku, attained_ci, sl_pieces, sl_orderlines)

sales_constants <- list(
  # ---- (A) Rules/Bounds của Sales (luật cứng)
  sla_rules = list(
    shelf_life_min_pct = 0.40,
    shelf_life_max_pct = 0.85,
    service_level_types = c("units", "order_lines", "orders"),
    trade_unit_types    = c("outer_box", "pallet_layer", "pallet"),
    promo_pressure_levels = c("none", "light", "middle", "heavy")
  ),
  # ---- (B) Promotion uplift rule 
  promotion_uplift = list(
    basic = tibble::tribble(
      ~pressure, ~uplift_min, ~uplift_max,
      "none",   0.000, 0.000,
      "light",  0.005, 0.010,
      "middle", 0.015, 0.020,
      "heavy",  0.040, 0.055
    ),
    value_for_money_multiplier = 2,
    slowmover_multiplier       = 2,
    uplift_method = "midpoint"
  ),
  # ---- (C) Market forces bounds (exogenous)
  market_forces = list(
    volume_factor_min = 0.90,
    volume_factor_max = 1.10
  ),
  # ---- (D) Shortage rules
  shortage_rules = list(
    methods = c("proportional", "fcfs", "priority")
  ),
  # ---- (E) Customer master metadata
  customer_master = customer_master,
  # ---- (F) Observed inputs từ report round trước
  observed = list(
    benchmark_demand = benchmark_demand,
    promo_uplift_observed = promo_uplift_observed,
    attained_ci_obs = attained_ci_obs
  ),
  # ---- (G) Slot cho decision round mới (bạn sẽ fill sau)
  decisions = list(
    customer_level = NULL,
    customer_product_level = NULL
  )
)
##Finance constrant
finance_constants <- list(
  # ============================================================
  # 1. INTEREST / CAPITAL COSTS
  # ============================================================
  interest = list(
    annual_rate = 0.15,            # 15% per annum
    working_days_per_week = 5,
    weeks_per_year = 52
  ),
  # ============================================================
  # 2. ADMINISTRATION COSTS (order handling)
  # ============================================================
  admin = list(
    inbound_order_cost = 50,        # € per inbound order
    inbound_orderline_cost = 10,    # € per inbound order line
    outbound_order_cost = 25,       # € per outbound order
    outbound_orderline_cost = 2,    # € per outbound order line
    supplier_relationship_cost_year = 40000  # € per supplier per year
  ),
  # ============================================================
  # 3. STOCK COSTS
  # ============================================================
  stock_costs = list(
    # (1) interest part
    interest_rate_year = 0.15,
    # valuation rules
    rm_valuation = "avg_purchase_cost",
    fg_valuation = "cogs",
    # (2) space part: take from operations constants later
    space = list(
      rm_pallet_location_cost_year = NULL,   # ops$rm_warehouse$pallet_location_cost_year
      fg_pallet_location_cost_year = NULL,   # ops$fg_warehouse$pallet_location_cost_year
      overflow_cost_per_pallet_day = 3,
      tank_yard_cost_per_tank_day = 25
    ),
    # (3) risk part (optional, can be expanded later)
    risk = list(
      shrinkage_rate = 0.0,      # default 0 if you don't model
      obsolete_cost_full_value = TRUE
    )
  ),
  # ============================================================
  # 4. DISTRIBUTION COSTS
  # ============================================================
  distribution = list(
    km_per_day_capacity = 600,
    base_cost_per_pallet = tibble::tribble(
      ~dc, ~base_cost,
      "DC Netherlands", 31.0
    ),
    
    # LSP (logistics service provider) tiered rates per pallet
    provider_tiers = tibble::tribble(
      ~tier, ~pallet_min, ~pallet_max, ~rate_per_pallet,
      "small",  0,   9,  65,
      "medium", 10, 20,  60,
      "large",  21, Inf, 55
    ),
    
    working_days_per_week = 5
  ),
  # ============================================================
  # 5. OVERHEAD / INDIRECT COSTS
  # ============================================================
  overhead = list(
    # you can keep it as a bucket; allocation later if needed
    fixed_bucket_year = NULL,
    include_supplier_mgmt = TRUE,
    include_customer_mgmt = TRUE,
    include_fp_mgmt = TRUE
  ),
  # ============================================================
  # 6. PROJECT COSTS / CONTRACT COSTS
  # ============================================================
  projects = list(
    # store per project name later
    project_costs_year = NULL
  ),
  
  contract_costs = list(
    termination_costs = NULL
  ),
  # ============================================================
  # 7. INVESTMENT STRUCTURE for ROI
  # ============================================================
  investment = list(
    building_fixed = 2500000,
    machinery = list(
      mixers_investment = NULL,        # from ops master
      bottling_investment = NULL
    ),
    inventory = list(
      rm_avg_value = NULL,             # will be calculated from sim
      fg_avg_value = NULL
    ),
    payment_terms = list(
      accounts_payable_value = NULL,   # from sim + supplier terms
      accounts_receivable_value = NULL # from sim + customer terms
    ),
    software_year = NULL
  ),
  # ============================================================
  # 8. FINANCIAL STATEMENT RULES
  # ============================================================
  statement_rules = list(
    gross_margin = function(revenue, cogs) revenue - cogs,
    operating_profit = function(gross_margin, operating_costs)
      gross_margin - operating_costs,
    ROI = function(operating_profit, investment_total)
      operating_profit / investment_total
  )
)
##help engine
calc_distribution_cost <- function(pallets_shipped, dc = "DC Netherlands",
                                   finance_constants) {
  base <- finance_constants$distribution$base_cost_per_pallet %>%
    dplyr::filter(dc == !!dc) %>%
    dplyr::pull(base_cost)
  
  tier <- finance_constants$distribution$provider_tiers %>%
    dplyr::filter(pallets_shipped >= pallet_min,
                  pallets_shipped <= pallet_max) %>%
    dplyr::slice(1)
  
  rate <- tier$rate_per_pallet
  cost_per_pallet <- base + rate
  
  tibble::tibble(
    dc = dc,
    pallets = pallets_shipped,
    tier = tier$tier,
    base_cost = base,
    provider_rate = rate,
    cost_per_pallet = cost_per_pallet,
    total_cost = pallets_shipped * cost_per_pallet
  )
}
##Format flow
make_empty_flows <- function() {
  list(
    sales = tibble(),
    production = tibble(),
    purchasing = tibble(),
    inventory = tibble(),
    warehousing = tibble(),
    distribution = tibble()
  )
}
##Situation previous round
state0_round <- list(
  rm_stock = tibble(
    component = df_component$component,
    units     = 0   # chưa có stock, cho engine tự mua
  ),
  fg_stock = tibble(
    sku   = df_sku$sku,
    units = 0,
    age_w = 0
  ),
  backlog  = tibble(
    customer = character(),
    sku      = character(),
    units    = double()
  ),
  capacities = list(
    rm_pallet_locations = 900,
    fg_pallet_locations = 1400
  ),
  shifts = tibble(line="SwissFill2", n_shifts=2),
  inventory_snapshot = tibble()  # sẽ được update trong engine
)
##DECISIOn
unique(df_sku)
decisions_round <- list(
  sales = list(
    customer_level = tibble(),
    customer_product_level = tibble()
  ),
  purchasing = list(
    chosen_suppliers = tibble(component=c("Pack","PET","Orange","Mango","Vitamin C"),
                              supplier_primary=c("Mono Packaging Materials","Trio PET PLC",
                                                 "Miami Oranges","NO8DO Mango","Seitan Vitamins"), 
                              supplier_secondary= NULL),
    supplier_params  = tibble()
  ),
  supply_chain = list(
    rm_safety_stock_w = c(Pack=2, PET=3, Orange=2, Mango=2, `Vitamin C`=2.5),
    rm_lot_size_w     = c(Pack=4, PET=4, Orange=4, Mango=4, `Vitamin C`=4),
    fg_safety_stock_w = c(`Fressie Orange 1 liter` = 3,`Fressie Orange/C-power 1 liter`=3,`Fressie Orange/Mango 1 liter` = 3,
                          `Fressie Orange PET` =3,`Fressie Orange/C-power PET` = 3,`Fressie Orange/Mango PET` =3),
    production_interval_days = c(`Fressie Orange 1 liter` = 10, `Fressie Orange/C-power 1 liter`=10, `Fressie Orange/Mango 1 liter` = 10,
                                 `Fressie Orange PET` =10, `Fressie Orange/C-power PET` = 10, `Fressie Orange/Mango PET` =10),
    frozen_period_w = 3
  ),
  operations = list(
    shifts = tibble(line="SwissFill2", n_shifts= c("2")),
    rm_pallet_locations = 900,
    rm_FTE = 3,
    rm_intaketime_day = 3,
    fg_FTE= 4,
    fg_pallet_locations = 1500,
    inspection_on = FALSE,
    mixer_type = "Fruitmix MQ"
  )
)
exo <- list(
  volume_factor = sales_constants$observed$benchmark_demand %>%
    distinct(customer) %>%
    mutate(volume_factor = 1)
)
lookups <- list(
  df_sku       = df_sku,
  df_component = df_component,
  CIpur_lookup = CIpur_lookup,
  CIsale_lookup = CIsale_lookup
)

constants <- list(
  sales        = sales_constants,
  purchasing   = purchasing_constants,
  supply_chain = sc_constants,
  operations   = operations_constants,
  finance      = finance_constants
)
##
unique(CIsale_lookup$Customer) 
unique(CIpur_lookup$Vendor)
colnames(CIsale_lookup)
colnames(CIpur_lookup)
make_sales_decisions <- function(customers, skus){
  expand.grid(
    customer = customers,
    sku = skus
  ) %>%
    as_tibble() %>%
    mutate(
      service_level_type = "Order lines",
      service_level = 95,
      shelf_life = 40,
      order_deadline = "14:00 pm",
      trade_unit = "Box",
      promotion_pressure = "None",
      promotion_horizon = "Short",
      payment_term = 4
    )
}

sales_decision_cp <- make_sales_decisions(
  customers = unique(sales_constants$customer_master$customer),
  skus = unique(df_sku$sku)
) %>%
  left_join(CIsale_lookup, by = c( "customer","sku",
                                   "service_level_type","service_level",
                                   "shelf_life","order_deadline",
                                   "trade_unit","promotion_pressure",
                                   "promotion_horizon","payment_term"))
##engine
# ============================================================
# 0) EMPTY FLOWS
# ============================================================
make_empty_flows <- function() {
  list(
    sales = tibble(),
    production = tibble(),
    purchasing = tibble(),
    inventory = tibble(),
    warehousing = tibble(),
    distribution = tibble()
  )
}

# ============================================================
# 1) DEMAND generation (Sales) - Phase 1
# demand = benchmark * volume_factor * (1 + promo_uplift)
# CI_promised lookup từ CIsale_lookup (nếu thiếu thì =1)
# ============================================================
sales_demand_week <- function(week, state, decisions_sales, sales_constants,
                              ci_sales_lookup, exogenous = list()) {
  
  bench <- sales_constants$observed$benchmark_demand
  
  # volume factor exogenous (nếu không có thì =1)
  vf <- exogenous$volume_factor
  if (is.null(vf)) {
    vf <- tibble(customer = unique(bench$customer), volume_factor = 1)
  }
  
  # promo uplift: lấy từ decisions hoặc observed nếu decisions chưa fill
  if (!is.null(decisions_sales$customer_product_level)) {
    promo_dec <- decisions_sales$customer_product_level %>%
      select(customer, sku, promotion_pressure)
  } else {
    promo_dec <- bench %>%
      mutate(promotion_pressure = "none") %>%
      select(customer, sku, promotion_pressure)
  }
  
  get_promo_uplift <- function(pressure, customer_type="basic", slowmover=FALSE) {
    tab <- sales_constants$promotion_uplift$basic
    row <- tab[tab$pressure == pressure, , drop=FALSE]
    uplift <- (row$uplift_min + row$uplift_max)/2
    
    if (customer_type == "value_for_money") uplift <- uplift * sales_constants$promotion_uplift$value_for_money_multiplier
    if (slowmover) uplift <- uplift * sales_constants$promotion_uplift$slowmover_multiplier
    
    uplift
  }
  
  dem <- bench %>%
    left_join(vf, by="customer") %>%
    left_join(promo_dec, by=c("customer","sku")) %>%
    left_join(sales_constants$customer_master %>% select(customer, value_for_money),
              by="customer") %>%
    mutate(
      customer_type = ifelse(value_for_money, "value_for_money", "basic"),
      promo_uplift = map_dbl(promotion_pressure, ~get_promo_uplift(.x, customer_type="basic")),
      demand_units = demand_week_pieces * volume_factor * (1 + promo_uplift)
    )
  
  # CI_promised lookup (Phase1: nếu join fail thì CI=1)
  if (!is.null(decisions_sales$customer_product_level)) {
    join_keys <- intersect(names(ci_sales_lookup), names(decisions_sales$customer_product_level))
    ci_prom <- ci_sales_lookup %>%
      left_join(decisions_sales$customer_product_level, by = join_keys) %>%
      select(customer, sku, CI_promised = contractIndex)
    dem <- dem %>% left_join(ci_prom, by=c("customer","sku"))
  }
  
  dem %>%
    mutate(CI_promised = ifelse(is.na(CI_promised), 1, CI_promised)) %>%
    select(customer, sku, demand_units, CI_promised, promotion_pressure, volume_factor)
}

# ============================================================
# 2) PRODUCTION PLAN (SC + PI-tool simplified)
# planned_units = demand + target_SSFG - FG_stock
# target_SSFG = safety_stock_weeks * demand_week
# ignore PI/capacity here (Phase1)
# ============================================================
planning_week <- function(week, state, demand_w, decisions_sc, sc_constants, ops_constants) {
  
  fg_stock <- state$fg_stock %>%
    select(sku, fg_units = units)
  
  ssfg_w <- decisions_sc$fg_safety_stock_w
  if (is.null(ssfg_w)) {
    ssfg_w <- setNames(rep(0, length(unique(demand_w$sku))), unique(demand_w$sku))
  }
  
  plan <- demand_w %>%
    group_by(sku) %>%
    summarise(demand_units = sum(demand_units), .groups="drop") %>%
    left_join(fg_stock, by="sku") %>%
    mutate(
      fg_units = ifelse(is.na(fg_units), 0, fg_units),
      target_ss_units = ssfg_w[sku] * demand_units,
      planned_units = pmax(0, demand_units + target_ss_units - fg_units)
    )
  
  plan %>%
    select(sku, demand_units, planned_units, target_ss_units)
}

# ============================================================
# 3) RM REPLENISHMENT (SC + Purchasing simplified)
# Compute RM need from planned_units * BOM
# Reorder if RM_stock < SSRM_target
# SSRM_target = ssrm_weeks * rm_week_need
# Order qty = lot_size_weeks * rm_week_need (>=0)
# Assume immediate receipt Phase1 (lead time ignored)
# ============================================================
rm_replenishment_week <- function(week, state, plan_w, decisions_sc, decisions_purch,
                                  purchasing_constants, ci_purch_lookup, df_sku, df_component) {
  
  # RM usage per SKU from BOM
  bom_long <- df_sku %>%
    select(sku, starts_with("rm_")) %>%
    tidyr::pivot_longer(starts_with("rm_"),
                        names_to="component_raw", values_to="qty_per_unit") %>%
    mutate(component = gsub("^rm_", "", component_raw)) %>%
    select(sku, component, qty_per_unit)
  
  rm_need <- plan_w %>%
    left_join(bom_long, by="sku") %>%
    mutate(rm_week_need = planned_units * qty_per_unit) %>%
    group_by(component) %>%
    summarise(rm_week_need = sum(rm_week_need, na.rm=TRUE), .groups="drop")
  
  rm_stock <- state$rm_stock %>%
    select(component, rm_units = units)
  
  ssrm_w <- decisions_sc$rm_safety_stock_w
  if (is.null(ssrm_w)) {
    ssrm_w <- setNames(rep(0, length(unique(rm_need$component))), unique(rm_need$component))
  }
  
  lot_w <- decisions_sc$rm_lot_size_w
  if (is.null(lot_w)) {
    lot_w <- setNames(rep(1, length(unique(rm_need$component))), unique(rm_need$component))
  }
  
  reorder_tbl <- rm_need %>%
    left_join(rm_stock, by="component") %>%
    mutate(
      rm_units = ifelse(is.na(rm_units), 0, rm_units),
      target_ss_units = ssrm_w[component] * rm_week_need,
      lot_units = lot_w[component] * rm_week_need,
      order_units = ifelse(rm_units < target_ss_units, lot_units, 0)
    )
  
  # Purchase price via CI_purchase lookup (Phase1: CI=1 if fail)
  # join theo keys có sẵn trong lookup
  # decisions_purch$supplier_params phải có component + các biến CI
  purchase_ci <- NULL
  if (!is.null(decisions_purch$supplier_params)) {
    join_keys <- intersect(names(ci_purch_lookup), names(decisions_purch$supplier_params))
    purchase_ci <- ci_purch_lookup %>%
      left_join(decisions_purch$supplier_params, by=join_keys) %>%
      select(component, CI_purch = contractIndex)
  }
  
  reorder_tbl <- reorder_tbl %>%
    left_join(purchase_ci, by="component") %>%
    left_join(df_component %>% select(component, basic_price), by="component") %>%
    mutate(
      CI_purch = ifelse(is.na(CI_purch), 1, CI_purch),
      purchase_price = basic_price * CI_purch,
      purchase_cost_total = order_units * purchase_price
    )
  
  orders_w <- reorder_tbl %>%
    filter(order_units > 0) %>%
    transmute(
      week, component,
      order_qty_units = order_units,
      purchase_price,
      purchase_cost_total
    )
  
  receipts_w <- orders_w %>%
    transmute(week, component, received_qty_units = order_qty_units,
              purchase_price)
  
  list(orders = orders_w, receipts = receipts_w, rm_need = rm_need)
}

# ============================================================
# 4) EXECUTE PRODUCTION (Ops simplified)
# If RM enough => produce planned_units else scale down proportionally
# ignore labor/capacity/breakdown Phase1
# ============================================================
execute_production_week <- function(week, state, plan_w, rm_receipts_w,
                                    decisions_ops, ops_constants) {
  
  # update RM stock temporarily to check availability
  rm_stock_now <- state$rm_stock %>%
    select(component, units) %>%
    full_join(rm_receipts_w %>% select(component, received_qty_units),
              by="component") %>%
    mutate(received_qty_units = ifelse(is.na(received_qty_units), 0, received_qty_units),
           units = ifelse(is.na(units), 0, units),
           units_now = units + received_qty_units)
  
  # compute RM need for full plan
  bom_long <- lookups$df_sku %>%
    select(sku, starts_with("rm_")) %>%
    tidyr::pivot_longer(starts_with("rm_"),
                        names_to="component_raw", values_to="qty_per_unit") %>%
    mutate(component = gsub("^rm_", "", component_raw)) %>%
    select(sku, component, qty_per_unit)
  
  rm_need_full <- plan_w %>%
    left_join(bom_long, by="sku") %>%
    mutate(need = planned_units * qty_per_unit) %>%
    group_by(component) %>%
    summarise(need = sum(need, na.rm=TRUE), .groups="drop")
  
  rm_check <- rm_need_full %>%
    left_join(rm_stock_now %>% select(component, units_now), by="component") %>%
    mutate(units_now = ifelse(is.na(units_now), 0, units_now),
           ratio = ifelse(need>0, pmin(1, units_now/need), 1))
  
  # bottleneck ratio = min across components
  bottleneck_ratio <- min(rm_check$ratio, na.rm=TRUE)
  if (!is.finite(bottleneck_ratio)) bottleneck_ratio <- 1
  
  produced <- plan_w %>%
    mutate(
      produced_units = planned_units * bottleneck_ratio,
      outsourced_units = 0,
      runtime_hours = NA_real_,
      changeover_hours = NA_real_,
      production_cost = 0
    ) %>%
    transmute(
      week, sku,
      planned_units, produced_units, outsourced_units,
      runtime_hours, changeover_hours, production_cost,
      utilization_rate = NA_real_
    )
  
  # RM used per component
  rm_used <- plan_w %>%
    left_join(bom_long, by="sku") %>%
    mutate(used_units = planned_units * bottleneck_ratio * qty_per_unit) %>%
    group_by(component) %>%
    summarise(used_units = sum(used_units, na.rm=TRUE), .groups="drop") %>%
    mutate(week=week)
  
  fg_produced <- produced %>%
    transmute(week, sku, produced_units)
  
  list(production = produced, rm_used = rm_used, fg_produced = fg_produced,
       utilization_rate = NA_real_)
}

# ============================================================
# 5) FULFILL DEMAND + SERVICE + PENALTY (Sales realized simplified)
# ship = min(FG_stock + produced, demand)
# penalty_factor approximated by comparing promised SL vs attained SL
# Phase1: penalty_factor = 1
# ============================================================
fulfill_demand_week <- function(week, state, demand_w, fg_available,
                                decisions_sales, sales_constants) {
  
  fg_stock <- state$fg_stock %>%
    select(sku, fg_units = units)
  
  avail <- fg_stock %>%
    full_join(fg_available %>% select(sku, produced_units), by="sku") %>%
    mutate(fg_units = ifelse(is.na(fg_units), 0, fg_units),
           produced_units = ifelse(is.na(produced_units), 0, produced_units),
           available_units = fg_units + produced_units)
  
  exec <- demand_w %>%
    left_join(avail %>% select(sku, available_units), by="sku") %>%
    mutate(
      available_units = ifelse(is.na(available_units), 0, available_units),
      delivered_units = pmin(demand_units, available_units),
      backorder_units = pmax(0, demand_units - available_units),
      service_level_units = ifelse(demand_units>0, delivered_units/demand_units, 1),
      penalty_factor = 1,               # Phase1 placeholder
      attained_CI = CI_promised * penalty_factor
    ) %>%
    left_join(lookups$df_sku %>% select(sku, basic_sales_price), by="sku") %>%
    mutate(
      sales_price = basic_sales_price * attained_CI,
      revenue = delivered_units * sales_price
    )
  
  # distribution rows minimal (total pallets)
  # pallets ~= delivered_units / units_per_pallet
  exec <- exec %>%
    left_join(lookups$df_sku %>% select(sku, units_per_pallet), by="sku") %>%
    mutate(
      pallets_shipped = ifelse(units_per_pallet>0, delivered_units/units_per_pallet, 0)
    )
  
  exec$distribution_rows <- exec %>%
    group_by(customer) %>%
    summarise(
      week=week,
      distributor="DC Netherlands",
      pallets_shipped=sum(pallets_shipped),
      .groups="drop"
    )
  
  exec %>%
    transmute(
      week, customer, sku,
      demand_units, delivered_units, backorder_units,
      service_level_units,
      basic_sales_price, CI_promised, penalty_factor, attained_CI,
      sales_price, revenue,
      pallets_shipped
    )
}

# ============================================================
# 6) WAREHOUSING WEEK (Ops simplified)
# Phase1: compute avg pallets stock using current state
# ignore flex labor/overflow details
# ============================================================
warehousing_week <- function(week, state, rm_receipts_w, fg_shipments_w,
                             decisions_ops, ops_constants) {
  
  rm_pallets <- state$rm_stock %>%
    left_join(lookups$df_component %>% select(component, pallet_qty), by="component") %>%
    mutate(pallet_qty = ifelse(is.na(pallet_qty), 1, pallet_qty),
           pallets = units / pallet_qty) %>%
    summarise(avg_pallets_in_stock = sum(pallets, na.rm=TRUE)) %>%
    mutate(warehouse="RM", week=week)
  
  fg_pallets <- state$fg_stock %>%
    left_join(lookups$df_sku %>% select(sku, units_per_pallet), by="sku") %>%
    mutate(units_per_pallet = ifelse(is.na(units_per_pallet), 1, units_per_pallet),
           pallets = units / units_per_pallet) %>%
    summarise(avg_pallets_in_stock = sum(pallets, na.rm=TRUE)) %>%
    mutate(warehouse="FG", week=week)
  
  bind_rows(rm_pallets, fg_pallets) %>%
    mutate(
      overflow_pallets = 0,
      overflow_cost = 0,
      handling_cost = 0,
      labor_hours_perm = 0,
      labor_hours_flex = 0
    )
}

# ============================================================
# 7) UPDATE STATE after each week - Phase1
# ============================================================
update_state_week <- function(state, rm_receipts_w, rm_used_w, fg_produced_w, fg_shipped_w) {
  
  # RM stock update
  rm_stock <- state$rm_stock %>%
    full_join(rm_receipts_w %>%
                group_by(component) %>%
                summarise(received=sum(received_qty_units), .groups="drop"),
              by="component") %>%
    full_join(rm_used_w %>%
                group_by(component) %>%
                summarise(used=sum(used_units), .groups="drop"),
              by="component") %>%
    mutate(
      units = ifelse(is.na(units), 0, units),
      received = ifelse(is.na(received), 0, received),
      used = ifelse(is.na(used), 0, used),
      units = pmax(0, units + received - used)
    ) %>%
    select(component, units)
  
  # FG stock update
  fg_stock <- state$fg_stock %>%
    full_join(fg_produced_w %>%
                group_by(sku) %>%
                summarise(produced=sum(produced_units), .groups="drop"),
              by="sku") %>%
    full_join(fg_shipped_w %>%
                group_by(sku) %>%
                summarise(shipped=sum(delivered_units), .groups="drop"),
              by="sku") %>%
    mutate(
      units = ifelse(is.na(units), 0, units),
      produced = ifelse(is.na(produced), 0, produced),
      shipped = ifelse(is.na(shipped), 0, shipped),
      units = pmax(0, units + produced - shipped),
      age_w = ifelse(is.na(age_w), 0, age_w + 1) # ageing placeholder
    ) %>%
    select(sku, units, age_w)
  
  # inventory snapshot for flows
  inventory_snapshot <- bind_rows(
    rm_stock %>% transmute(item_type="RM", item=component, avg_stock_units=units),
    fg_stock %>% transmute(item_type="FG", item=sku, avg_stock_units=units)
  )
  
  state$rm_stock <- rm_stock
  state$fg_stock <- fg_stock
  state$inventory_snapshot <- inventory_snapshot
  state
}

# ============================================================
# 8) FINANCE AGGREGATE ROUND - Phase1
# ROI = operating_profit / investment_total
# investment_total rough: building + avg inventories
# ============================================================
finance_aggregate_round <- function(flows, finance_constants, ops_constants) {
  
  revenue <- sum(flows$sales$revenue, na.rm=TRUE)
  
  purchase_costs <- sum(flows$purchasing$purchase_cost_total, na.rm=TRUE)
  production_costs <- sum(flows$production$production_cost, na.rm=TRUE)
  
  cogs <- purchase_costs + production_costs
  
  gross_margin <- revenue - cogs
  
  distribution_costs <- 0
  if (nrow(flows$distribution) > 0) {
    distribution_costs <- sum(
      purrr::map_dbl(flows$distribution$pallets_shipped, ~{
        calc_distribution_cost(.x, dc="DC Netherlands",
                               finance_constants=finance_constants)$total_cost
      }),
      na.rm=TRUE
    )
  }
  
  handling_costs <- sum(flows$warehousing$handling_cost, na.rm=TRUE)
  
  operating_costs <- distribution_costs + handling_costs
  
  operating_profit <- gross_margin - operating_costs
  
  # investments (rough Phase1)
  avg_rm_value <- mean(flows$inventory %>% filter(item_type=="RM") %>% pull(avg_stock_units), na.rm=TRUE)
  avg_fg_value <- mean(flows$inventory %>% filter(item_type=="FG") %>% pull(avg_stock_units), na.rm=TRUE)
  
  investment_total <- finance_constants$investment$building_fixed +
    ifelse(is.finite(avg_rm_value), avg_rm_value, 0) +
    ifelse(is.finite(avg_fg_value), avg_fg_value, 0)
  
  ROI_pred <- ifelse(investment_total > 0, operating_profit / investment_total, NA_real_)
  
  list(
    revenue = revenue,
    purchase_costs = purchase_costs,
    production_costs = production_costs,
    cogs = cogs,
    gross_margin = gross_margin,
    distribution_costs = distribution_costs,
    handling_costs = handling_costs,
    operating_profit = operating_profit,
    investment_total = investment_total,
    ROI_pred = ROI_pred
  )
}
lookups <- list(
  df_sku = df_sku,
  df_component = df_component,
  CIpur_lookup = CIpur_lookup,
  CIsale_lookup = CIsale_lookup
)

constants <- list(
  sales = sales_constants,
  purchasing = purchasing_constants,
  supply_chain = sc_constants,
  operations = operations_constants,
  finance = finance_constants
)

state0_round <- list(
  rm_stock = tibble(component = unique(df_component$component), units = 0),
  fg_stock = tibble(sku = unique(df_sku$sku), units = 0, age_w=0),
  backlog = tibble(),
  inventory_snapshot = tibble()
)

decisions_round <- list(
  sales = list(
    customer_level = NULL,
    customer_product_level = NULL
  ),
  purchasing = list(
    supplier_params = NULL
  ),
  supply_chain = list(
    rm_safety_stock_w = NULL,
    rm_lot_size_w = NULL,
    fg_safety_stock_w = NULL
  ),
  operations = list()
)

out <- engine_round(
  state0 = state0_round,
  decisions = decisions_round,
  constants = constants,
  lookups = lookups,
  exogenous = list(volume_factor = tibble(customer=unique(df_sku$sku), volume_factor=1)),
  n_weeks = 26
)

out$finance_round$ROI_pred

