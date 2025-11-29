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
customer_master <- tibble::tribble(
  ~customer, ~value_for_money, ~market_share, ~pref_small_pack, ~maturity,
  "Food & Groceries", TRUE, 0.10, FALSE, "high",
  "LAND Market", TRUE, 0.08, FALSE, "low",
  "Dominick's", FALSE, 0.02, TRUE, "medium"
)
sales_area_cp <- sales_area_cp %>%
  rename(
    customer = `Customer - Sales Area`,
    sku = `Product`,
    demand_week_pieces =`Demand per week (pieces)`,
    promo_additional_pct = `Additional sales as a result of promotions (%)`,
    sl_pieces = `Service level (pieces)`,
    sl_orderlines = `Service level (order lines)`,
    sales_price = `Sales price`,
    demand_value_per_w = `Demand value per week`,
    percen_gross_margin_each_piece= `Gross margin per piece`,
    gross_margin_per_w = `Gross margin per week...6`)
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
    customer_level = tibble(...),
    customer_product_level = tibble(...)
  ),
  purchasing = list(
    chosen_suppliers = tibble(component=c("Pack","PET","Orange","Mango","Vitamin C"),
                              supplier_primary=c("Mono Packaging Materials","Trio PET PLC",
                                                 "Miami Oranges","NO8DO Mango","Seitan Vitamins"), 
                              supplier_secondary= NULL),
    supplier_params  = tibble(component, quality, delivery_window, delivery_reliability,
                              trade_unit, payment_term)
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