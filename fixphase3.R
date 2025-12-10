# =========================
# 0) LOAD DATA
# =========================
df_component  <- read_excel("df_component.xlsx")  %>% clean_names()
sales_area_cp <- read_excel("sale_area_cp.xlsx") %>% clean_names()
df_sku        <- read_excel("df_sku.xlsx")        %>% clean_names()
CIpur_lookup  <- read_excel("CIpurlookup.xlsx")
CIsale_lookup <- read_excel("CIsalelookup.xlsx")
str(CIsale_lookup)
str(CIpur_lookup)

# =========================
# 1) NORMALIZE LOOKUPS
# =========================
norm_customer <- function(x){
  x1 <- x %>%
    stringr::str_replace_all("_"," ") %>%
    stringr::str_squish()
  
  dplyr::case_when(
    x1 %in% c("Food Groceries","Groceries") ~ "Food & Groceries",
    x1 %in% c("Land Market","Landmark")    ~ "LAND Market",
    x1 %in% c("Dominick s","Dominick")     ~ "Dominick's",
    TRUE ~ x1
  )
}
norm_component <- function(x){##Beta 0.0.3 
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+","_") %>%  # space -> _
    stringr::str_squish()
}
df_sku <- df_sku %>%##beta 0.0.2
  mutate(sku = str_squish(sku))
df_component <- df_component %>%
  mutate(component = norm_component(component)) ##beta 0.0.3
#~(1A) Sales CI lookup normalize
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
    ci_promised          = contract_index
  ) %>%
  mutate(
    customer = norm_customer(customer),##
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

#~(1B) Purchasing CI lookup normalize
CIpur_lookup <- CIpur_lookup %>%
  rename(
    quality              = Quality,
    delivery_window      = DeliveryWindow,
    delivery_reliability = DeliveryReliability,
    trade_unit           = Tradeunit,
    payment_term         = Paymentterm,
    vendor_lookup        = Vendor,
    ci_purch             = ContractIndex
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
##Update phase 3
CIsale_lookup <- CIsale_lookup %>%
  mutate(
    customer = norm_customer(customer),
    promotional_pressure = str_to_lower(promotional_pressure),
    trade_unit = str_to_lower(trade_unit),
    promotion_horizon = str_to_lower(promotion_horizon),
    order_deadline = stringr::str_replace_all(order_deadline, "\\s+",""),
    payment_term = as.character(payment_term),
    shelf_life = as.character(round(as.numeric(shelf_life))), # tránh 80.0 vs "80"
    service_level = as.numeric(service_level),
    ci_promised = as.numeric(ci_promised)
  )

CIpur_lookup <- CIpur_lookup %>%
  mutate(
    vendor = as.character(vendor),
    quality = str_to_lower(quality),
    delivery_window = str_to_lower(delivery_window),
    trade_unit = str_to_lower(trade_unit),
    payment_term = as.character(payment_term),
    delivery_reliability = as.numeric(readr::parse_number(as.character(delivery_reliability))),
    ci_purch = as.numeric(ci_purch)
  )

# =========================
# 2) CONSTANTS 
# =========================

operations_constants <- list(
  mixer = list(###
    technical_min_liters = 8000,
    max_capacity_liters  = 12000,
    operating_time_hours = 2.0,
    cleaning_time_hours  = 2.0,
    fixed_annual_cost    = 62500.00,
    variable_cost_per_hour = 135.00,
    investment_cost        = 312500.00
  ),
  bottling_line = list(
    line_type = "Swiss Fill 2",
    capacity_lph = 3100,                 # <<< Swiss Fill 2 capacity
    n_operators = 5,                     # <<< Swiss Fill 2 operators
    operator_cost_per_year = 40000,      # <<< per operator
    fixed_annual_cost = 98000,           # <<< Swiss Fill 2 fixed cost
    hours_per_shift = 40,
    shift_hours = c(`1`=40,`2`=80,`3`=120,`4`=144,`5`=168),
    operator_cost_per_year = 40000,
    operator_hours_per_week = 40,
    formula_changeover_hours = 2.0,##
    size_changeover_hours    = 4.0,##
    startup_loss_hours = 0.1,
    investment_cost = 490000,             # <<< Swiss Fill 2 investment
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
customer_master <- customer_master %>%
  mutate(customer = norm_customer(customer))##

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
  )%>%
  mutate(
    customer = norm_customer(customer),
    sku = stringr::str_squish(sku)   ##beta 0.0.2
  )
##Update 0.2.0
sales_area_cp <- sales_area_cp %>%
  mutate(demand_week_pieces = as.numeric(gsub("\\.", "", demand_week_pieces)))
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
  
  # nếu pallets_shipped NA/Inf -> báo để biết data bẩn
  if (length(pallets_shipped)==0 || is.na(pallets_shipped) || !is.finite(pallets_shipped)){
    warning("pallets_shipped invalid (NA/Inf). Set distribution cost = 0 for this row.")
    return(0)
  }
  
  # nếu =0 hoặc âm -> không ship => cost 0
  if (pallets_shipped <= 0){
    return(0)
  }
  
  base_tbl <- finance_constants$distribution$base_cost_per_pallet
  tier_tbl <- finance_constants$distribution$provider_tiers
  
  base <- base_tbl %>%
    dplyr::filter(dc == !!dc) %>%
    dplyr::pull(base_cost)
  
  if (length(base)==0 || is.na(base)){
    warning(paste0("No base_cost found for dc=", dc, ". Using base_cost=0."))
    base <- 0
  }
  
  tier <- tier_tbl %>%
    dplyr::filter(pallets_shipped >= pallet_min,
                  pallets_shipped <= pallet_max) %>%
    dplyr::slice(1)
  
  if (nrow(tier)==0){
    warning(paste0("No tier matched pallets_shipped=", pallets_shipped,
                   ". Using only base_cost."))
    return(pallets_shipped * base)
  }
  
  cost_per_pallet <- base + tier$rate_per_pallet
  pallets_shipped * cost_per_pallet
}

df_sku %>%
  filter(is.na(units_per_pallet) | units_per_pallet <= 0) %>%
  select(sku, units_per_pallet)#check
# promo uplift midpoint
get_promo_uplift <- function(p){
  tab <- sales_constants$promotion_uplift$basic
  row <- tab %>% dplyr::filter(.data$pressure == p)
  (row$uplift_min + row$uplift_max)/2
}
#Empty flow 
make_empty_flows <- function(){
  list(
    sales       = tibble(),
    production  = tibble(),
    operations  = tibble(),
    purchasing  = tibble(),
    purchasing_receipts = tibble(),# receipts
    inventory   = tibble(),
    warehousing = tibble(),
    distribution= tibble(),
    pi = tibble() #update beta 0.1.0
  )
}
#Inventory holding + spoilage
inventory_snapshot_week <- function(week, state, lookups, decisions, constants){
  
  df_component <- lookups$df_component
  df_sku <- lookups$df_sku
  supplier_params <- decisions$purchasing$supplier_params
  interest_rate <- constants$purchasing$interest_rate
  
  # RM value (basic_price * ci_purch)
  rm_val_tbl <- state$rm_stock %>%
    left_join(df_component %>% select(component, basic_price), by="component") %>%
    left_join(supplier_params %>% select(component, ci_purch), by="component") %>%
    mutate(
      basic_price = replace_na(basic_price, 0),
      ci_purch = replace_na(ci_purch, 1),
      rm_value = units * basic_price * ci_purch
    )
  
  rm_value_total <- sum(rm_val_tbl$rm_value, na.rm=TRUE)
  rm_holding_cost <- rm_value_total * interest_rate / 52
  
  # FG value (basic_sales_price)
  fg_val_tbl <- state$fg_stock %>%
    left_join(df_sku %>% select(sku, basic_sales_price), by="sku") %>%
    mutate(
      basic_sales_price = replace_na(basic_sales_price, 0),
      fg_value = units * basic_sales_price
    )
  
  fg_value_total <- sum(fg_val_tbl$fg_value, na.rm=TRUE)
  fg_holding_cost <- fg_value_total * interest_rate / 52
  
  tibble(
    week = week,
    rm_value_total = rm_value_total,
    fg_value_total = 0,##update 0.1.1 after add PI
    rm_holding_cost = rm_holding_cost,
    fg_holding_cost = 0##update 0.1.1 after add PI
  )
}
##update beta 0.0.7 FG warehouse cost + scrap cost
fg_warehouse_week <- function(week, state, lookups, operations_constants,decisions_ops){
  
  df_sku <- lookups$df_sku
  fg_const <- operations_constants$fg_warehouse
  
  fg_tbl <- state$fg_stock %>%
    left_join(df_sku %>% select(sku, units_per_pallet, basic_sales_price),
              by="sku") %>%
    mutate(
      units_per_pallet = replace_na(units_per_pallet, NA_real_),
      pallets = ifelse(is.na(units_per_pallet) | units_per_pallet<=0, 0, units/units_per_pallet),
      fg_value = units * replace_na(basic_sales_price,0)
    )
  
  pallets_total <- sum(fg_tbl$pallets, na.rm=TRUE)
  
  ##Update 0.1.6 : pallet capacity knob 
  pallet_capacity <- decisions_ops$fg_pallet_locations %||% 10
  
  overflow_pallets <- pmax(0, pallets_total - pallet_capacity)
  
  pallet_location_cost_week <- pallet_capacity * fg_const$pallet_location_cost_year / 52
  overflow_cost_week <- overflow_pallets * fg_const$pallet_overflow_cost_per_week
  
  scrap_cost_week <- state$fg_stock %>%
    left_join(df_sku %>% select(sku, basic_sales_price), by="sku") %>%
    mutate(
      obsolete_units = replace_na(obsolete_units,0),
      basic_sales_price = replace_na(basic_sales_price,0),
      scrap_cost = obsolete_units * basic_sales_price
    ) %>%
    summarise(scrap_cost=sum(scrap_cost, na.rm=TRUE)) %>% pull(scrap_cost)
  
  tibble(
    week=week,
    fg_pallets=pallets_total,
    fg_overflow_pallets=overflow_pallets,
    fg_location_cost=pallet_location_cost_week,
    fg_overflow_cost=overflow_cost_week,
    scrap_cost=scrap_cost_week
  )
}
##Update 0.1.9 RM warehouse cost + labor + inspection 
rm_warehouse_week <- function(week, state, lookups, operations_constants,
                              decisions_ops, orders_w = tibble(), receipts_w = tibble(),
                              sc_constants = NULL, supplier_params = NULL){
  
  rm_const <- operations_constants$rm_warehouse
  insp_const <- operations_constants$raw_material_inspection %||% list(add_hours_per_orderline=0)
  logi_const <- if (!is.null(sc_constants)) sc_constants$logistics else list(
    drum_liters=250, ibc_liters=1000, tanktruck_liters=30000
  )
  
  df_comp <- lookups$df_component
  #Update 0.2.1
  #pallets in RM stock (overflow capacity)
  rm_stock_tbl <- state$rm_stock %>%
    mutate(component = norm_component(component))
  
  if (!is.null(supplier_params) && "trade_unit" %in% names(supplier_params)){
    
    rm_stock_tbl <- rm_stock_tbl %>%
      left_join(
        supplier_params %>%
          mutate(component = norm_component(component),
                 trade_unit = str_to_lower(replace_na(trade_unit,"pallet"))) %>%
          select(component, trade_unit),
        by="component"
      ) %>%
      mutate(
        trade_size_units = case_when(
          trade_unit == "drum" ~ logi_const$drum_liters,
          trade_unit == "ibc"  ~ logi_const$ibc_liters,
          trade_unit %in% c("tank","tanktruck") ~ logi_const$tanktruck_liters,
          TRUE ~ NA_real_
        ),
        pallets = case_when(
          trade_unit %in% c("drum","ibc") ~ units / trade_size_units,  # 1 drum/IBC = 1 pallet
          trade_unit %in% c("tank","tanktruck") ~ 0,                   # tank ở tank yard
          TRUE ~ NA_real_
        )
      )
    
    # nếu pallet NA (pack/PET) thì fallback sang units_per_pallet nếu có
    if ("units_per_pallet" %in% names(df_comp)){
      rm_stock_tbl <- rm_stock_tbl %>%
        left_join(df_comp %>% select(component, units_per_pallet), by="component") %>%
        mutate(
          units_per_pallet = replace_na(units_per_pallet, NA_real_),
          pallets = ifelse(
            is.na(pallets),
            ifelse(is.na(units_per_pallet) | units_per_pallet<=0, 0, units/units_per_pallet),
            pallets
          )
        )
    } else {
      rm_stock_tbl$pallets <- replace_na(rm_stock_tbl$pallets, 0)
    }
    
    pallets_total <- sum(rm_stock_tbl$pallets, na.rm=TRUE)
    
  } else {
    # fallback cũ nếu chưa pass supplier_params
    if ("units_per_pallet" %in% names(df_comp)) {
      rm_tbl <- rm_stock_tbl %>%
        left_join(df_comp %>% select(component, units_per_pallet), by="component") %>%
        mutate(
          units_per_pallet = replace_na(units_per_pallet, NA_real_),
          pallets = ifelse(is.na(units_per_pallet) | units_per_pallet <= 0, 0, units/units_per_pallet)
        )
      pallets_total <- sum(rm_tbl$pallets, na.rm = TRUE)
    } else {
      pallets_total <- 0
      warning("df_component has no units_per_pallet AND no supplier_params -> RM pallets_total=0.")
    }
  }
  
  rm_capacity <- decisions_ops$rm_pallet_locations %||% 10
  overflow_pallets <- pmax(0, pallets_total - rm_capacity)
  
  rm_location_cost_week <- rm_capacity * rm_const$pallet_location_cost_year / 52
  rm_overflow_cost_week <- overflow_pallets * rm_const$pallet_overflow_cost_week
  
  #workload hours
  inbound_orderlines <- nrow(orders_w)
  
  # pallets received this week
  pallets_received <- 0
  if (nrow(receipts_w) > 0){
    
    tmp_rec <- receipts_w %>%
      mutate(component = norm_component(component)) %>%
      left_join(
        orders_w %>% 
          mutate(component = norm_component(component),
                 trade_unit = str_to_lower(replace_na(trade_unit,"pallet"))) %>%
          select(component, trade_unit) %>% distinct(),
        by="component"
      ) %>%
      mutate(
        trade_unit = str_to_lower(replace_na(trade_unit,"pallet")),
        trade_size_units = case_when(
          trade_unit == "drum" ~ logi_const$drum_liters,
          trade_unit == "ibc"  ~ logi_const$ibc_liters,
          trade_unit %in% c("tank","tanktruck") ~ logi_const$tanktruck_liters,
          TRUE ~ NA_real_
        ),
        pallets = case_when(
          trade_unit %in% c("drum","ibc") ~ received_qty_units / trade_size_units,
          trade_unit %in% c("tank","tanktruck") ~ 0,
          TRUE ~ NA_real_
        )
      )
    
    if ("units_per_pallet" %in% names(df_comp)){
      tmp_rec <- tmp_rec %>%
        left_join(df_comp %>% select(component, units_per_pallet), by="component") %>%
        mutate(
          units_per_pallet = replace_na(units_per_pallet, NA_real_),
          pallets = ifelse(
            is.na(pallets),
            ifelse(is.na(units_per_pallet) | units_per_pallet<=0, 0, received_qty_units/units_per_pallet),
            pallets
          )
        )
    } else {
      tmp_rec$pallets <- replace_na(tmp_rec$pallets, 0)
    }
    
    pallets_received <- sum(tmp_rec$pallets, na.rm=TRUE)
  }
  
  trade_unit_tbl <- orders_w %>%
    select(component, trade_unit) %>%
    mutate(trade_unit = str_to_lower(replace_na(trade_unit, "pallet"))) %>%
    distinct()
  
  n_tank_deliv <- trade_unit_tbl %>% filter(trade_unit %in% c("tank","tanktruck")) %>% nrow()
  
  intake_hours <- inbound_orderlines * rm_const$intake_hours_per_orderline
  intake_pallet_hours <- pallets_received * rm_const$intake_minutes_per_pallet / 60
  
  make_avail_hours <- 
    pallets_received * rm_const$make_available_minutes_pallet / 60 +
    n_tank_deliv * rm_const$make_available_minutes_tank / 60
  
  overflow_handle_hours <- overflow_pallets * rm_const$overflow_handling_minutes_per_pallet / 60
  
  ibc_fill_hours <- trade_unit_tbl %>%
    filter(trade_unit == "ibc") %>%
    nrow() * rm_const$ibc_filling_hours
  
  housekeeping_hours <- rm_const$weekly_housekeeping_hours
  
  inspection_on <- decisions_ops$rm_inspection_on %||% FALSE
  inspection_hours <- if (inspection_on) inbound_orderlines * insp_const$add_hours_per_orderline else 0
  
  workload_hours <- intake_hours + intake_pallet_hours + make_avail_hours +
    overflow_handle_hours + ibc_fill_hours + housekeeping_hours + inspection_hours
  
  perm_rate <- rm_const$fte_cost_per_year / (52 * rm_const$fte_hours_per_week)
  
  n_fte_rm <- decisions_ops$n_fte_rm_wh %||% 1
  perm_cap_hours <- n_fte_rm * rm_const$fte_hours_per_week
  
  perm_hours <- pmin(workload_hours, perm_cap_hours)
  flex_hours <- pmax(0, workload_hours - perm_cap_hours)
  
  labor_cost_week <- (perm_hours + flex_hours) * perm_rate
  
  tibble(
    week = week,
    rm_pallets = pallets_total,
    rm_overflow_pallets = overflow_pallets,
    rm_location_cost = rm_location_cost_week,
    rm_overflow_cost = rm_overflow_cost_week,
    rm_workload_hours = workload_hours,
    rm_perm_hours = perm_hours,
    rm_flex_hours = flex_hours,
    rm_labor_cost = labor_cost_week,
    rm_inspection_hours = inspection_hours,
    rm_inspection_on = inspection_on
  )
}
##update helper admin beta 0.0.7
admin_cost_week <- function(week, orders_w, sales_exec_w, purchasing_constants){
  
  adm <- purchasing_constants$admin
  
  inbound_orders     <- nrow(orders_w)
  inbound_orderlines <- nrow(orders_w)   # Phase 2 đơn giản coi 1 line = 1 orderline
  
  outbound_orders     <- sales_exec_w %>% distinct(customer, week) %>% nrow()
  outbound_orderlines <- nrow(sales_exec_w)
  
  cost <- inbound_orders     * adm$inbound_order +
    inbound_orderlines * adm$inbound_orderline +
    outbound_orders    * adm$outbound_order +
    outbound_orderlines* adm$outbound_orderline +
    (adm$supplier_maintenance/52)
  
  tibble(week=week,
         inbound_orders=inbound_orders,
         inbound_orderlines=inbound_orderlines,
         outbound_orders=outbound_orders,
         outbound_orderlines=outbound_orderlines,
         admin_cost=cost)
}
##Decision knob shortage rule #update beta 0.0.9
allocate_shortage <- function(demand_tbl, rule=c("proportional","fcfs","priority")){
  rule <- match.arg(rule)
  
  demand_tbl <- demand_tbl %>%
    group_by(sku) %>%
    group_modify(function(df, key){
      
      avail <- df$available_units[1] %||% 0
      avail <- replace_na(avail, 0)
      
      if (nrow(df)==0){
        return(df)
      }
      
      if (avail <= 0){
        df$delivered_units <- 0
        df$backorder_units <- df$demand_units
        return(df)
      }
      
      if (rule=="proportional"){
        tot_dem <- sum(df$demand_units, na.rm=TRUE)
        if (tot_dem <= 0){
          df$delivered_units <- 0
        } else {
          df$delivered_units <- df$demand_units * pmin(1, avail/tot_dem)
        }
        df$backorder_units <- df$demand_units - df$delivered_units
        return(df)
      }
      
      # FCFS hoặc PRIORITY: giao tuần tự theo thứ tự
      if (rule=="fcfs"){
        df <- df %>% arrange(order_deadline_num, .by_group=FALSE)
      }
      
      if (rule=="priority"){
        df <- df %>% arrange(desc(priority_score), order_deadline_num, .by_group=FALSE)
      }
      
      cum_dem <- cumsum(df$demand_units)
      prev_cum <- dplyr::lag(cum_dem, default=0)
      
      df$delivered_units <- pmax(0, pmin(df$demand_units, avail - prev_cum))
      df$backorder_units <- df$demand_units - df$delivered_units
      df
    }) %>%
    ungroup()
  
  demand_tbl
}
##PI helper update beta 0.1.0
pi_tool_predict_week <- function(week, demand_w, decisions_sc, df_sku,plan_w,produced_w, ## 0.1.8 ver2
                                 operations_constants, purchasing_constants,
                                 decisions_ops, sc_constants){
  
  op_const <- operations_constants$bottling_line
  cap_lph  <- op_const$capacity_lph
  f_co_h   <- op_const$formula_changeover_hours
  s_co_h   <- op_const$size_changeover_hours
  su_h     <- op_const$startup_loss_hours
  
  operator_cost_per_hour_one  <- op_const$operator_cost_per_year / (52 * op_const$operator_hours_per_week)
  cost_per_hour_line <- operator_cost_per_hour_one * op_const$n_operators
  
  interest_rate <- purchasing_constants$interest_rate
  
  # interval decision theo SKU (DAYS)
  interval_d <- decisions_sc$fg_production_interval_d
  if (is.null(interval_d)){
    interval_d <- setNames(rep(5, length(unique(demand_w$sku))), unique(demand_w$sku)) # default 5 days
  }
  
  # working days per week (game là 5)
  wd_pw <- sc_constants$time$working_days_per_week %||% 5
  #ACTUAL lot produced per SKU (internal + outsourced) ##0.1.8 ver2
  prod_sku <- produced_w %>%
    select(sku, produced_units, outsourced_units) %>%
    mutate(
      produced_units  = replace_na(produced_units, 0),
      outsourced_units= replace_na(outsourced_units, 0),
      Q_total = produced_units + outsourced_units,   # FG thực sự nhận về
      Q_int   = produced_units                       # chạy trên line nội bộ
    ) %>%
    left_join(df_sku %>% select(sku, liters_per_pack, basic_sales_price), by="sku") %>%
    mutate(
      liters_per_pack   = replace_na(liters_per_pack, 0),
      basic_sales_price = replace_na(basic_sales_price, 0),
      
      # runtime chỉ tính lượng chạy nội bộ
      liters_int = Q_int * liters_per_pack,
      run_hours  = ifelse(cap_lph > 0, liters_int / cap_lph, 0),
      
      # cycle stock cost dựa trên total lot (nội bộ + outsource)
      avg_inv_units = Q_total / 2,
      avg_inv_value = avg_inv_units * basic_sales_price,
      stock_cost_year = avg_inv_value * interest_rate,
      
      interval_days = interval_d[sku] %>% replace_na(5),
      interval_days = pmin(25, pmax(1, interval_days)),
      k_weeks = interval_days / wd_pw
    )
  
  # SKU count for changeover/startup: chỉ count SKU thực sự chạy nội bộ
  n_sku_int_run <- sum(prod_sku$Q_int > 0, na.rm=TRUE)
  n_co <- max(0, n_sku_int_run - 1)
  
  co_formula_hours <- n_co * f_co_h
  co_size_hours    <- n_co * s_co_h
  startup_hours    <- n_sku_int_run * su_h
  
  runtime_hours    <- sum(prod_sku$run_hours, na.rm=TRUE)
  stock_costs_year <- sum(prod_sku$stock_cost_year, na.rm=TRUE)
  
  startup_cost_year    <- startup_hours * cost_per_hour_line * 52
  changeover_costs_year<- (co_formula_hours + co_size_hours) * cost_per_hour_line * 52
  
  total_time_hours <- runtime_hours + co_formula_hours + co_size_hours + startup_hours
  
  num_shifts <- decisions_ops$num_shifts %||% 1
  available_hours <- op_const$shift_hours[as.character(num_shifts)]
  available_hours <- ifelse(is.na(available_hours), 40, available_hours)
  
  utilization <- ifelse(available_hours>0, total_time_hours/available_hours, 0)
  
  tibble(
    week = week,
    stock_costs_year = stock_costs_year,
    run_time_hours = runtime_hours,
    startup_loss_cost_year = startup_cost_year,
    changeover_formula_hours = co_formula_hours,
    changeover_size_hours = co_size_hours,
    changeover_costs_year = changeover_costs_year,
    total_costs_year = stock_costs_year + startup_cost_year + changeover_costs_year,
    total_time_hours = total_time_hours,
    utilization_rate = utilization,
    n_sku_run = n_sku_int_run,
    n_changeover = n_co
  )
}
##Update 0.1.6 Cate management
auto_assortment_phase3 <- function(
    benchmark_demand,
    sales_exec_hist = NULL,
    df_sku = NULL,
    sales_decision_cp = NULL,
    assortment_prior = NULL,
    rules = list(
      margin_min_per_unit = 0,        # <0 => margin âm
      margin_fail_weeks = 4,          # margin âm >=4 tuần thì tắt
      sla_fail_rate_max = 0.30,       # >30% tuần fail SLA thì tắt
      slowmover_ratio = 0.20,         # demand <20% median customer demand => slow
      min_weeks_obs = 4               # cần ít nhất 4 tuần history mới xét SLA/margin
    )
){
  stopifnot(all(c("customer","sku","demand_week_pieces") %in% names(benchmark_demand)))
  ##
  benchmark_full <- tidyr::expand_grid(
    customer = unique(benchmark_demand$customer),
    sku      = unique(df_sku$sku)
  ) %>%
    left_join(benchmark_demand, by=c("customer","sku")) %>%
    mutate(demand_week_pieces = replace_na(demand_week_pieces, 0))
  #0) base matrix from benchmark
  base <- benchmark_full %>%
    mutate(
      customer = norm_customer(customer),
      sku = stringr::str_squish(sku),
      base_active = demand_week_pieces > 0
    ) %>%
    select(customer, sku, demand_week_pieces, base_active)
  
  # nếu không có history => trả baseline luôn
  if (is.null(sales_exec_hist) || is.null(df_sku)) {
    out <- base %>%
      transmute(customer, sku, active = base_active)
    
    # apply prior override nếu có
    if (!is.null(assortment_prior) && nrow(assortment_prior)>0){
      out <- out %>%
        left_join(assortment_prior %>% select(customer, sku, active_prior=active),
                  by=c("customer","sku")) %>%
        mutate(active = ifelse(!is.na(active_prior), active_prior, active)) %>%
        select(customer, sku, active)
    }
    return(out)
  }
  
  # 1) margin per unit from history 
  # sales_exec_hist expected cols: week, customer, sku, demand_units, delivered_units, sales_price, revenue
  hist <- sales_exec_hist %>%
    mutate(
      customer = norm_customer(customer),
      sku = stringr::str_squish(sku)
    ) %>%
    left_join(df_sku %>% select(sku, basic_sales_price), by="sku") %>%
    mutate(
      basic_sales_price = replace_na(basic_sales_price, 0),
      unit_margin = sales_price - basic_sales_price
    )
  
  margin_stat <- hist %>% ##update 0.1.9 
    group_by(customer, sku) %>%
    summarise(
      weeks_obs = n_distinct(week),
      neg_margin_weeks = sum(unit_margin < rules$margin_min_per_unit &
                               delivered_units > 0, na.rm=TRUE),
      avg_unit_margin = mean(unit_margin[delivered_units > 0], na.rm=TRUE),
      .groups="drop"
    ) %>%
    mutate(
      margin_bad = weeks_obs >= rules$min_weeks_obs*2 & ##update 0.2.3
        neg_margin_weeks >= rules$margin_fail_weeks
    )
  
  #2) SLA fail rate
  # nếu có sales_decision_cp thì lấy service_level theo customer-sku
  if (!is.null(sales_decision_cp) && "service_level" %in% names(sales_decision_cp)){
    sl_tbl <- sales_decision_cp %>%
      mutate(
        customer = norm_customer(customer),
        sku = stringr::str_squish(sku),
        service_level = as.numeric(service_level)/100
      ) %>%
      select(customer, sku, service_level)
  } else {
    sl_tbl <- tibble(customer=character(), sku=character(), service_level=numeric())
  }
  
  sla_stat <- hist %>%##update 0.2.0
    left_join(sl_tbl, by=c("customer","sku")) %>%
    mutate(
      service_level = replace_na(service_level, 0.95),
      # chỉ xét SLA khi có demand và *có giao hàng*
      sla_eval = demand_units > 0 & delivered_units > 0,
      fill_rate = ifelse(sla_eval, delivered_units/demand_units, NA_real_),
      sla_fail = ifelse(sla_eval, fill_rate < service_level, NA)
    ) %>%
    group_by(customer, sku) %>%
    summarise(
      weeks_eval = sum(sla_eval, na.rm=TRUE),          # số tuần thật sự evaluate
      sla_fail_rate = ifelse(weeks_eval>0,
                             mean(sla_fail, na.rm=TRUE),
                             0),                      # nếu chưa eval tuần nào thì coi như ok
      .groups="drop"
    ) %>%
    mutate(
      sla_bad = weeks_eval >= rules$min_weeks_obs &
        sla_fail_rate > rules$sla_fail_rate_max
    )
  
  #3) Slow mover (demand thấp so với median của customer)
  slow_stat <- base %>%
    left_join(##Update 0.2.0
      hist %>% group_by(customer, sku) %>%
        summarise(ever_delivered = any(delivered_units>0), .groups="drop"),
      by=c("customer","sku")
    ) %>%
    group_by(customer) %>%
    mutate(
      med_dem = median(demand_week_pieces[ base_active ], na.rm=TRUE),
      slowmover = base_active & ever_delivered &
        demand_week_pieces < rules$slowmover_ratio * med_dem
    ) %>%
    ungroup() %>%
    select(customer, sku, slowmover)
  
  #4) combine rules
  #Update 0.2.3 contribution margin avg per unit from history
  cm_hist <- hist %>%
    mutate(cm_unit = sales_price - basic_sales_price,
           cm_total = cm_unit * delivered_units) %>%
    group_by(customer, sku) %>%
    summarise(
      cm_unit_avg  = mean(cm_unit[delivered_units > 0], na.rm=TRUE),
      cm_total_sum = sum(cm_total, na.rm=TRUE),
      vol_sum      = sum(delivered_units, na.rm=TRUE),
      .groups="drop"
    )
  out <- base %>%
    left_join(margin_stat, by=c("customer","sku")) %>%
    left_join(sla_stat, by=c("customer","sku")) %>%
    left_join(slow_stat, by=c("customer","sku")) %>%
    left_join(cm_hist,     by=c("customer","sku")) %>%
    group_by(customer) %>% #Update 0.2.2 rank theo customer
    mutate(
      margin_bad = replace_na(margin_bad, FALSE),
      sla_bad    = replace_na(sla_bad, FALSE),
      slowmover  = replace_na(slowmover, FALSE),
      cm_total_sum = replace_na(cm_total_sum, -Inf),
      vol_sum      = replace_na(vol_sum, 0),
      # core chỉ xét SKU đủ volume/CM
      eligible_core = vol_sum >= 1000 & cm_total_sum > 0,  #0.2.3tune ngưỡng
      #Tune rule: keep core SKUs #Update 0.2.1 floor rule
      rank_core = rank(-ifelse(eligible_core, cm_total_sum, -Inf),
                       ties.method="first"),
      keep_core = rank_core <= 2, # giữ top X SKU / customer
      keep_core = replace_na(keep_core, FALSE), ##Update 0.2.3 make more robust
      # active logic:
      # - phải base_active trước
      # - nếu bad theo margin hoặc SLA hoặc slowmover => tắt
      active_auto = base_active & (keep_core | !(margin_bad | sla_bad | slowmover)
      )) %>% ungroup() %>% 
    transmute(customer, sku, active = active_auto)
  #Adjustment 2: hard guarantee min 2 SKU active / customer #Update 0.2.2
  out <- out %>%
    left_join(base %>% select(customer, sku, demand_week_pieces), by=c("customer","sku")) %>%
    group_by(customer) %>%
    mutate(
      n_active = sum(active, na.rm=TRUE),
      rank_dem = rank(-demand_week_pieces, ties.method="first"),
      active = ifelse(n_active < 2, rank_dem <= 2, active)
    ) %>%
    ungroup() %>%
    select(customer, sku, active)
  #5) apply manual override if provided
  if (!is.null(assortment_prior) && nrow(assortment_prior)>0){
    out <- out %>%
      left_join(
        assortment_prior %>%
          mutate(
            customer = norm_customer(customer),
            sku = stringr::str_squish(sku)
          ) %>%
          select(customer, sku, active_prior=active),
        by=c("customer","sku")
      ) %>%
      mutate(
        active = ifelse(!is.na(active_prior), active_prior, active)
      ) %>%
      select(customer, sku, active)
  }
  
  out
}
##Update 0.2.1 Add helper refresh CI purchasing
refresh_ci_purch <- function(supplier_params, CIpur_lookup){
  pur_ci_key <- c("vendor","quality","delivery_window","delivery_reliability",
                  "trade_unit","payment_term")
  
  supplier_params_clean <- supplier_params %>%
    mutate(
      vendor = as.character(vendor),
      quality = str_to_lower(quality),
      delivery_window = str_to_lower(delivery_window),
      trade_unit = str_to_lower(trade_unit),
      payment_term = as.character(payment_term),
      delivery_reliability = as.numeric(delivery_reliability)
    ) 
  #Update 0.2.3 nếu đã có ci_purch sẵn thì bỏ đi trước khi join để tránh .x/.y
  if ("ci_purch" %in% names(supplier_params_clean)) {
    supplier_params_clean <- supplier_params_clean %>% select(-ci_purch)
  }
  supplier_params_clean %>% left_join(CIpur_lookup, by=pur_ci_key) %>%
    mutate(ci_purch = replace_na(ci_purch, 1))
}
##Update 0.2.1 Add refresh CI sale
refresh_ci_sales <- function(sales_decision_cp, CIsale_lookup) {
  sales_ci_key <- c("customer","promotional_pressure","order_deadline","service_level",
                    "trade_unit","payment_term","promotion_horizon","shelf_life")
  sales_decision_clean <-sales_decision_cp %>%
    mutate(
      customer             = norm_customer(customer),
      promotional_pressure = str_to_lower(promotional_pressure),
      trade_unit           = str_to_lower(trade_unit),
      promotion_horizon    = str_to_lower(promotion_horizon),
      order_deadline       = str_replace_all(order_deadline, "\\s+",""),
      payment_term         = as.character(payment_term),
      shelf_life           = as.character(shelf_life),
      service_level        = as.numeric(service_level)
    ) 
  #Update 0.2.3 nếu đã có ci_promised sẵn thì bỏ đi trước khi join để tránh .x/.y
  if ("ci_promised" %in% names(sales_decision_clean)) {
    sales_decision_clean <- sales_decision_clean %>% select(-ci_promised)
  }
  sales_decision_clean %>% left_join(CIsale_lookup, by = sales_ci_key) %>%
    mutate(ci_promised = replace_na(ci_promised, 1))
}
##Phase 3 helper
#Sale agreement
apply_sales_agreement_customer <- function(sales_decision_cp, CIsale_lookup, idx_by_customer){
  # idx_by_customer: named int vector, names = customer
  
  sale_choices <- CIsale_lookup %>%
    mutate(sale_idx = row_number())
  
  out <- sales_decision_cp %>%
    mutate(customer = norm_customer(customer),
           sku = stringr::str_squish(sku)) %>%
    select(customer, sku) %>%
    left_join(
      tibble(customer = names(idx_by_customer),
             sale_idx = as.integer(idx_by_customer)),
      by="customer"
    ) %>%
    mutate(sale_idx = replace_na(sale_idx, 1L)) %>%
    left_join(
      sale_choices %>%
        select(sale_idx, promotional_pressure, order_deadline, service_level,
               trade_unit, payment_term, promotion_horizon, shelf_life, ci_promised),
      by="sale_idx"
    ) %>%
    select(-sale_idx) %>%
    mutate(
      # đảm bảo type sạch cho engine
      promotional_pressure = str_to_lower(promotional_pressure),
      trade_unit = str_to_lower(trade_unit),
      promotion_horizon = str_to_lower(promotion_horizon),
      order_deadline = stringr::str_replace_all(order_deadline, "\\s+",""),
      payment_term = as.character(payment_term),
      shelf_life = as.character(shelf_life),
      service_level = as.numeric(service_level),
      ci_promised = as.numeric(ci_promised)
    )
  
  out
}
#Purchasing agrewement
apply_purch_agreement_component <- function(supplier_params_default, CIpur_lookup, idx_by_component){
  
  # vendor base theo component
  base_vendor_tbl <- supplier_params_default %>%
    select(component, base_vendor = vendor)
  
  out_list <- lapply(names(idx_by_component), function(comp){
    
    base_vendor <- base_vendor_tbl$base_vendor[match(comp, base_vendor_tbl$component)]
    
    choices_comp <- CIpur_lookup %>%
      filter(vendor == base_vendor) %>%
      mutate(pur_idx = row_number())
    
    # nếu vendor này không có row nào trong lookup -> fallback idx=1 & ci=1
    if (nrow(choices_comp) == 0){
      return(
        tibble(
          component = comp,
          vendor = base_vendor,
          quality="high",
          delivery_window="1day",
          delivery_reliability=95,
          trade_unit="pallet",
          payment_term="4",
          ci_purch=1
        )
      )
    }
    
    pur_i <- as.integer(idx_by_component[[comp]])
    pur_i <- pmin(pmax(pur_i, 1L), nrow(choices_comp))
    
    row_pick <- choices_comp[pur_i,]
    
    tibble(
      component = comp,
      vendor    = row_pick$vendor,
      quality   = row_pick$quality,
      delivery_window = row_pick$delivery_window,
      delivery_reliability = row_pick$delivery_reliability,
      trade_unit = row_pick$trade_unit,
      payment_term = row_pick$payment_term,
      ci_purch  = row_pick$ci_purch
    )
  })
  
  out <- bind_rows(out_list)
  
  # hard constraint reliability như bạn có
  out <- out %>%
    mutate(
      delivery_reliability =
        ifelse(component %in% c("vitamin_c","pet"),
               pmax(delivery_reliability, 95),
               delivery_reliability)
    )
  
  out
}
#safty helper phase 3
safe_key <- function(x){
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x
}
# =========================
# 4) DECISIONS: BUILD SALES & PURCH PARAMS DEFAULT
# =========================
# =========================
# 4) DECISIONS DEFAULT (SALES + PURCHASING)
# =========================

#~sales decision default
make_sales_decisions <- function(customers, skus){
  expand_grid(customer=customers, sku=skus) %>%
    mutate(
      promotional_pressure="middle",
      order_deadline="14pm",
      service_level=95,           # numeric
      trade_unit="box",
      payment_term="4",           # character
      promotion_horizon="short",
      shelf_life="50"             # <-- để match lookup thì dùng 50/65/80; Phase1 ok
    )
}

sales_decision_cp <- make_sales_decisions(
  customers = unique(customer_master$customer),
  skus      = unique(benchmark_demand$sku)##beta 0.0.2
) 
sales_decision_cp <-refresh_ci_sales(sales_decision_cp, CIsale_lookup)

# purchasing supplier_params default
# Phase 1  chưa scrape decision purchasing param => set baseline
supplier_params_default <- tibble(
  component = c("Pack","PET","Orange","Mango","Vitamin C"),
  vendor    = c("Mono Packaging Materials","Trio PET PLC","Miami Oranges",
                "NO8DO Mango","Seitan Vitamins"),
  quality   = "high",
  delivery_window = "1day",
  delivery_reliability = 95,    # numeric (match lookup!)
  trade_unit = "pallet",
  payment_term = "4"
) %>%
  mutate(
    quality = str_to_lower(quality),
    delivery_window = str_to_lower(delivery_window),
    trade_unit = str_to_lower(trade_unit),
    payment_term = as.character(payment_term),
    delivery_reliability = as.numeric(delivery_reliability),
    component = norm_component(component)
  )
#Update 0.2.1
supplier_params_default <-refresh_ci_purch(supplier_params_default, CIpur_lookup)
# =========================
# 5) PHASE 1 FUNCTIONS
# =========================

sales_demand_week <- function(week, state, sales_decision_cp, sales_constants, exo,
                              assortment_cp=NULL){
  bench <- sales_constants$observed$benchmark_demand
  
  vf <- exo$volume_factor
  if (is.null(vf)) vf <- tibble(customer=unique(bench$customer), volume_factor=1)
  
  dem <- bench %>%
    left_join(vf, by="customer") %>% 
    mutate(
      volume_factor = replace_na(volume_factor, 1),               # <<<<<< FIX
      demand_week_pieces = as.numeric(demand_week_pieces)         # <<<<<< FIX an toàn
    ) %>%
    left_join(sales_decision_cp %>% select(customer, sku, promotional_pressure, ci_promised),
              by=c("customer","sku")) %>%
    mutate(
      promotional_pressure = replace_na(promotional_pressure,"none"),
      promo_uplift = map_dbl(promotional_pressure, get_promo_uplift),
      demand_units = demand_week_pieces * volume_factor * (1 + promo_uplift),
      ci_promised  = replace_na(ci_promised,1))
  #Update 0.1.6 NEW: category management 
  if (!is.null(assortment_cp) && nrow(assortment_cp)>0){
    dem <- dem %>%
      left_join(assortment_cp, by=c("customer","sku")) %>%
      mutate(active = ifelse(is.na(active), TRUE, active),
             demand_units = ifelse(active, demand_units, 0))
  }
  
  dem %>%select(customer, sku, demand_units, ci_promised, promotional_pressure, volume_factor)
}

planning_week <- function(week, state, demand_w, decisions_sc, sc_constants){##Big update beta 0.1.3
  
  fg_stock <- state$fg_stock %>% select(sku, fg_units=units)
  next_tbl <- state$fg_next_prod %||% tibble(sku=unique(demand_w$sku), next_prod_week=1L)
  
  # decisions
  ssfg_w <- decisions_sc$fg_safety_stock_w
  if (is.null(ssfg_w)){
    ssfg_w <- setNames(rep(0, length(unique(demand_w$sku))), unique(demand_w$sku))
  }
  
  interval_d <- decisions_sc$fg_production_interval_d
  if (is.null(interval_d)){
    interval_d <- setNames(rep(5, length(unique(demand_w$sku))), unique(demand_w$sku))
  }
  
  wd_pw <- sc_constants$time$working_days_per_week %||% 5
  
  demand_sku <- demand_w %>%
    group_by(sku) %>%
    summarise(demand_units = sum(demand_units), .groups="drop")
  
  plan <- demand_sku %>%
    left_join(fg_stock, by="sku") %>%
    left_join(next_tbl, by="sku") %>%
    mutate(
      fg_units = replace_na(fg_units, 0),
      next_prod_week = replace_na(next_prod_week, 1L),
      
      interval_days = interval_d[sku] %>% replace_na(5),
      interval_days = pmin(25, pmax(1, interval_days)),
      k_weeks = interval_days / wd_pw,
      cadence_weeks = pmax(1L, ceiling(k_weeks)),  # how many weeks between runs
      
      # only allow run if week >= next_prod_week
      allowed_to_run = week >= next_prod_week,
      
      ss_factor = ssfg_w[sku] %>% replace_na(0),
      target_ss_units = ss_factor * demand_units,
      
      # lot sizing: produce for k_weeks worth of demand when you run
      lot_units = demand_units * k_weeks,
      
      planned_units = ifelse(
        allowed_to_run,
        pmax(0, lot_units + target_ss_units - fg_units),
        0
      )
    ) %>%
    select(sku, demand_units, planned_units, cadence_weeks, allowed_to_run)
  
  plan
}

rm_replenishment_week <- function(week, state, plan_w, decisions_sc,
                                  supplier_params, df_sku, df_component,
                                  purchasing_constants,operations_constants,
                                  sc_constants){
  bom_long <- df_sku %>%
    select(sku, starts_with("rm_")) %>%
    pivot_longer(
      cols = starts_with("rm_"),
      names_to = "component_raw",
      values_to = "qty_per_unit"
    ) %>%
    mutate(
      component = str_replace(component_raw, "^rm_", ""),
      qty_per_unit = replace_na(as.numeric(qty_per_unit), 0),
      component = case_when(
        component == "pack1liter" ~ "pack",
        component == "vitaminc"   ~ "vitamin_c",
        TRUE ~ component
      ),
      component = norm_component(component)
    ) %>%
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
  ##update 0.2.1
  #base reorder need (weeks-of-demand)
  reorder_tbl <- rm_need %>%
    left_join(rm_stock, by="component") %>%
    mutate(
      rm_units=replace_na(rm_units,0),
      target_ss_units = ssrm_w[component] * rm_week_need,
      lot_units = lot_w[component] * rm_week_need,
      order_units_raw = ifelse(rm_units < target_ss_units, lot_units, 0)
    ) %>%
    # join supplier params for trade_unit + vendor + ci
    left_join(supplier_params %>% 
                select(component, ci_purch, vendor, trade_unit),
              by="component") %>%
    mutate(
      trade_unit = str_to_lower(replace_na(trade_unit, "pallet"))
    )
  
  #enforce trade unit multiple (InfoCenter)
  logi_const <- sc_constants$logistics %||% list(
    drum_liters=250, ibc_liters=1000, tanktruck_liters=30000
  )
  
  reorder_tbl <- reorder_tbl %>%
    mutate(
      trade_size_units = case_when(
        trade_unit == "drum" ~ logi_const$drum_liters,
        trade_unit == "ibc"  ~ logi_const$ibc_liters,
        trade_unit %in% c("tank","tanktruck") ~ logi_const$tanktruck_liters,
        TRUE ~ 1  # pallet/pack/PET giữ nguyên theo BOM units
      ),
      order_units = ifelse(
        order_units_raw > 0 & trade_size_units > 1,
        ceiling(order_units_raw / trade_size_units) * trade_size_units,
        order_units_raw
      )
    )
  
  #price & cost
  reorder_tbl <- reorder_tbl %>%
    left_join(df_component %>% select(component, basic_price), by="component") %>%
    mutate(
      basic_price = replace_na(basic_price, 0),
      ci_purch=replace_na(ci_purch,1),
      purchase_price = basic_price * ci_purch,
      purchase_cost_total = order_units * purchase_price
    )
  
  #lead time
  sup_master <- purchasing_constants$supplier_master %>%
    rename(vendor = supplier) %>%
    mutate(vendor = as.character(vendor))
  
  reorder_tbl <- reorder_tbl %>%
    mutate(vendor = as.character(vendor)) %>%
    left_join(sup_master %>% select(vendor, lead_time_days), by="vendor") %>%
    mutate(
      lead_time_days = replace_na(lead_time_days, 0),
      lead_time_w = pmax(1, ceiling(lead_time_days / 5))
    )
  
  #orders (cost now)
  orders <- reorder_tbl %>%
    filter(order_units > 0) %>%
    transmute(
      week,
      component,
      vendor,
      order_qty_units = order_units,
      purchase_price,
      purchase_cost_total,
      trade_unit
    )
  
  #tank yard / inbound handling cost (book at order week)
  tank_const <- operations_constants$tank_yard
  
  orders <- orders %>%
    mutate(
      inbound_tank_cost = case_when(
        trade_unit %in% c("tank", "tanktruck") ~ (
          tank_const$intake_handling_per_delivery +
            tank_const$delivery_to_TFC +
            tank_const$tank_use_cost_week
        ),
        trade_unit %in% c("ibc") ~ (
          tank_const$intake_handling_per_delivery +
            tank_const$delivery_to_TFC
        ),
        trade_unit %in% c("drum") ~ (
          tank_const$intake_handling_per_delivery
        ),
        TRUE ~ 0
      )
    )
  # fix receipts pipeline
  lead_tbl <- reorder_tbl %>%
    group_by(component, vendor) %>%
    summarise(lead_time_w = max(lead_time_w), .groups="drop")
  receipts_w <- orders %>%
    left_join(lead_tbl, by=c("component","vendor")) %>%
    transmute(
      week_order = week,
      component,
      vendor,
      order_qty_units,
      purchase_price,
      purchase_cost_total,
      trade_unit,
      eta_week = week + lead_time_w
    )
  list(orders = orders, receipts_w = receipts_w)
}

execute_production_week <- function(week, state, plan_w, rm_receipts_w, df_sku,##big update beta 0.0.7 
                                    operations_constants, decisions_ops){ 
  #~BOM long
  bom_long <- df_sku %>%
    select(sku, starts_with("rm_")) %>%
    pivot_longer(
      cols = starts_with("rm_"),
      names_to = "component_raw",
      values_to = "qty_per_unit"
    ) %>%
    mutate(
      component = str_replace(component_raw, "^rm_", ""),
      qty_per_unit = replace_na(as.numeric(qty_per_unit), 0),
      component = case_when(
        component == "pack1liter" ~ "pack",
        component == "vitaminc"   ~ "vitamin_c",
        TRUE ~ component
      ),
      component = norm_component(component)
    ) %>%
    select(sku, component, qty_per_unit) 
  #~RM available
  rm_avail <- state$rm_stock %>%
    left_join(
      rm_receipts_w %>% group_by(component) %>%
        summarise(received=sum(received_qty_units), .groups="drop"),
      by="component"
    ) %>%
    mutate(received=replace_na(received,0),
           avail_units = units + received) %>%
    select(component, avail_units)
  rm_need_for_plan <- plan_w %>%
    left_join(bom_long, by="sku") %>%
    mutate(need_units = planned_units * qty_per_unit) %>%
    group_by(component) %>%
    summarise(need_units=sum(need_units, na.rm=TRUE), .groups="drop") %>%
    left_join(rm_avail, by="component") %>%
    mutate(avail_units=replace_na(avail_units,0),
           need_units = replace_na(need_units,0))
  ##fix not need rm not mean that =0, robust update beta 0.0.8
  #-ratio RM robust
  ratios_tbl <- rm_need_for_plan %>%
    mutate(ratio = ifelse(need_units > 0, avail_units / need_units, NA_real_))
  
  if (all(is.na(ratios_tbl$ratio))) {
    ratio_rm <- 1   # không cần RM => không cắt
  } else {
    ratio_rm <- min(ratios_tbl$ratio, na.rm = TRUE)
    ratio_rm <- ifelse(is.finite(ratio_rm), pmin(1, ratio_rm), 0)
  }
  plan_after_rm <- plan_w %>%
    mutate(planned_units_rm = planned_units * ratio_rm)
  #~liters required 
  liters_req <- plan_after_rm %>%
    left_join(df_sku %>% select(sku, liters_per_pack), by="sku") %>%
    mutate(
      liters_per_pack = replace_na(liters_per_pack, 0),
      liters_required = planned_units_rm * liters_per_pack
    )
  total_liters_required <- sum(liters_req$liters_required, na.rm=TRUE)
  #~MIXER capacity constraint
  mix_const <- operations_constants$mixer
  ##enforece min batch #update beta 0.0.9
  tech_min  <- mix_const$technical_min_liters %||% 0
  if (total_liters_required > 0 && total_liters_required < tech_min){
    scale_up <- tech_min / total_liters_required
    liters_req <- liters_req %>%
      mutate(liters_required = liters_required * scale_up,
             planned_units_rm = planned_units_rm * scale_up)
    total_liters_required <- tech_min
  }
  # số batch cần chạy (mỗi batch tối đa max_capacity_liters)
  n_batches <- ifelse(total_liters_required > 0,
                      ceiling(total_liters_required / mix_const$max_capacity_liters),
                      0)
  mixer_hours_required <- n_batches * (mix_const$operating_time_hours + mix_const$cleaning_time_hours)
  # available mixer hours/week: dùng max_internal_hours (logic TFC)
  mixer_hours_available <- operations_constants$labor_production$max_internal_hours %||% 168
  ratio_mixer <- ifelse(mixer_hours_required > 0,
                        pmin(1, mixer_hours_available / mixer_hours_required),
                        1)
  plan_after_mixer <- liters_req %>%
    mutate(planned_units_after_mixer = planned_units_rm * ratio_mixer,
           liters_after_mixer        = planned_units_after_mixer * liters_per_pack)
  #~BOTTLING capacity + outsourcing
  op_const <- operations_constants$bottling_line
  labor_const <- operations_constants$labor_production
  bottling_rate_lph <- op_const$capacity_lph
  if (is.null(bottling_rate_lph) || bottling_rate_lph <= 0) bottling_rate_lph <- 3100
  hours_required <- sum(plan_after_mixer$liters_after_mixer, na.rm=TRUE) / bottling_rate_lph
  startup_loss_pct <- op_const$startup_loss_pct %||% 0.10
  hours_required <- hours_required / (1 - startup_loss_pct)
  ##Update 0.1.8 Add FTE effect
  op_const   <- operations_constants$bottling_line
  labor_const<- operations_constants$labor_production
  
  bottling_rate_lph <- op_const$capacity_lph
  if (is.null(bottling_rate_lph) || bottling_rate_lph <= 0) bottling_rate_lph <- 3100
  
  hours_required <- sum(plan_after_mixer$liters_after_mixer, na.rm=TRUE) / bottling_rate_lph
  startup_loss_pct <- op_const$startup_loss_pct %||% 0.10
  hours_required <- hours_required / (1 - startup_loss_pct)
  
  # shifts -> giới hạn nội bộ theo ca
  num_shifts <- decisions_ops$num_shifts %||% 1
  shift_hours_tbl <- op_const$shift_hours
  available_hours_shift <- shift_hours_tbl[as.character(num_shifts)]
  available_hours_shift <- ifelse(is.na(available_hours_shift), 40, available_hours_shift)
  # FTE capacity (40h/FTE)
  n_fte <- decisions_ops$n_fte_bottling %||% op_const$n_operators
  fte_cap_hours <- n_fte * labor_const$fte_hours_per_week  # 40h/FTE * n_fte
  # max internal hours theo game
  max_int_hours <- labor_const$max_internal_hours %||% 168
  # split hours
  internal_fte_hours  <- pmin(hours_required, fte_cap_hours)
  internal_flex_hours <- pmin(
    pmax(0, hours_required - fte_cap_hours),
    pmax(0, max_int_hours - fte_cap_hours)
  )
  outsourced_hours <- pmax(0, hours_required - max_int_hours)
  # internal hours không vượt shift capacity
  internal_hours_total <- internal_fte_hours + internal_flex_hours
  internal_hours_total <- pmin(internal_hours_total, available_hours_shift)
  outsource_pct <- ifelse(hours_required > 0, outsourced_hours / hours_required, 0)
  produced <- plan_after_mixer %>%
    transmute(
      week=week, sku,
      planned_units,
      planned_units_rm,
      planned_units_after_mixer,
      produced_units   = planned_units_after_mixer * (1 - outsource_pct),
      outsourced_units = planned_units_after_mixer * outsource_pct,
      liters_required  = liters_after_mixer
    )
  #~RM used (chỉ tính trên produced_units nội bộ)
  rm_used <- produced %>%
    left_join(bom_long, by="sku") %>%
    mutate(used_units = produced_units * qty_per_unit) %>%
    group_by(component) %>%
    summarise(used_units=sum(used_units,na.rm=TRUE),.groups="drop") %>%
    mutate(week=week)
  
  #costs
  fte_cost_per_hour_one  <- op_const$operator_cost_per_year / (52 * labor_const$fte_hours_per_week)
  fte_cost_per_hour_line <- fte_cost_per_hour_one * n_fte
  fte_cost  <- internal_fte_hours  * fte_cost_per_hour_line
  flex_cost <- internal_flex_hours * labor_const$flex_cost_per_hour
  out_cost  <- outsourced_hours    * labor_const$flex_cost_per_hour * labor_const$outsourcing_factor
  bottling_internal_cost  <- fte_cost + flex_cost
  bottling_outsource_cost <- out_cost
  bottling_fixed_week     <- op_const$fixed_annual_cost / 52
  
  #mixer cost
  mixer_fixed_week <- mix_const$fixed_annual_cost / 52
  mixer_var_cost   <- mixer_hours_required * mix_const$variable_cost_per_hour
  production_cost_total <- bottling_internal_cost +
    bottling_outsource_cost +
    bottling_fixed_week +
    mixer_fixed_week +
    mixer_var_cost
  # phân bổ cost theo liters
  #  Robust allocation per week (fix over-allocate) update beta 0.0.8
  produced <- produced %>%
    mutate(
      liters_required = replace_na(liters_required, 0),
      liters_required = pmax(0, liters_required)   # chặn âm nếu có
    )
  total_liters_week <- sum(produced$liters_required, na.rm = TRUE)
  produced <- produced %>%
    mutate(
      liters_share = ifelse(total_liters_week > 0,
                            liters_required / total_liters_week,
                            0),
      production_cost = production_cost_total * liters_share
    )
  # FINAL RESCALE SAFETY (never over/under allocate) update beta 0.0.8
  sum_alloc <- sum(produced$production_cost, na.rm = TRUE)
  if (is.finite(sum_alloc) && sum_alloc > 0) {
    produced <- produced %>%
      mutate(production_cost = production_cost * (production_cost_total / sum_alloc))
  }
  # update 0.0.8 : idle cost handling -----
  idle_cost <- ifelse(total_liters_week <= 0, production_cost_total, 0)
  # nếu idle thì production_cost của SKU sẽ vẫn = 0 (đúng)
  # cost không mất đi mà nằm ở idle_cost
  production_kpis <- tibble(
    week=week,
    ratio_rm=ratio_rm,
    ratio_mixer=ratio_mixer,
    hours_required=hours_required,
    available_hours=available_hours_shift,
    internal_hours=internal_hours_total,
    outsourced_hours=outsourced_hours,
    outsourced_pct=outsource_pct,
    mixer_hours_required=mixer_hours_required,
    mixer_hours_available=mixer_hours_available,
    production_cost_total=production_cost_total,
    idle_cost = idle_cost
  )
  list(production=produced, rm_used=rm_used, kpis=production_kpis)
}

fulfill_demand_week <- function(week, state, demand_w, produced_w, df_sku,
                                sales_decision_cp, customer_master,
                                shortage_rule="proportional"){
  #-Available FG
  fg_stock <- state$fg_stock %>% select(sku, fg_units=units)
  avail <- fg_stock %>%
    full_join(produced_w %>% select(sku, produced_units), by="sku") %>%
    mutate(
      fg_units=replace_na(fg_units,0),
      produced_units=replace_na(produced_units,0),
      available_units=fg_units+produced_units
    )
  #add order_deadline from decision (để FCFS) #update beta 0.0.9
  dl_tbl <- sales_decision_cp %>%
    select(customer, sku, order_deadline) %>%
    mutate(
      order_deadline = replace_na(order_deadline,"99pm"),
      order_deadline_num = readr::parse_number(order_deadline)
    )
  #add customer priority #update beta 0.0.9
  pri_tbl <- customer_master %>%
    mutate(
      maturity_score = case_when(
        maturity=="high" ~ 3,
        maturity=="medium" ~ 2,
        maturity=="low" ~ 1,
        TRUE ~ 1
      ),
      vfm_score = ifelse(value_for_money, 1, 0),
      priority_score = maturity_score*10 + vfm_score*5 + market_share*100 ##priority criteria
    ) %>%
    select(customer, priority_score)
  demand_aug <- demand_w %>%
    left_join(avail, by="sku") %>%
    left_join(dl_tbl, by=c("customer","sku")) %>%
    left_join(pri_tbl, by="customer") %>%
    mutate(
      available_units = replace_na(available_units, 0),
      order_deadline_num = replace_na(order_deadline_num, 99),
      priority_score = replace_na(priority_score, 0)
    )
  #~Allocate theo rule pick
  demand_alloc <- allocate_shortage(demand_aug, rule=shortage_rule)
  
  sales_exec <- demand_alloc %>%
    left_join(df_sku %>% select(sku, basic_sales_price, units_per_pallet), by="sku") %>% #Update beta 0.0.9
    mutate(
      units_per_pallet = ifelse(is.na(units_per_pallet) | units_per_pallet <= 0, NA, units_per_pallet),
      attained_ci=ci_promised,
      sales_price = basic_sales_price * attained_ci,
      revenue = delivered_units * sales_price,
      pallets_shipped = ifelse(is.na(units_per_pallet), 0, delivered_units / units_per_pallet),
      penalty_rate = 0.2,  #có thể đổi
      penalty_factor=1,
      penalty_cost = backorder_units * basic_sales_price * penalty_rate* penalty_factor,
      week = week
    ) %>%
    select(week, customer, sku, demand_units, delivered_units, attained_ci,
           backorder_units, sales_price, revenue, pallets_shipped, penalty_cost)
  
  return(sales_exec)
}

update_state_week <- function(state, receipts_w, rm_used_w, produced_w, sales_exec, df_sku){
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
  
  # FG #update phase 2
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
      age_w=ifelse(units > 0, replace_na(age_w, 0) + 1, 0)
    ) %>% left_join(
      df_sku %>% select(sku, shelf_life_w),
      by="sku"
    ) %>%
    mutate(
      shelf_life_w = replace_na(shelf_life_w, Inf),
      
      # nếu quá shelf life thì obsolete
      obsolete_units = ifelse(age_w > shelf_life_w, units, 0),
      units          = ifelse(age_w > shelf_life_w, 0, units),
      age_w          = ifelse(age_w > shelf_life_w, 0, age_w)
    ) %>%
    select(sku, units, age_w, obsolete_units)
  
  state$rm_stock <- rm_stock
  state$fg_stock <- fg_stock
  state
}

finance_aggregate_round <- function(flows, finance_constants, operations_constants){ #fix patch beta 0.0.1
  revenue <- sum(flows$sales$revenue, na.rm=TRUE)
  purchase_costs <- sum(flows$purchasing$purchase_cost_total, na.rm=TRUE)
  tank_yard_costs <- sum(flows$purchasing$inbound_tank_cost, na.rm = TRUE) ##upadte 0.1.8
  prod_costs_alloc <- sum(flows$production$production_cost, na.rm=TRUE)
  prod_costs_idle  <- sum(flows$operations$idle_cost, na.rm=TRUE)#update beta 0.0.8
  prod_costs <- prod_costs_alloc + prod_costs_idle   # update beta 0.0.8
  warehouse_costs <- sum(flows$warehousing$fg_location_cost, na.rm=TRUE) + ##update beta 0.0.7
    sum(flows$warehousing$fg_overflow_cost, na.rm=TRUE)+
    sum(flows$warehousing$rm_location_cost, na.rm=TRUE) +
    sum(flows$warehousing$rm_overflow_cost, na.rm=TRUE) +
    sum(flows$warehousing$rm_labor_cost, na.rm=TRUE)
  scrap_costs <- sum(flows$warehousing$scrap_cost, na.rm=TRUE) ##update beta 0.0.7
  holding_costs <- sum(flows$inventory$rm_holding_cost, na.rm=TRUE) +
    sum(flows$inventory$fg_holding_cost, na.rm=TRUE)
  penalty_costs <- sum(flows$sales$penalty_cost, na.rm=TRUE)
  admin_costs <- sum(flows$distribution$admin_cost, na.rm=TRUE)
  pi_costs_week <- 0 ##Update beta 0.1.0
  if (nrow(flows$pi) > 0){
    pi_costs_week <- sum(flows$pi$total_costs_year / 52, na.rm=TRUE)
  }
  cogs <- purchase_costs + prod_costs + holding_costs + penalty_costs+
    warehouse_costs + scrap_costs + admin_costs+ pi_costs_week+ tank_yard_costs
  gross_margin <- revenue - cogs
  
  dist_costs <- sum(purrr::map_dbl(
    flows$sales$pallets_shipped,
    ~calc_distribution_cost(.x, finance_constants=finance_constants)
  ), na.rm=TRUE)
  
  operating_profit <- gross_margin - dist_costs
  
  # Gom CAPEX lại
  investment_total <- finance_constants$investment$building_fixed +
    operations_constants$mixer$investment_cost +
    operations_constants$bottling_line$investment_cost
  
  ROI_pred <- operating_profit / investment_total
  
  list(
    ROI_pred=ROI_pred,
    revenue=revenue,
    gross_margin=gross_margin,
    operating_profit=operating_profit,
    distribution_costs=dist_costs,
    investment_total=investment_total,
    warehouse_costs=warehouse_costs,##update beta 0.0.7
    scrap_costs=scrap_costs,##update beta 0.0.7
    admin_costs, #update beta 0.0.7
    pi_costs_week = pi_costs_week,  #update beta 0.1.0
    tank_yard_costs = tank_yard_costs,   ##update 0.1.8
    cogs=cogs,
    purchase_costs=purchase_costs,
    production_costs=prod_costs, #alloc + idle
    holding_costs=holding_costs,
    penalty_costs=penalty_costs
  )
}
options(error = rlang::entrace)
# =========================
# 6) ENGINE ROUND (PHASE 1)
# =========================

engine_round <- function(state0, decisions, constants, lookups, exogenous, n_weeks=26){
  
  flows <- make_empty_flows()
  state <- state0
  ##Update 0.2.3 FIX CI REFRESH
  #refresh purchasing CI every round in case optimizer changes params
  if (!is.null(decisions$purchasing$supplier_params)) {
    decisions$purchasing$supplier_params <- refresh_ci_purch(
      decisions$purchasing$supplier_params,
      lookups$CIpur_lookup %||% CIpur_lookup
    )
  }
  # refresh sales CI every round in case optimizer changes agreements
  if (!is.null(decisions$sales$customer_product_level)) {
    decisions$sales$customer_product_level <- refresh_ci_sales(
      decisions$sales$customer_product_level,
      lookups$CIsale_lookup %||% CIsale_lookup
    )
  }
  #--
  for (w in 1:n_weeks){
    
    demand_w <- sales_demand_week(
      week=w, state=state,
      sales_decision_cp = decisions$sales$customer_product_level,
      sales_constants = constants$sales,
      exo = exogenous,
      assortment_cp = decisions$sales$assortment_cp   ##Update 0.1.6
    )
    
    #PATCH Upadte 0.1.5 frozen production
    frozen_w_raw <- decisions$supply_chain$frozen_period_weeks
    if (is.null(frozen_w_raw)) frozen_w_raw <- 1L
    frozen_w <- as.integer(pmin(6L, pmax(1L, frozen_w_raw)))
    
    if (is.null(state$prod_frozen_pipeline)) {
      state$prod_frozen_pipeline <- tibble()
    }
    
    # 1) Nếu tuần này đã có frozen plan -> dùng lại
    if (nrow(state$prod_frozen_pipeline) > 0 &&
        any(state$prod_frozen_pipeline$week == w)) {
      
      plan_w <- state$prod_frozen_pipeline %>%
        filter(week == w) %>%
        left_join(
          demand_w %>%
            group_by(sku) %>%
            summarise(demand_units = sum(demand_units), .groups="drop"),
          by="sku"
        ) %>%
        mutate(
          demand_units = replace_na(demand_units, 0),
          cadence_weeks = NA_integer_,
          allowed_to_run = planned_units > 0
        ) %>%
        select(sku, demand_units, planned_units, cadence_weeks, allowed_to_run)
      
    } else {
      
      # 2) Chưa có plan -> tính mới
      plan_w <- planning_week(
        week=w, state=state, demand_w=demand_w,
        decisions_sc = decisions$supply_chain,
        sc_constants = constants$supply_chain
      )
      
      # 3) Khoá plan cho frozen_w tuần tới
      frozen_future <- plan_w %>%
        select(sku, planned_units) %>%
        tidyr::crossing(week = w:(w + frozen_w - 1L))
      
      # chỉ thêm những tuần chưa có trong pipeline
      state$prod_frozen_pipeline <- bind_rows(state$prod_frozen_pipeline, frozen_future) %>%
        arrange(week) %>%
        distinct(week, sku, .keep_all = TRUE)
      
      # dọn pipeline quá khứ
      state$prod_frozen_pipeline <- state$prod_frozen_pipeline %>%
        filter(week >= w)
    }
    # ===== END PATCH =====
    
    ##phase 2 rm no receipt yet
    rm_rep <- rm_replenishment_week(
      week=w, state=state, plan_w=plan_w,
      decisions_sc = decisions$supply_chain,
      supplier_params = decisions$purchasing$supplier_params,
      df_sku = lookups$df_sku,
      df_component = lookups$df_component,
      purchasing_constants = constants$purchasing, ##upadte phase 2
      operations_constants = constants$operations,##upadte 0.1.8
      sc_constants = constants$supply_chain##upadte 0.1.8
    )
    ##update patch 0.0.5
    if (nrow(rm_rep$receipts_w) > 0){
      state$rm_pipeline <- bind_rows(state$rm_pipeline, rm_rep$receipts_w)
    }
    # Update 0.1.7  more safty,make robust engine
    if (is.null(state$rm_pipeline) || !("eta_week" %in% names(state$rm_pipeline))) {
      state$rm_pipeline <- tibble(
        week_order = integer(),
        component  = character(),
        vendor     = character(),
        order_qty_units = double(),
        purchase_price  = double(),
        purchase_cost_total = double(),
        eta_week   = integer()
      )
    }
    # receipts are those eta_week == w update phase 2
    rm_receipts_w <- state$rm_pipeline %>%
      filter(eta_week == w) %>%left_join(decisions$purchasing$supplier_params %>%
                                           select(component, delivery_reliability),
                                         by="component") %>% #update beta 0.1.1
      transmute(week=w, component, 
                received_qty_units=order_qty_units*replace_na(delivery_reliability,100)/100) ##update beta 0.1.1
    # keep future pipeline only update phase 2
    state$rm_pipeline <- state$rm_pipeline %>%
      filter(eta_week > w)
    
    prod_rep <- execute_production_week(
      week=w, state=state, plan_w=plan_w,
      rm_receipts_w = rm_receipts_w,     # Phase 2 pipeline receipts
      df_sku = lookups$df_sku,
      operations_constants = constants$operations,
      decisions_ops = decisions$operations
    )
    ## Adjust 0.1.8 ver2
    ##Add PI tool #update beta 0.1.3 move Pi tool below plan_w
    pi_kpi_w <- pi_tool_predict_week(
      week = w,
      demand_w = demand_w,
      plan_w = plan_w, ##Update 0.1.3
      produced_w = prod_rep$production,#NEW 0.1.8 ver2
      decisions_sc = decisions$supply_chain,
      df_sku = lookups$df_sku,
      operations_constants = constants$operations,
      purchasing_constants = constants$purchasing,
      decisions_ops = decisions$operations,
      sc_constants = constants$supply_chain   ##update beta 0.1.0
    )
    
    # PATCH 0.1.3: update next production week after running #Update logic 0.1.4 production based on plant
    if (!is.null(state$fg_next_prod)) {
      
      ran_skus <- plan_w %>%
        filter(allowed_to_run, planned_units > 0) %>%
        distinct(sku)
      
      if (nrow(ran_skus) > 0) {
        
        cad_tbl <- plan_w %>% select(sku, cadence_weeks)
        
        ran_skus <- ran_skus %>%
          left_join(cad_tbl, by="sku") %>%
          mutate(
            cadence_weeks = replace_na(cadence_weeks, 1L),
            next_prod_week = w + cadence_weeks
          )
        
        state$fg_next_prod <- state$fg_next_prod %>%
          left_join(ran_skus %>% select(sku, next_prod_week), by="sku") %>%
          mutate(next_prod_week = coalesce(next_prod_week.y, next_prod_week.x)) %>%
          select(sku, next_prod_week)
      }
    }
    
    flows$production <- bind_rows(flows$production, prod_rep$production)##
    flows$operations <- bind_rows(flows$operations, prod_rep$kpis)
    
    sales_exec <- fulfill_demand_week(
      week=w, state=state, demand_w=demand_w,
      produced_w = prod_rep$production,
      df_sku = lookups$df_sku,
      sales_decision_cp = decisions$sales$customer_product_level,
      customer_master = constants$sales$customer_master,
      shortage_rule = decisions$sales$shortage_rule   ## Update beta 0.0.9
    )
    
    state <- update_state_week(
      state, receipts_w=rm_receipts_w,
      rm_used_w=prod_rep$rm_used,
      produced_w=prod_rep$production,
      sales_exec=sales_exec,
      df_sku = lookups$df_sku
    )
    
    # append flows
    flows$sales       <- bind_rows(flows$sales, sales_exec)
    flows$purchasing  <- bind_rows(flows$purchasing, rm_rep$orders)
    flows$purchasing_receipts <- bind_rows(flows$purchasing_receipts, rm_rep$receipts_w)#update beta 0.0.8
    flows$distribution <- bind_rows(##update beta 0.0.7
      flows$distribution,
      admin_cost_week(w, rm_rep$orders, sales_exec, constants$purchasing)
    )
    flows$pi <- bind_rows(flows$pi, pi_kpi_w) ##Update beta 0.1.0
    flows$warehousing <- bind_rows(
      flows$warehousing,
      fg_warehouse_week(w, state, lookups, constants$operations, decisions$operations)) ##Update 0.1.6
    #Update 0.1.9 RM warehouse
    flows$warehousing <- bind_rows(
      flows$warehousing,
      rm_warehouse_week(
        week = w,
        state = state,
        lookups = lookups,
        operations_constants = constants$operations,
        decisions_ops = decisions$operations,
        orders_w = rm_rep$orders,
        receipts_w = rm_receipts_w,
        sc_constants = constants$supply_chain,
        supplier_params = decisions$purchasing$supplier_params   #update 0.2.1
      )
    )
    # inventory snapshot for holding cost update phase 2
    flows$inventory <- bind_rows(flows$inventory,
                                 inventory_snapshot_week(w, state, lookups, decisions, constants)
    )
  }
  
  finance_round <- finance_aggregate_round(flows = flows, 
                                           finance_constants = constants$finance,
                                           operations_constants = constants$operations)#update beta 0.0.1
  list(flows=flows, finance_round=finance_round, state_end=state)
}

# =========================
# 7) STATE0 + DECISIONS ROUND (PHASE 1 DEFAULT)
# =========================
##beta 0.0.3
rm_ss  <- c(Pack=2, PET=3, Orange=2, Mango=2, `Vitamin C`=2.5)
rm_lot <- c(Pack=4, PET=4, Orange=4, Mango=4, `Vitamin C`=4)

names(rm_ss)  <- norm_component(names(rm_ss))
names(rm_lot) <- norm_component(names(rm_lot))
#Update beta 0.1.6
make_assortment_decisions <- function(customers, skus){
  expand_grid(customer = customers, sku = skus) %>%
    mutate(active = TRUE)
}

assortment_cp <- make_assortment_decisions(
  customers = unique(customer_master$customer),
  skus      = unique(benchmark_demand$sku)
)

# normal cate manage :
assortment_cp <- assortment_cp %>%
  mutate(active = case_when(
    customer == "Dominick's" & sku %in% c(
      "Fressie Orange PET",
      "Fressie Orange/C-power PET",
      "Fressie Orange/Mango PET"
    ) ~ FALSE,
    TRUE ~ TRUE
  ))
#new: production cadence tracker (next week each SKU can be produced) #Update beta 0.1.3
init_fg_next_prod <- tibble(
  sku = unique(df_sku$sku),
  next_prod_week = 1L
)
state0_round <- list(
  rm_stock = tibble(component = unique(df_component$component), units=0),
  fg_stock = tibble(sku = unique(df_sku$sku), units=0, age_w=0),
  rm_pipeline = tibble(##Update 0.1.7 init schema cho pipeline để init schema cho pipeline để
    week_order = integer(),
    component  = character(),
    vendor     = character(),
    order_qty_units = double(),
    purchase_price  = double(),
    purchase_cost_total = double(),
    eta_week   = integer() 
  ), #add phase 2
  fg_next_prod = init_fg_next_prod, ##Update beta 0.1.3
  prod_frozen_pipeline = tibble()   ##Update beta 0.1.5
)

decisions_round <- list(
  sales = list(
    customer_product_level = sales_decision_cp,
    assortment_cp = assortment_cp,##Update 0.1.6
    shortage_rule = "proportional" ##update beta 0.0.9
  ),
  purchasing = list(
    supplier_params = supplier_params_default
  ),
  supply_chain = list(
    rm_safety_stock_w = rm_ss ,##beta 0.0.3
    rm_lot_size_w     = rm_lot,##beta 0.0.3
    fg_safety_stock_w = setNames(rep(3,length(unique(df_sku$sku))), unique(df_sku$sku)),
    ##Update beta 0.1.0 : interval in DAYS, bound [1;25]
    fg_production_interval_d = setNames(rep(9, length(unique(df_sku$sku))), unique(df_sku$sku)),
    frozen_period_weeks = 2  #Update beta 0.1.5
  ),
  operations = list(##Update 0.1.6
    fg_pallet_locations = 1500,
    rm_pallet_locations = 900,
    num_shifts = 1,          ##Update 0.1.8
    n_fte_bottling = 5,      ##Update 0.1.8
    n_fte_rm_wh = 3,        ##update 0.1.9
    rm_inspection_on = FALSE  #On/off inspection
  )
)

lookups <- list(df_sku=df_sku, df_component=df_component,
                CIpur_lookup = CIpur_lookup,CIsale_lookup = CIsale_lookup)##update 0.2.3 for phase 3 sync by lookup instead of stale ci

constants <- list(
  sales=sales_constants,
  purchasing=purchasing_constants,
  supply_chain=sc_constants,
  operations=operations_constants,
  finance=finance_constants
)

exo <- list(
  volume_factor = sales_area_cp %>%
    distinct(customer) %>%
    mutate(volume_factor = 1)
)

# =========================
# 8) RUN PHASE 1 (free running cost)
# =========================
out <- engine_round(
  state0 = state0_round,
  decisions = decisions_round,
  constants = constants,
  lookups = lookups,
  exogenous = exo,
  n_weeks = 26
)
# =========================
# 9) RUN PHASE 2 keep it real:))
# =========================
out$finance_round$ROI_pred
out$finance_round

# =========================
# 9.1) Prepare phase 3 - AUTO CATEGORY MANAGEMENT 
# =========================
# chạy 1 vòng baseline để có sales history
out_base <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)

# auto assortment từ history vừa rồi
assort_auto <- auto_assortment_phase3(
  benchmark_demand = benchmark_demand,
  sales_exec_hist  = out_base$flows$sales,
  df_sku           = df_sku,
  sales_decision_cp= sales_decision_cp,
  assortment_prior = NULL, # nếu muốn override giữ lại
  rules = list(
    margin_min_per_unit = 0,
    margin_fail_weeks   = 4,
    sla_fail_rate_max   = 0.60,
    slowmover_ratio     = 0.20,
    min_weeks_obs       = 6
  )
)

decisions_round$sales$assortment_cp <- assort_auto

# chạy lại để xem effect
out_auto <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)

out_base$finance_round$ROI_pred
out_auto$finance_round$ROI_pred

assortment_cp %>% arrange(customer, sku)
assort_auto    %>% arrange(customer, sku)

anti_join(assortment_cp, assort_auto, by=c("customer","sku","active"))

# =========================
# Phase 3 
# =========================
sku_gm <- sales_area_cp %>%
  group_by(sku) %>%
  summarise(gm = mean(gross_margin_per_w, na.rm=TRUE), .groups="drop")

gm_cut <- median(sku_gm$gm, na.rm=TRUE)

sku_segment <- sku_gm %>%
  mutate(segment = ifelse(gm >= gm_cut, "innovative", "functional")) %>%
  select(sku, segment)
##Wrapper safty avoid boom
suggest_int_r <- function(trial, name, low, high){
  val <- trial$suggest_int(name, as.integer(low), as.integer(high))
  val <- reticulate::py_to_r(val)
  if (length(val) != 1) stop("suggest_int returned non-scalar for ", name)
  as.integer(val)
}

suggest_float_r <- function(trial, name, low, high){
  val <- trial$suggest_float(name, as.numeric(low), as.numeric(high))
  val <- reticulate::py_to_r(val)
  if (length(val) != 1) stop("suggest_float returned non-scalar for ", name)
  as.numeric(val)
}

suggest_cat_r <- function(trial, name, choices){
  val <- trial$suggest_categorical(name, choices)
  val <- reticulate::py_to_r(val)
  if (length(val) != 1) stop("suggest_categorical returned non-scalar for ", name)
  val
}
##Objecttive Optuna function
objective_r <- function(trial){
  
  # (0) Base vectors cho RM safety stock & lot size
  rm_ss_base  <- decisions_round$supply_chain$rm_safety_stock_w
  rm_lot_base <- decisions_round$supply_chain$rm_lot_size_w
  
  #(1) SALES AGREEMENT per CUSTOMER
  customers <- unique(customer_master$customer) %>% as.character()
  
  sale_idx <- purrr::map_int(customers, function(cu){
    key <- paste0("sale_", safe_key(cu))
    
    cu_norm <- norm_customer(cu)
    n_sale_cu <- CIsale_lookup %>%
      mutate(customer = norm_customer(customer)) %>%
      dplyr::filter(customer == cu_norm) %>%
      nrow()
    
    if (n_sale_cu == 0) n_sale_cu <- 1L
    
    suggest_int_r(trial, key, 1L, n_sale_cu)
  })
  names(sale_idx) <- customers
  
  sales_decision_trial <- apply_sales_agreement_customer(
    sales_decision_cp, CIsale_lookup, sale_idx
  )
  
  #(2) PURCH AGREEMENT per COMPONENT
  components <- unique(supplier_params_default$component)%>% as.character()
  
  pur_idx <- purrr::map_int(components, function(co){
    key <- paste0("pur_", safe_key(co))
    
    base_vendor <- supplier_params_default$vendor[match(co, supplier_params_default$component)]
    n_pur_co <- CIpur_lookup %>% filter(vendor == base_vendor) %>% nrow()
    if (n_pur_co == 0) n_pur_co <- 1L
    
    suggest_int_r(trial, key, 1L, n_pur_co)
  })
  names(pur_idx) <- components
  
  supplier_params_trial <- apply_purch_agreement_component(
    supplier_params_default = supplier_params_default,
    CIpur_lookup            = CIpur_lookup,
    idx_by_component        = pur_idx
  )
  #(3a) SUPPLY CHAIN FG knobs theo segment 
  fg_ss_func   <- suggest_float_r(trial, "fg_ss_func", 0, 2)
  fg_int_func  <- suggest_int_r(trial, "fg_int_func", 7L, 25L)
  
  fg_ss_inno   <- suggest_float_r(trial, "fg_ss_inno", 2, 6)
  fg_int_inno  <- suggest_int_r(trial, "fg_int_inno", 1L, 10L)
  
  frozen_w     <- suggest_int_r(trial, "frozen_w", 1L, 4L)
  # build per-SKU vectors
  fg_ss_vec  <- setNames(rep(0, length(df_sku$sku)), df_sku$sku)
  fg_int_vec <- setNames(rep(9, length(df_sku$sku)), df_sku$sku)
  
  for(s in df_sku$sku){
    seg <- sku_segment$segment[match(s, sku_segment$sku)]
    if (is.na(seg) || seg=="functional"){
      fg_ss_vec[s]  <- fg_ss_func
      fg_int_vec[s] <- fg_int_func
    } else {
      fg_ss_vec[s]  <- fg_ss_inno
      fg_int_vec[s] <- fg_int_inno
    }
  }
  
  #(3b) NEW: RM safety stock cho TẤT CẢ components
  # - pet, vitamin_c: [1;6]
  # - pack, orange, mango: [1;8]
  rm_ss_vec <- rm_ss_base
  for (comp in names(rm_ss_vec)) {
    if (comp %in% c("pet", "vitamin_c")) {
      low <- 1; high <- 6
    } else {
      low <- 1; high <- 8
    }
    key <- paste0("rm_ss_", comp)
    val <- suggest_float_r(trial, key, low, high)
    rm_ss_vec[comp] <- val
  }
  
  #(3c) NEW: RM lot size
  # global base [1;10] + override cho PET & VitC [4;10]
  rm_lot_all  <- suggest_int_r(trial, "rm_lot_all", 1L, 10L)
  rm_lot_pet  <- suggest_int_r(trial, "rm_lot_pet", 4L, 10L)
  rm_lot_vitc <- suggest_int_r(trial, "rm_lot_vitamin_c", 4L, 10L)
  
  rm_lot_vec <- rm_lot_base
  rm_lot_vec[] <- rm_lot_all
  if ("pet" %in% names(rm_lot_vec))        rm_lot_vec["pet"]       <- rm_lot_pet
  if ("vitamin_c" %in% names(rm_lot_vec)) rm_lot_vec["vitamin_c"] <- rm_lot_vitc
  
  #(3d) NEW: Shortage rule (3 options)
  shortage_rule <- suggest_cat_r(
    trial, "shortage_rule",
    c("proportional","fcfs","priority")
  )
  
  #(3e) NEW: Operations knobs
  fg_pallet_locations <- suggest_int_r(trial, "fg_pallet_locations", 500L, 5000L)
  rm_pallet_locations <- suggest_int_r(trial, "rm_pallet_locations", 500L, 5000L)
  n_fte_rm_wh         <- suggest_int_r(trial, "n_fte_rm_wh", 2L, 20L)
  n_fte_bottling      <- suggest_int_r(trial, "n_fte_bottling", 2L, 20L)
  num_shifts          <- suggest_int_r(trial, "num_shifts", 1L, 5L)
  rm_inspection_on    <- suggest_cat_r(trial, "rm_inspection_on", c(FALSE, TRUE))
  
  #(4) assemble decisions
  decisions_trial <- decisions_round
  
  # sales & purchasing từ lookup index
  decisions_trial$sales$customer_product_level <- sales_decision_trial
  decisions_trial$purchasing$supplier_params   <- supplier_params_trial
  
  # FG knobs
  decisions_trial$supply_chain$fg_safety_stock_w        <- fg_ss_vec
  decisions_trial$supply_chain$fg_production_interval_d <- fg_int_vec
  decisions_trial$supply_chain$frozen_period_weeks      <- frozen_w
  
  # NEW: RM knobs
  decisions_trial$supply_chain$rm_safety_stock_w <- rm_ss_vec
  decisions_trial$supply_chain$rm_lot_size_w     <- rm_lot_vec
  
  # NEW: shortage rule
  decisions_trial$sales$shortage_rule <- shortage_rule
  
  # NEW: operations knobs
  decisions_trial$operations$fg_pallet_locations <- fg_pallet_locations
  decisions_trial$operations$rm_pallet_locations <- rm_pallet_locations
  decisions_trial$operations$n_fte_rm_wh         <- n_fte_rm_wh
  decisions_trial$operations$n_fte_bottling      <- n_fte_bottling
  decisions_trial$operations$num_shifts          <- num_shifts
  decisions_trial$operations$rm_inspection_on    <- rm_inspection_on
  
  # keep assortment fixed từ auto CM nếu muốn:
  # decisions_trial$sales$assortment_cp <- decisions_round$sales$assortment_cp
  
  #(5) run 26w
  out_trial <- engine_round(
    state0 = state0_round,
    decisions = decisions_trial,
    constants = constants,
    lookups = lookups,
    exogenous = exo,
    n_weeks = 26
  )
  
  roi <- out_trial$finance_round$ROI_pred
  if (!is.finite(roi)) roi <- -1e9
  roi
}
# =========================
# A) RECONSTRUCT BEST DECISIONS FROM OPTUNA PARAMS
# =========================
reconstruct_best_decisions <- function(
    best_params,
    decisions_base,
    sales_decision_cp,
    supplier_params_default,
    CIsale_lookup,
    CIpur_lookup,
    customer_master,
    df_sku,
    sku_segment
){
  
  #1) SALES agreement per customer
  customers <- unique(customer_master$customer)
  
  sale_idx <- purrr::map_int(customers, function(cu){
    key <- paste0("sale_", safe_key(cu))
    val <- best_params[[key]]
    if (is.null(val)) 1L else as.integer(val)
  })
  names(sale_idx) <- customers
  
  sales_decision_best <- apply_sales_agreement_customer(
    sales_decision_cp = sales_decision_cp,
    CIsale_lookup = CIsale_lookup,
    idx_by_customer = sale_idx
  )
  
  #2) PURCH agreement per component
  components <- unique(supplier_params_default$component)
  
  pur_idx <- purrr::map_int(components, function(co){
    key <- paste0("pur_", safe_key(co))
    val <- best_params[[key]]
    if (is.null(val)) 1L else as.integer(val)
  })
  names(pur_idx) <- components
  
  supplier_params_best <- apply_purch_agreement_component(
    supplier_params_default = supplier_params_default,
    CIpur_lookup = CIpur_lookup,
    idx_by_component = pur_idx
  )
  
  #3a) SUPPLY CHAIN FG knobs
  fg_ss_func  <- best_params[["fg_ss_func"]]  %||% 3
  fg_int_func <- best_params[["fg_int_func"]] %||% 9
  
  fg_ss_inno  <- best_params[["fg_ss_inno"]]  %||% 3
  fg_int_inno <- best_params[["fg_int_inno"]] %||% 9
  
  frozen_w    <- best_params[["frozen_w"]] %||% 2
  
  fg_ss_vec  <- setNames(rep(0, length(df_sku$sku)), df_sku$sku)
  fg_int_vec <- setNames(rep(9, length(df_sku$sku)), df_sku$sku)
  
  for(s in df_sku$sku){
    seg <- sku_segment$segment[match(s, sku_segment$sku)]
    if (is.na(seg) || seg == "functional"){
      fg_ss_vec[s]  <- fg_ss_func
      fg_int_vec[s] <- fg_int_func
    } else {
      fg_ss_vec[s]  <- fg_ss_inno
      fg_int_vec[s] <- fg_int_inno
    }
  }
  
  #3b) NEW: RM safety stock cho TẤT CẢ components
  rm_ss_vec <- decisions_base$supply_chain$rm_safety_stock_w
  for (comp in names(rm_ss_vec)) {
    key <- paste0("rm_ss_", comp)
    val <- best_params[[key]]
    if (!is.null(val)) {
      rm_ss_vec[comp] <- as.numeric(val)
    }
  }
  
  #3c) NEW: RM lot size
  rm_lot_vec <- decisions_base$supply_chain$rm_lot_size_w
  
  rm_lot_all <- best_params[["rm_lot_all"]]
  if (!is.null(rm_lot_all)) {
    rm_lot_vec[] <- as.numeric(rm_lot_all)
  }
  
  rm_lot_pet <- best_params[["rm_lot_pet"]]
  if (!is.null(rm_lot_pet) && "pet" %in% names(rm_lot_vec)) {
    rm_lot_vec["pet"] <- as.numeric(rm_lot_pet)
  }
  
  rm_lot_vitc <- best_params[["rm_lot_vitamin_c"]]
  if (!is.null(rm_lot_vitc) && "vitamin_c" %in% names(rm_lot_vec)) {
    rm_lot_vec["vitamin_c"] <- as.numeric(rm_lot_vitc)
  }
  
  #3d) NEW: shortage rule
  shortage_rule <- best_params[["shortage_rule"]] %||%
    decisions_base$sales$shortage_rule %||%
    "proportional"
  
  #3e) NEW: operations knobs
  fg_pallet_locations <- best_params[["fg_pallet_locations"]] %||%
    decisions_base$operations$fg_pallet_locations
  rm_pallet_locations <- best_params[["rm_pallet_locations"]] %||%
    decisions_base$operations$rm_pallet_locations
  n_fte_rm_wh <- best_params[["n_fte_rm_wh"]] %||%
    decisions_base$operations$n_fte_rm_wh
  n_fte_bottling <- best_params[["n_fte_bottling"]] %||%
    decisions_base$operations$n_fte_bottling
  num_shifts <- best_params[["num_shifts"]] %||%
    decisions_base$operations$num_shifts
  rm_inspection_on <- best_params[["rm_inspection_on"]] %||%
    decisions_base$operations$rm_inspection_on
  
  #4) Assemble decisions
  decisions_best <- decisions_base
  decisions_best$sales$customer_product_level <- sales_decision_best
  decisions_best$purchasing$supplier_params   <- supplier_params_best
  
  decisions_best$supply_chain$fg_safety_stock_w        <- fg_ss_vec
  decisions_best$supply_chain$fg_production_interval_d <- fg_int_vec
  decisions_best$supply_chain$frozen_period_weeks      <- as.integer(frozen_w)
  
  # NEW: RM knobs
  decisions_best$supply_chain$rm_safety_stock_w <- rm_ss_vec
  decisions_best$supply_chain$rm_lot_size_w     <- rm_lot_vec
  
  # NEW: shortage rule
  decisions_best$sales$shortage_rule <- shortage_rule
  
  # NEW: operations knobs
  decisions_best$operations$fg_pallet_locations <- as.integer(fg_pallet_locations)
  decisions_best$operations$rm_pallet_locations <- as.integer(rm_pallet_locations)
  decisions_best$operations$n_fte_rm_wh         <- as.integer(n_fte_rm_wh)
  decisions_best$operations$n_fte_bottling      <- as.integer(n_fte_bottling)
  decisions_best$operations$num_shifts          <- as.integer(num_shifts)
  decisions_best$operations$rm_inspection_on    <- as.logical(rm_inspection_on)
  
  decisions_best
}
# =========================
# B) SAVE BEST DECISIONS TO EXCEL
# =========================
##Optuna TPE
optuna <- reticulate::import("optuna")

objective_py <- reticulate::r_to_py(objective_r)
study <- optuna$create_study(
  direction = "maximize",
  sampler = optuna$samplers$TPESampler(seed=as.integer(42))
)

study$optimize(objective_py, n_trials = 5)

best <- study$best_trial
best$value
best$params

decisions_best <- reconstruct_best_decisions(
  best_params = best$params,
  decisions_base = decisions_round,
  sales_decision_cp = sales_decision_cp,
  supplier_params_default = supplier_params_default,
  CIsale_lookup = CIsale_lookup,
  CIpur_lookup  = CIpur_lookup,
  customer_master = customer_master,
  df_sku = df_sku,
  sku_segment = sku_segment
)

out_best <- engine_round(state0_round, decisions_best, constants, lookups, exo, 26)
out_best$finance_round$ROI_pred
out_best$finance_round

# (optional) export Excel
save_best_decisions_excel <- function(
    decisions_best,
    out_dir = "best_output",
    prefix  = "round_best",
    best_params = NULL,
    finance_round = NULL
){
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # 1) Sales agreement (customer x sku)
  sales_agree <- decisions_best$sales$customer_product_level
  
  # 2) Assortment
  assort <- decisions_best$sales$assortment_cp
  
  # 3) Purchasing agreement (component level)
  purch_agree <- decisions_best$purchasing$supplier_params
  
  # 4) Supply chain knobs tidy
  sc_knobs <- bind_rows(
    tibble(knob="fg_safety_stock_w", name=names(decisions_best$supply_chain$fg_safety_stock_w),
           value=as.numeric(decisions_best$supply_chain$fg_safety_stock_w)),
    tibble(knob="fg_production_interval_d", name=names(decisions_best$supply_chain$fg_production_interval_d),
           value=as.numeric(decisions_best$supply_chain$fg_production_interval_d)),
    tibble(knob="rm_safety_stock_w", name=names(decisions_best$supply_chain$rm_safety_stock_w),
           value=as.numeric(decisions_best$supply_chain$rm_safety_stock_w)),
    tibble(knob="rm_lot_size_w", name=names(decisions_best$supply_chain$rm_lot_size_w),
           value=as.numeric(decisions_best$supply_chain$rm_lot_size_w)),
    tibble(knob="frozen_period_weeks", name="global",
           value=as.numeric(decisions_best$supply_chain$frozen_period_weeks))
  )
  
  # 5) Operations knobs
  ops_knobs <- tibble(
    knob = names(decisions_best$operations),
    value = purrr::map_chr(decisions_best$operations, ~as.character(.x))
  )
  
  # 6) Best params + finance (optional)
  best_params_tbl <- NULL
  if (!is.null(best_params)){
    best_params_tbl <- tibble(
      param = names(best_params),
      value = purrr::map_chr(best_params, ~as.character(.x))
    )
  }
  
  finance_tbl <- NULL
  if (!is.null(finance_round)){
    finance_tbl <- tibble(
      metric = names(finance_round),
      value  = purrr::map_chr(finance_round, ~as.character(.x))
    )
  }
  
  sheets <- list(
    sales_agreement   = sales_agree,
    assortment        = assort,
    purchasing_agreement = purch_agree,
    supply_chain_knobs = sc_knobs,
    operations_knobs = ops_knobs
  )
  if (!is.null(best_params_tbl)) sheets$best_params <- best_params_tbl
  if (!is.null(finance_tbl))     sheets$finance_round <- finance_tbl
  
  path <- file.path(out_dir, paste0(prefix, "_BEST_BUNDLE.xlsx"))
  writexl::write_xlsx(sheets, path)
  
  message("Saved to: ", path)
  invisible(path)
}
save_best_decisions_excel(decisions_best, out_dir = "best_output", prefix = "round1")




save_best_decisions_excel <- function(decisions_best, out_dir="best_output", prefix="round1"){
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # pick main block
  sales_cp   <- decisions_best$sales$customer_product_level %||% tibble()
  assort_cp  <- decisions_best$sales$assortment_cp %||% tibble()
  purch_cp   <- decisions_best$purchasing$supplier_params %||% tibble()
  
  # knobs supply_chain: list (vector named) -> long table
  sc_list <- decisions_best$supply_chain %||% list()
  sc_knobs <- purrr::imap_dfr(sc_list, function(v, k){
    if (is.atomic(v) && length(v) > 0) {
      tibble(
        group = "supply_chain",
        knob  = k,
        key   = if (!is.null(names(v))) names(v) else NA_character_,
        value = as.numeric(v)
      )
    } else {
      tibble(group="supply_chain", knob=k, key=NA_character_, value=NA_real_)
    }
  })
  # knobs operations
  ops_list <- decisions_best$operations %||% list()
  ops_knobs <- purrr::imap_dfr(ops_list, function(v, k){
    tibble(
      group = "operations",
      knob  = k,
      key   = NA_character_,
      value = as.numeric(v)
    )
  })
  # sales knobs (shortage_rule etc.)
  sales_list <- decisions_best$sales %||% list()
  sales_knobs <- purrr::imap_dfr(sales_list, function(v, k){
    if (is.atomic(v) && length(v)==1 && !is.data.frame(v)) {
      tibble(group="sales", knob=k, key=NA_character_,
             value=as.character(v))
    } else {
      NULL
    }
  })
  file_path <- file.path(out_dir, paste0(prefix, "_best_decisions.xlsx"))
  writexl::write_xlsx(
    list(
      sales_customer_product_level = sales_cp,
      sales_assortment             = assort_cp,
      purchasing_supplier_params   = purch_cp,
      knobs_supply_chain           = sc_knobs,
      knobs_operations             = ops_knobs,
      knobs_sales                  = sales_knobs
    ),
    path = file_path
  )
  message("Saved to: ", normalizePath(file_path))
  invisible(file_path)
}
save_best_decisions_excel(decisions_best, out_dir="best_output", prefix="round1")

##another
save_best_bundle_excel <- function(decisions_best,
                                   out_best = NULL,
                                   out_dir = "best_output",
                                   prefix = "round1") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---------- helper ----------
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ---------- A) decisions ----------
  sales_cp   <- decisions_best$sales$customer_product_level %||% tibble()
  assort_cp  <- decisions_best$sales$assortment_cp %||% tibble()
  purch_cp   <- decisions_best$purchasing$supplier_params %||% tibble()
  
  # supply_chain knobs -> long
  sc_list <- decisions_best$supply_chain %||% list()
  sc_knobs <- purrr::imap_dfr(sc_list, function(v, k){
    if (is.atomic(v) && length(v) > 0 && !is.data.frame(v)) {
      tibble(
        group = "supply_chain",
        knob  = k,
        key   = if (!is.null(names(v))) names(v) else NA_character_,
        value = as.numeric(v)
      )
    } else {
      tibble(group="supply_chain", knob=k, key=NA_character_, value=NA_real_)
    }
  })
  
  # operations knobs
  ops_list <- decisions_best$operations %||% list()
  ops_knobs <- purrr::imap_dfr(ops_list, function(v, k){
    if (is.atomic(v) && length(v) > 0 && !is.data.frame(v)) {
      tibble(
        group = "operations",
        knob  = k,
        key   = if (!is.null(names(v))) names(v) else NA_character_,
        value = as.numeric(v)
      )
    } else {
      tibble(group="operations", knob=k, key=NA_character_, value=NA_real_)
    }
  })
  
  # sales simple knobs (shortage_rule, ...)
  sales_list <- decisions_best$sales %||% list()
  sales_knobs <- purrr::imap_dfr(sales_list, function(v, k){
    if (is.atomic(v) && length(v)==1 && !is.data.frame(v)) {
      tibble(
        group="sales",
        knob=k,
        key=NA_character_,
        value=as.character(v)
      )
    } else {
      NULL
    }
  })
  
  # ---------- B) out_best ----------
  finance_tbl <- tibble()
  state_rm_end <- tibble()
  state_fg_end <- tibble()
  flows_sales <- flows_production <- flows_operations <- tibble()
  flows_purchasing <- flows_purch_receipts <- tibble()
  flows_inventory <- flows_warehousing <- flows_distribution <- flows_pi <- tibble()
  
  if (!is.null(out_best)) {
    # finance_round is list -> 1-row tibble
    fr <- out_best$finance_round %||% list()
    if (length(fr) > 0) {
      finance_tbl <- tibble::as_tibble(fr, .name_repair = "unique")
    }
    
    # state_end
    st_end <- out_best$state_end %||% list()
    state_rm_end <- st_end$rm_stock %||% tibble()
    state_fg_end <- st_end$fg_stock %||% tibble()
    
    # flows
    fl <- out_best$flows %||% list()
    flows_sales        <- fl$sales %||% tibble()
    flows_production   <- fl$production %||% tibble()
    flows_operations   <- fl$operations %||% tibble()
    flows_purchasing   <- fl$purchasing %||% tibble()
    flows_purch_receipts <- fl$purchasing_receipts %||% tibble()
    flows_inventory    <- fl$inventory %||% tibble()
    flows_warehousing  <- fl$warehousing %||% tibble()
    flows_distribution <- fl$distribution %||% tibble()
    flows_pi           <- fl$pi %||% tibble()
  }
  
  # ---------- assemble sheets ----------
  sheets <- list(
    sales_customer_product_level = sales_cp,
    sales_assortment             = assort_cp,
    purchasing_supplier_params   = purch_cp,
    knobs_supply_chain           = sc_knobs,
    knobs_operations             = ops_knobs,
    knobs_sales                  = sales_knobs,
    finance_round                = finance_tbl,
    state_end_rm_stock           = state_rm_end,
    state_end_fg_stock           = state_fg_end,
    flows_sales                  = flows_sales,
    flows_production             = flows_production,
    flows_operations             = flows_operations,
    flows_purchasing             = flows_purchasing,
    flows_purchasing_receipts    = flows_purch_receipts,
    flows_inventory              = flows_inventory,
    flows_warehousing            = flows_warehousing,
    flows_distribution           = flows_distribution,
    flows_pi                     = flows_pi
  )
  
  # remove empty NULL sheets just in case
  sheets <- sheets[!vapply(sheets, is.null, logical(1))]
  
  file_path <- file.path(out_dir, paste0(prefix, "_BEST_BUNDLE.xlsx"))
  writexl::write_xlsx(sheets, path = file_path)
  
  message("Saved full bundle to: ", normalizePath(file_path))
  invisible(file_path)
}
save_best_bundle_excel(
  decisions_best = decisions_best,
  out_best = out_best,
  out_dir = "best_output",
  prefix = "round1"
)

##
sum(out$flows$sales$revenue, na.rm=TRUE)
sum(out$flows$purchasing$purchase_cost_total, na.rm=TRUE)
sum(out$flows$sales$pallets_shipped, na.rm=TRUE)
summary(sales_constants$observed$benchmark_demand$demand_week_pieces)
head(out$flows$sales)
summary(out$flows$sales$demand_units)##not =0
summary(out$flows$sales$delivered_units)
##expect not divergent check
out$flows$operations %>% summarise(avg_cost=mean(production_cost_total),
                                   total_cost = sum(production_cost_total))
out$finance_round$production_costs
out$flows$warehousing %>% summarise(sum_fg_cost=sum(fg_location_cost+fg_overflow_cost))
##check 
out$flows$pi %>% summarise(
  stock = mean(stock_costs_year/52),
  startup = mean(startup_loss_cost_year/52),
  changeover = mean(changeover_costs_year/52),
  total = mean(total_costs_year/52)
)
# check PI chỉ nhảy khi có internal production
out$flows$pi %>% filter(n_sku_run==0) %>% summarise(total_pi=sum(total_costs_year/52))

out$flows$purchasing %>%
  group_by(week) %>%
  summarise(order=sum(order_qty_units), cost=sum(purchase_cost_total)) %>%
  arrange(week) %>% head(10)

chk_week2 <- out$flows$production %>%
  group_by(week) %>%
  summarise(prod_cost=sum(production_cost),
            liters=sum(liters_required),
            .groups="drop") %>% print()

kpi_week2 <- out$flows$operations %>%
  transmute(week, kpi_cost=production_cost_total, idle_cost) %>% print()

chk_week2 %>%
  left_join(kpi_week2, by="week") %>%
  mutate(total_prod_cost = prod_cost + idle_cost,
         diff = total_prod_cost - kpi_cost) %>%
  summarise(max_abs_diff = max(abs(diff)))
sum(out$flows$operations$production_cost_total) -
  (sum(out$flows$production$production_cost) + sum(out$flows$operations$idle_cost))

out$flows$sales %>% summarise(demand=sum(demand_units), delivered=sum(delivered_units))
summary(out$flows$sales$sales_price)
summary(out$flows$production$produced_units)

##Sanity check 
out$flows$sales %>% 
  summarise(total_demand=sum(demand_units),
            total_delivered=sum(delivered_units),
            fill_rate=total_delivered/total_demand)##check why revenue is low rely on demand not fullfill by RM leadtime + safty stock?
out$flows$production %>%
  group_by(week) %>%
  summarise(liters=sum(liters_required), prod=sum(produced_units)) %>%
  filter(liters>0 | prod>0)

##
# baseline
decisions_round$supply_chain$fg_production_interval_d[] <- 9
outB <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)

# scenario A
decisions_round$supply_chain$fg_production_interval_d[] <- 1
outA <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)

outB$flows$pi %>% summarise(
  stock = mean(stock_costs_year/52),
  startup = mean(startup_loss_cost_year/52),
  changeover = mean(changeover_costs_year/52),
  total = mean(total_costs_year/52)
)

outA$flows$pi %>% summarise(
  stock = mean(stock_costs_year/52),
  startup = mean(startup_loss_cost_year/52),
  changeover = mean(changeover_costs_year/52),
  total = mean(total_costs_year/52)
)
outB$finance_round$ROI_pred
outB$finance_round
head(out$flows$pi)

decisions_round$supply_chain$frozen_period_weeks <- 1
out1 <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)
out1$finance_round

decisions_round$supply_chain$frozen_period_weeks <- 4
out4 <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)
out4$finance_round