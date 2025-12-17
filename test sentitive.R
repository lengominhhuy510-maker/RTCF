##test sentitive
#Purchasing
base <- decisions_round
base$supply_chain$rm_safety_stock_w[] <- 2
outSS2 <- engine_round(state0_round, base, constants, lookups, exo, 26)

hi <- decisions_round
hi$supply_chain$rm_safety_stock_w[] <- 5
outSS5 <- engine_round(state0_round, hi, constants, lookups, exo, 26)

outSS2$finance_round
outSS5$finance_round
mean(outSS2$flows$operations$ratio_rm)
mean(outSS5$flows$operations$ratio_rm)
#RM size
lo <- decisions_round
lo$supply_chain$rm_lot_size_w[] <- 1
outLot1 <- engine_round(state0_round, lo, constants, lookups, exo, 26)

hi <- decisions_round
hi$supply_chain$rm_lot_size_w[] <- 6
outLot6 <- engine_round(state0_round, hi, constants, lookups, exo, 26)

outLot1$finance_round
outLot6$finance_round
outLot1$flows$distribution$inbound_orders

#frozen
decisions_round$supply_chain$frozen_period_weeks <- 5
out2 <-engine_round(state0_round, hi, constants, lookups, exo, 26)
decisions_round$supply_chain$frozen_period_weeks <- 0
out0 <- engine_round(state0_round, hi, constants, lookups, exo, 26)
out0$finance_round
out2$finance_round
#FG
lo <- decisions_round
lo$supply_chain$fg_safety_stock_w[] <- 0
outFG0 <- engine_round(state0_round, lo, constants, lookups, exo, 26)

hi <- decisions_round
hi$supply_chain$fg_safety_stock_w[] <- 5
outFG5 <- engine_round(state0_round, hi, constants, lookups, exo, 26)

outFG0$finance_round
outFG5$finance_round
outFG5$flows$operations$hours_required
outFG0$flows$operations$hours_required

#sale
# chạy 1 vòng baseline để có sales history
out_base <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)

# auto assortment  history vừa rồi
assort_auto <- auto_assortment_phase3(
  benchmark_demand = benchmark_demand,
  sales_exec_hist  = out_base$flows$sales,
  df_sku           = df_sku,
  sales_decision_cp= sales_decision_cp,
  assortment_prior = NULL, # nếu muốn override giữ lại
  rules = list(
    margin_min_per_unit = 0,
    margin_fail_weeks   = 4,
    sla_fail_rate_max   = 0.30,
    slowmover_ratio     = 0.20,
    min_weeks_obs       = 4
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
#. Option B
margin_stat <- hist %>%
  group_by(customer, sku) %>%
  summarise(
    weeks_obs = n_distinct(week),
    neg_margin_weeks = sum(unit_margin < rules$margin_min_per_unit &
                             delivered_units > 0, na.rm=TRUE),
    avg_unit_margin = mean(unit_margin[delivered_units > 0], na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    margin_bad = weeks_obs >= rules$min_weeks_obs & 
      neg_margin_weeks >= rules$margin_fail_weeks
  )

#pallet location
decisions_round$operations$fg_pallet_locations <- 500
out_low <-engine_round(state0_round, decisions_round, constants, lookups, exo, 26)
decisions_round$operations$fg_pallet_locations <- 300
out_high <-engine_round(state0_round, decisions_round, constants, lookups, exo, 26)
out_low$finance_round
out_high$finance_round

# 1) check demand scale
sales_area_cp %>% summarise(
  min_dem = min(demand_week_pieces, na.rm=TRUE),
  max_dem = max(demand_week_pieces, na.rm=TRUE),
  mean_dem = mean(demand_week_pieces, na.rm=TRUE)
)

# 2) check demand in engine
out_base$flows$sales %>% summarise(
  total_demand = sum(demand_units),
  total_delivered = sum(delivered_units)
)
str(sales_area_cp$demand_week_pieces)

df_sku %>%
  select(sku, starts_with("rm_"), liters_per_pack, basic_sales_price) %>%
  head()

w1_plan <- out$flows$production %>% filter(week==1)
w1_rm_used <- out$flows$production %>%
  filter(week==1) %>%
  left_join(df_sku %>% select(sku, starts_with("rm_")), by="sku") 
w1_rm_used <- w1_rm_used %>% select(sku, rm_pack1liter,rm_pet,rm_orange,rm_mango,rm_vitaminc,planned_units) %>% 
  print()

head(df_sku %>% select(sku, liters_per_pack, starts_with("rm_")))
out$flows$purchasing %>% summarise(weeks_order=n_distinct(week), total_order_units=sum(order_qty_units))
out$flows$sales %>% group_by(week) %>% summarise(pallets=sum(pallets_shipped)) %>% head()

##sẽ thấy loss how much revenue
base_rev <- out_base$flows$sales %>%
  group_by(customer, sku) %>%
  summarise(rev=sum(revenue), .groups="drop")

auto_rev <- out_auto$flows$sales %>%
  group_by(customer, sku) %>%
  summarise(rev=sum(revenue), .groups="drop")

full_join(base_rev, auto_rev, by=c("customer","sku"), suffix=c("_base","_auto")) %>%
  mutate(delta = rev_auto - rev_base) %>%
  arrange(customer, delta)

out_base$flows$pi %>% summarise(pi_cost=sum(total_costs_year)/52)
out_auto$flows$pi %>% summarise(pi_cost=sum(total_costs_year)/52)

out_base$flows$operations %>% summarise(prod_cost=sum(production_cost_total))
out_auto$flows$operations %>% summarise(prod_cost=sum(production_cost_total))




##phase 3
out_base$flows$production %>% summarise(
  produced = sum(produced_units, na.rm=TRUE),
  outsourced = sum(outsourced_units, na.rm=TRUE)
)

out_base$flows$sales %>% summarise(
  delivered = sum(delivered_units, na.rm=TRUE),
  backorder = sum(backorder_units, na.rm=TRUE),
  fill_rate = delivered/(delivered+backorder)
)
out_base$flows$purchasing %>%
  group_by(component) %>%
  summarise(qty = sum(order_qty_units, na.rm=TRUE),
            cost = sum(purchase_cost_total, na.rm=TRUE))

out_best$flows$production %>%
  select(week, sku, produced_units) %>%
  summarise(prod = sum(produced_units, na.rm=TRUE))

unique(df_component$component)[grepl("pack", unique(df_component$component), ignore.case=TRUE)]

df_component %>%
  mutate(component = norm_component(component)) %>%
  filter(str_detect(component, "pack")) %>%
  select(component, basic_price, pallet_qty)

df_sku %>%
  select(sku, starts_with("rm_")) %>%
  mutate(across(starts_with("rm_"), ~replace_na(as.numeric(.x), 0))) %>%
  mutate(
    rm_sum = rowSums(across(starts_with("rm_"))),
    rm_nonzero = rowSums(across(starts_with("rm_"), ~(.x > 0)))
  ) %>%
  arrange(desc(rm_nonzero), desc(rm_sum)) %>%
  select(sku, rm_nonzero, rm_sum, everything()) %>%
  head(20)

total_delivered <- sum(out_base$flows$sales$delivered_units, na.rm=TRUE)

c(
  revenue_per_unit = out_base$finance_round$revenue / total_delivered,
  cogs_per_unit    = out_base$finance_round$cogs / total_delivered,
  purchase_per_unit= out_base$finance_round$purchase_costs / total_delivered,
  prod_per_unit    = out_base$finance_round$production_costs / total_delivered,
  dist_per_unit    = out_base$finance_round$distribution_costs / total_delivered,
  admin_per_unit   = out_base$finance_round$admin_costs / total_delivered,
  wh_per_unit      = out_base$finance_round$warehouse_costs / total_delivered,
  penalty_per_unit = out_base$finance_round$penalty_costs / total_delivered
)


out_base$state_end$fg_stock %>%
  summarise(
    fg_units_end = sum(units, na.rm=TRUE),
    obsolete_end = sum(obsolete_units, na.rm=TRUE)
  )

out_base$state_end$rm_stock %>%
  summarise(rm_units_end = sum(units, na.rm=TRUE))
##pallets_shipped có về “khoảng hợp lý” không?
out_base$flows$sales %>%
  summarise(
    pallets = sum(pallets_shipped, na.rm=TRUE),
    delivered = sum(delivered_units, na.rm=TRUE),
    pallets_per_million_units = pallets / (delivered/1e6)
  )


# 1) pallet table bạn đang dùng trong admin_cost_week
comp_pallet_tbl <- df_component %>%
  transmute(
    component_key = norm_component(component),
    pallet_qty = as.numeric(readr::parse_number(as.character(pallet_qty)))
  )

# 2) xem key nào NA / <=0
comp_pallet_tbl %>%
  filter(is.na(pallet_qty) | pallet_qty <= 0) %>%
  arrange(component_key)

comp_pallet_tbl %>%
  filter(component_key %in% c("pack_1_liter","pet"))

out_best$flows$sales %>%##not expect 0
  summarise(
    sum_pallets = sum(pallets_shipped, na.rm=TRUE),
    n_lines = n(),
    n_pallet_lines = sum(pallets_shipped > 0, na.rm=TRUE),
    min_pallets = min(pallets_shipped, na.rm=TRUE),
    max_pallets = max(pallets_shipped, na.rm=TRUE)
  )
#check 
sales_chk <- out_base$flows$sales %>%
  summarise(
    rev_flow = sum(revenue, na.rm=TRUE),
    rev_recalc = sum(delivered_units * sales_price, na.rm=TRUE),
    diff = rev_flow - rev_recalc
  )
sales_chk##Nếu diff != 0 => có dòng sales bị mutate/override sai.

pen_chk <- out_base$flows$sales %>%
  left_join(lookups$df_sku %>% select(sku, basic_sales_price), by="sku") %>%
  summarise(
    pen_flow = sum(penalty_cost, na.rm=TRUE),
    pen_recalc = sum(backorder_units * basic_sales_price * 0.3, na.rm=TRUE),
    diff = pen_flow - pen_recalc
  )
pen_chk

out_base$flows$sales %>%
  summarise(
    n = n(),
    n_bad = sum(!is.finite(pallets_shipped) | pallets_shipped < 0, na.rm=TRUE),
    min_p = min(pallets_shipped, na.rm=TRUE),
    max_p = max(pallets_shipped, na.rm=TRUE),
    total_p = sum(pallets_shipped, na.rm=TRUE)
  )

tier_tbl <- constants$finance$distribution$provider_tiers

##closing = opening + produced - shipped - obsolete (obsolete do shelf life)
fg_open_total <- sum(state0_round0$fg_stock$units, na.rm=TRUE)
fg_prod_total <- out_base$flows$production %>% summarise(x=sum(produced_units, na.rm=TRUE)+sum(outsourced_units,na.rm=TRUE)) %>% pull(x)
fg_ship_total <- out_base$flows$sales %>% summarise(x=sum(delivered_units, na.rm=TRUE)) %>% pull(x)
fg_close_total <- sum(out_base$state_end$fg_stock$units, na.rm=TRUE)
fg_obsolete_total <- sum(out_base$state_end$fg_stock$obsolete_units, na.rm=TRUE)

c(
  open=fg_open_total,
  produced=fg_prod_total,
  shipped=fg_ship_total,
  close=fg_close_total,
  obsolete=fg_obsolete_total,
  balance = fg_open_total + fg_prod_total - fg_ship_total - fg_obsolete_total - fg_close_total
)##balance phải gần 0 (sai số floating nhỏ ok). Lệch lớn => bug update_state_week.

rm_open_total <- sum(state0_round0$rm_stock$units, na.rm=TRUE)
rm_recv_total <- out_base$flows$purchasing_receipts %>% summarise(x=sum(order_qty_units, na.rm=TRUE)) %>% pull(x) # nếu muốn receipts theo delivery_reliability thì coi test #6
rm_used_total <- out_base$flows$production %>% { # rm_used nằm riêng, nên dùng flows$production không có
  out_base$flows$operations # placeholder
}

rm_close_total <- sum(out_base$state_end$rm_stock$units, na.rm=TRUE)

c(open=rm_open_total, recv_raw=rm_recv_total, close=rm_close_total)

out_base$flows$operations %>%
  summarise(
    min_out = min(outsourced_pct, na.rm=TRUE),
    max_out = max(outsourced_pct, na.rm=TRUE)
  )

out_base$flows$sales %>%
  summarise(
    n_na_ci = sum(is.na(attained_ci)),
    min_ci = min(attained_ci, na.rm=TRUE),
    max_ci = max(attained_ci, na.rm=TRUE)
  )
out_base$flows$purchasing %>%
  left_join(decisions_round0$purchasing$supplier_params %>% select(component, ci_purch), by="component") %>%
  summarise(
    n_na = sum(is.na(ci_purch)),
    min_ci = min(ci_purch, na.rm=TRUE),
    max_ci = max(ci_purch, na.rm=TRUE)
  )
out_base$flows$warehousing %>%
  summarise(
    fg_overflow = sum(fg_overflow_pallets, na.rm=TRUE),
    rm_overflow = sum(rm_overflow_pallets, na.rm=TRUE),
    fg_cost = sum(fg_location_cost + fg_overflow_cost, na.rm=TRUE),
    rm_cost = sum(rm_location_cost + rm_overflow_cost + rm_labor_cost, na.rm=TRUE)
  )
#diff phải = 0. Nếu không =0 → finance_aggregate_round bị double-count / missed line
flows <- out_base$flows
fr <- out_base$finance_round

recalc <- list(
  revenue = sum(flows$sales$revenue, na.rm=TRUE),
  purchase_costs = sum(flows$purchasing$purchase_cost_total, na.rm=TRUE) + (constants$finance$initial_inventory_cost %||% 0),
  production_costs = sum(flows$production$production_cost, na.rm=TRUE) + sum(flows$operations$idle_cost, na.rm=TRUE),
  holding_costs = sum(flows$inventory$rm_holding_cost, na.rm=TRUE) + sum(flows$inventory$fg_holding_cost, na.rm=TRUE),
  penalty_costs = sum(flows$sales$penalty_cost, na.rm=TRUE),
  warehouse_costs = sum(flows$warehousing$fg_location_cost, na.rm=TRUE) +
    sum(flows$warehousing$fg_overflow_cost, na.rm=TRUE) +
    sum(flows$warehousing$rm_location_cost, na.rm=TRUE) +
    sum(flows$warehousing$rm_overflow_cost, na.rm=TRUE) +
    sum(out_base$flows$warehousing$fg_labor_cost, na.rm = TRUE)+
    sum(flows$warehousing$rm_labor_cost, na.rm=TRUE),
  scrap_costs = sum(flows$warehousing$scrap_cost, na.rm=TRUE),
  admin_costs = sum(flows$distribution$admin_cost, na.rm=TRUE),
  tank_yard_costs = sum(flows$purchasing$inbound_tank_cost, na.rm=TRUE),
  pi_costs_week = sum(flows$pi$total_costs_year/52, na.rm=TRUE) - sum(flows$pi$stock_costs_year/52, na.rm=TRUE)
)

recalc$cogs <- with(recalc,
                    purchase_costs + production_costs + holding_costs + penalty_costs +
                      warehouse_costs + scrap_costs + admin_costs + tank_yard_costs + pi_costs_week
)

c(
  cogs_engine = fr$cogs,
  cogs_recalc = recalc$cogs,
  diff = fr$cogs - recalc$cogs
)


flows <- out_best$flows
# compare weekwise vs linewise per week
wk <- flows$sales %>%
  group_by(week) %>%
  summarise(
    pallets_week = sum(pallets_shipped, na.rm=TRUE),
    cost_weekwise = calc_distribution_cost(pallets_week, finance_constants = constants$finance),
    cost_linewise = sum(purrr::map_dbl(pallets_shipped, ~calc_distribution_cost(.x, finance_constants = constants$finance)), na.rm=TRUE),
    ratio = cost_weekwise / pmax(cost_linewise, 1e-9),
    .groups="drop"
  ) %>%
  arrange(desc(ratio))

head(wk, 10)

constants$operations$bottling_line$base_breakdown_rate <- 0
out_test <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)
out_test$finance_round$ROI_pred