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
#SC
lo <- decisions_round
lo$supply_chain$rm_lot_size_w[] <- 1
outLot1 <- engine_round(state0_round, lo, constants, lookups, exo, 26)

hi <- decisions_round
hi$supply_chain$rm_lot_size_w[] <- 6
outLot6 <- engine_round(state0_round, hi, constants, lookups, exo, 26)

outLot1$finance_round
outLot6$finance_round
outLot1$flows$distribution$inbound_orders
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

#

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