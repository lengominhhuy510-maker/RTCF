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
# baseline: active all
decisions_round$sales$assortment_cp <- NULL
out_base <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)

# scenario assort: tắt 3 SKU cho Dominick's
decisions_round$sales$assortment_cp <- assortment_cp
out_assort <- engine_round(state0_round, decisions_round, constants, lookups, exo, 26)

out_base$flows$sales %>% summarise(rev=sum(revenue))
out_assort$flows$sales %>% summarise(rev=sum(revenue))

benchmark_demand %>%
  filter(customer=="Dominick's",
         sku %in% c("Fressie Orange 1 liter",
                    "Fressie Orange/C-power 1 liter",
                    "Fressie Orange/Mango 1 liter"))

benchmark_demand %>%
  filter(customer == "Dominick's") %>%
  distinct(sku)