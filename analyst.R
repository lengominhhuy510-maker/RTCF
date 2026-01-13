library(writexl)
library(tibble)
library(openxlsx)
library(readxl)
library(highcharter)
library(dplyr)
library(data.table)
library(DT)
library(readr)
library(tidyverse)
library(purrr)
library(tibble)
library(janitor)
library(naniar)
library(scales)
library(skimr)
library(lubridate)
file_path <- "[VSCC 2026] CHALLENGE C - BUSINESS CASE (DATASET).xlsx"
readxl::excel_sheets(file_path)
read_clean <- function(sheet){
  readxl::read_excel(file_path, sheet = sheet) %>%
    janitor::clean_names()
}
product_master   <- read_clean("ProductMaster")
country_master   <- read_clean("CountryMaster") %>%
  select(country_code, country_name, region, domestic_flag, currency) # drop unnamed cols
supplier_master  <- read_clean("SupplierMaster")
supplier_material<- read_clean("SupplierMaterial")
port24           <- read_clean("Port24_Historical")
port25           <- read_clean("Port25_Historical")
dev26            <- read_clean("Dev26_Potential FCST")
pnl              <- read_clean("PnL_FY2024")
financial_view   <- read_clean("FinancialView")

skimr::skim(port24)
naniar::miss_var_summary(port24)
port24 <- port24 %>%
  mutate(
    fx_rate_to_usd = if_else(currency == "USD" & is.na(fx_rate_to_usd),
                             1, fx_rate_to_usd))
port25 <- port25 %>%
  mutate(
    fx_rate_to_usd = if_else(currency == "USD" & is.na(fx_rate_to_usd),
                             1, fx_rate_to_usd))
pnl_raw <- read_excel(
  file_path,
  sheet = "PnL_FY2024",
  col_types = c("text", "text", "text", "text")
) %>% clean_names()
eu_to_num <- function(x) {
  x <- str_squish(as.character(x))
  x <- ifelse(str_count(x, "\\.") > 1, str_replace_all(x, "\\.", ""), x)
  x <- str_replace_all(x, ",", ".")
  val <- as.numeric(x)
  val <- case_when(
    val < 100 ~ val * 1000000,
    val < 100000 ~ val * 1000,
    TRUE ~ val)
  return(val)
}
pnl_clean <- pnl_raw %>%
  mutate(
    fy2024 = eu_to_num(fy2024),
    fy2025 = eu_to_num(fy2025),
    fy2026_projected = eu_to_num(fy2026_projected)
  )
pnl_clean <- pnl_clean %>%
  rename(pnl_usd = pn_l_usd)
pnl <- pnl_clean
pnl_clean %>%
  filter(pn_l_usd %in% c("Net Sales - Port","Net Sales - Dev","Total Net Sales")) %>%
  select(pn_l_usd, fy2024, fy2025, fy2026_projected)
## Combine+normal
sales_raw <- bind_rows(
  port24 %>% mutate(src_year = 2024),
  port25 %>% mutate(src_year = 2025)
) %>%
  mutate(
    req_ship_date = as.Date(req_ship_date),
    year = if_else(!is.na(year_tmp), as.integer(year_tmp), year(req_ship_date)),
    discount_rate = as.numeric(discount_rate),
    gm_percent    = as.numeric(gm_percent),
    otif_percent  = as.numeric(otif_percent),
    sales_usd     = as.numeric(sales_usd),
    sales_units   = as.numeric(sales_units)
  )
##Cost to serve by country
cts <- financial_view %>%
  filter(!!sym("x2026_revenue_mix_targets") %in% c("VN","DE","FR","ES","NL","US","TH","SG","MY")) %>%
  transmute(
    country_code = x2026_revenue_mix_targets,
    logistics_cost_perunit_usd = as.numeric(x2),
    cost_nature = x3,
    remarks = x4
  )
##Join
country_master <- readxl::read_excel(file_path, sheet = "CountryMaster") %>% clean_names()
product_master <- readxl::read_excel(file_path, sheet = "ProductMaster") %>% clean_names()
fact_sales <- sales_raw %>%
  left_join(country_master, by = c("country_code")) %>%
  left_join(product_master %>% select(sku, portfolio_type, category, cogs_usd, std_prod_lead_time_days),
            by = "sku") %>%
  left_join(cts, by = "country_code") %>%
  mutate(
    # Ước tính "list sales" và discount value (nếu discount_rate là % giảm trên list)
    list_sales_usd   = if_else(!is.na(discount_rate) & discount_rate < 1,
                               sales_usd / (1 - discount_rate), NA_real_),
    discount_value_usd = list_sales_usd - sales_usd,
    # Gross profit theo GM% (để consistent với dataset)
    gross_profit_usd = sales_usd * gm_percent,
    # Logistics cost-to-serve (ước tính) từ cost/unit
    logistics_cost_usd = sales_units * logistics_cost_perunit_usd
  )
##Revenue + GM%
wmean <- function(x, w) sum(x*w, na.rm = TRUE) / sum(w, na.rm = TRUE)

kpi_year <- fact_sales %>%
  group_by(year) %>%
  summarise(
    net_sales = sum(sales_usd, na.rm = TRUE),
    gm_pct_w  = wmean(gm_percent, sales_usd),
    disc_w    = wmean(discount_rate, sales_usd),
    otif_w    = wmean(otif_percent, sales_usd),
  ) %>% ungroup() %>%  arrange(year)

kpi_country <- fact_sales %>%
  group_by(country_code) %>%
  summarise(
    net_sales = sum(sales_usd, na.rm = TRUE),
    gm_pct_w  = wmean(gm_percent, sales_usd),
    disc_w    = wmean(discount_rate, sales_usd),
    otif_w    = wmean(otif_percent, sales_usd),
  ) %>% ungroup() %>%  arrange(desc(net_sales))

top10 <- kpi_country %>% slice_head(n = 10)

hchart(kpi_year, "column", hcaes(x = year, y = net_sales)) %>%
  hc_title(text = "Net Sales by Year") %>%
  hc_yAxis(labels = list(format = "{value}")) %>%
  hc_tooltip(pointFormat = "Sales: <b>${point.y:,.0f}</b>") %>%
  hc_add_theme(hc_theme_flat())
sale_gm <- highchart() %>%
  hc_title(text = "Sales and GM% Trend") %>%
  hc_xAxis(categories = kpi_year$year) %>%
  hc_add_series(name = "Net Sales (USD)", type = "column",
                data = round(kpi_year$net_sales, 0)) %>%
  hc_add_series(name = "GM% (weighted)", type = "line",
                data = round(kpi_year$gm_pct_w * 100, 2),
                yAxis = 1) %>%
  hc_yAxis_multiples(
    list(title = list(text = "Net Sales"), labels = list(format = "{value}")),
    list(title = list(text = "GM%"), labels = list(format = "{value}%"), opposite = TRUE)
  ) %>%
  hc_tooltip(shared = TRUE) %>%
  hc_add_theme(hc_theme_flat())
bubble_df <- top10 %>%
  transmute(
    country = country_code,
    x = round(otif_w * 100, 2),
    y = round(gm_pct_w * 100, 2),
    z = net_sales
  )

hchart(bubble_df, "bubble", hcaes(x = x, y = y, size = z, name = country)) %>%
  hc_title(text = "OTIF vs GM% (Top 10 markets, bubble size = sales)") %>%
  hc_xAxis(title = list(text = "OTIF (%)")) %>%
  hc_yAxis(title = list(text = "GM% (%)")) %>%
  hc_tooltip(pointFormat = "{point.name}<br>OTIF: <b>{point.x}%</b><br>GM%: <b>{point.y}%</b><br>Sales: <b>${point.z:,.0f}</b>") %>%
  hc_add_theme(hc_theme_flat())
##Supp Information flow + Finance
supplier_master %>% count(payment_terms) %>% arrange(desc(n))
supplier_material %>% count(compliance_status) %>% arrange(desc(n))
parse_terms_days <- function(x){
  x <- tolower(trimws(as.character(x)))
  d <- case_when(
    is.na(x) ~ NA_real_,
    str_detect(x, "net\\s*\\d+") ~ as.numeric(str_extract(x, "\\d+")),
    str_detect(x, "\\bn\\s*\\d+\\b") ~ as.numeric(str_extract(x, "\\d+")),
    str_detect(x, "cod|immediate|due on receipt|prepay|advance") ~ 0,
    TRUE ~ NA_real_
  )
  d
}

### 1) Supply fact
fact_supply <- supplier_material %>%
  mutate(
    moq_per_order = as.numeric(moq_per_order),
    production_lt_days = as.numeric(production_lt_days),
    unit_price = as.numeric(unit_price),
    overhead_fee_pct = as.numeric(overhead_fee_pct)
  ) %>%
  left_join(
    supplier_master %>%
      mutate(supplier_terms_days = parse_terms_days(payment_terms)) %>%
      select(supplier_id, supplier_country, payment_terms, supplier_terms_days,
             overall_supplier_score, preferred_supplier_flag, core_supplier_flag),
    by = "supplier_id"
  ) %>%
  mutate(
    is_china = supplier_country %in% c("CN","CHINA"),
    is_active = active_usage_flag == "Y"
  )

### 2) KPI: compliance mix (active only)
compliance_mix <- fact_supply %>%
  filter(is_active) %>%
  count(compliance_status) %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

### 3) KPI: China-origin exposure (active only)
china_exposure <- fact_supply %>%
  filter(is_active) %>%
  summarise(
    active_materials = n(),
    china_materials = sum(is_china, na.rm = TRUE),
    pct_china = china_materials / active_materials
  )

### 4) KPI: supplier readiness (active materials, split CN vs non-CN)
supplier_readiness <- fact_supply %>%
  filter(is_active) %>%
  distinct(supplier_id, supplier_country, is_china, overall_supplier_score) %>%
  summarise(
    suppliers = n(),
    avg_score = mean(overall_supplier_score, na.rm = TRUE),
    pct_score_ge4 = mean(overall_supplier_score >= 4, na.rm = TRUE),
    .by = is_china
  )

### 5) KPI: MOQ/LT pressure (active only) - top 20
pressure_tbl <- fact_supply %>%
  filter(is_active) %>%
  mutate(pressure_raw = moq_per_order * production_lt_days) %>%
  arrange(desc(pressure_raw)) %>%
  select(material_code, supplier_id, supplier_country, compliance_status,
         moq_per_order, production_lt_days, pressure_raw) %>%
  slice_head(n = 20)
compliance_mix
china_exposure
supplier_readiness
pressure_tbl

##Payment terms mismatch (Working capital)
### customer terms days trong fact_sales
fact_sales <- fact_sales %>%
  mutate(customer_terms_days = parse_terms_days(payment_terms))

### supplier terms reference (active supply average)
sup_terms_ref <- fact_supply %>%
  filter(is_active) %>%
  summarise(sup_terms_avg = mean(supplier_terms_days, na.rm = TRUE)) %>%
  pull(sup_terms_avg)

### cash gap by market + cash tied proxy
wmean <- function(x, w) sum(x*w, na.rm = TRUE) / sum(w, na.rm = TRUE)

cash_gap_market <- fact_sales %>%
  group_by(country_code) %>%
  summarise(
    net_sales = sum(sales_usd, na.rm = TRUE),
    cust_terms_w = wmean(customer_terms_days, sales_usd),
  ) %>% ungroup() %>% 
  mutate(
    supplier_terms_ref = sup_terms_ref,
    cash_gap_days = cust_terms_w - supplier_terms_ref,
    cash_tied_proxy = (net_sales/365) * cash_gap_days
  ) %>%
  arrange(desc(cash_tied_proxy))

cash_gap_market %>% head(10)

##visual
comp_df <- compliance_mix %>%
  mutate(pct = round(pct*100, 1))
hchart(comp_df, "column", hcaes(x = compliance_status, y = pct)) %>%
  hc_title(text = "Compliance Readiness Mix (Active materials)") %>%
  hc_yAxis(title = list(text = "% of active materials")) %>%
  hc_tooltip(pointFormat = "<b>{point.y}%</b>") %>%
  hc_add_theme(hc_theme_flat())
###China exposure
china_df <- data.frame(metric="China-origin exposure", pct=round(china_exposure$pct_china*100,1))
hchart(china_df, "bar", hcaes(x = metric, y = pct)) %>%
  hc_title(text = "China-origin Exposure (Active materials)") %>%
  hc_yAxis(title = list(text = "%")) %>%
  hc_tooltip(pointFormat = "<b>{point.y}%</b>") %>%
  hc_add_theme(hc_theme_flat())
### MOQ x Lead time pressure
press_df <- pressure_tbl %>%
  mutate(label = paste0(material_code, " (", supplier_country, ")")) %>%
  arrange(pressure_raw)
hchart(press_df, "bar", hcaes(x = label, y = pressure_raw)) %>%
  hc_title(text = "MOQ × Lead Time Pressure (Top 20 active materials)") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Pressure (MOQ × LT days)")) %>%
  hc_tooltip(pointFormat = "Pressure: <b>{point.y:,.0f}</b>") %>%
  hc_add_theme(hc_theme_flat())

## 1) compliance mix % (active)
compliance_mix <- fact_supply %>%
  filter(is_active) %>%
  count(compliance_status) %>%
  mutate(pct = n/sum(n)) %>%
  arrange(desc(n))
compliance_mix

## 2) compliance impact heatmap style table
comp_impact <- fact_supply %>%
  filter(is_active) %>%
  count(compliance_status, compliance_impact) %>%
  group_by(compliance_impact) %>%
  mutate(pct_within_impact = n/sum(n)) %>%
  ungroup()
comp_impact

financial_view <- readxl::read_excel(file_path, sheet="FinancialView") %>% janitor::clean_names()
# Bạn đang cần map bảng logistics cost/unit đúng cột — nếu bạn chưa extract được,
# bạn chụp/ paste head(financial_view) hoặc names(financial_view) để mình map chính xác.
names(financial_view)
head(financial_view, 20)

dev26 <- readxl::read_excel(file_path, sheet="Dev26_Potential FCST") %>% janitor::clean_names()
dev_summary <- dev26 %>%
  summarise(
    dev_expected_sales = sum(expected_net_sales_usd, na.rm = TRUE),
    dev_units = sum(fcst_2026_units, na.rm = TRUE)
  )
dev_summary
##
kpi_cts <- fact_sales %>%
  group_by(country_code) %>%
  summarise(
    net_sales = sum(sales_usd, na.rm = TRUE),
    logistics_cost = sum(logistics_cost_usd, na.rm = TRUE),
    cts_per_sales = logistics_cost / net_sales,
    .groups = "drop"
  ) %>%
  arrange(desc(net_sales))
kpi_cts %>% head(10)

# lấy top markets (theo net sales)
top_mkts <- kpi_cts %>% arrange(desc(net_sales))
ceo_stack <- top_mkts %>%
  left_join(kpi_country %>% select(country_code, gm_pct_w, disc_w, otif_w),
            by = "country_code") %>%
  left_join(cash_gap_market %>% select(country_code, cash_gap_days, cash_tied_proxy),
            by = "country_code") %>%
  mutate(
    gm_pct_w = gm_pct_w * 100,
    disc_w = disc_w * 100,
    otif_w = otif_w * 100,
    cts_per_sales = cts_per_sales * 100
  ) %>%
  select(country_code, net_sales, gm_pct_w, disc_w, otif_w, cts_per_sales, cash_gap_days, cash_tied_proxy)
ceo_stack
# Value from discount reduction (assume sales giữ nguyên)
top_sales <- ceo_stack %>% summarise(sales = sum(net_sales)) %>% pull()
sens_discount <- tibble::tibble(
  scenario = c("-0.5pp discount (top mkts)", "-1.0pp discount (top mkts)"),
  delta_pp = c(0.5, 1.0)
) %>%
  mutate(value_usd = top_sales * (delta_pp/100))
sens_discount

# Value from CTS reduction
top_sales_cts <- ceo_stack %>% summarise(sales = sum(net_sales)) %>% pull()
sens_cts <- tibble::tibble(
  scenario = c("-0.3pp CTS/Sales (top mkts)", "-0.5pp CTS/Sales (top mkts)"),
  delta_pp = c(0.3, 0.5)
) %>%
  mutate(value_usd = top_sales_cts * (delta_pp/100))
sens_cts

##time to market
ttm_core_market <- fact_sales %>%
  group_by(country_code) %>%
  summarise(
    net_sales = sum(sales_usd, na.rm = TRUE),
    ttm_prod_w = sum(std_prod_lead_time_days * sales_usd, na.rm = TRUE) / sum(sales_usd, na.rm = TRUE),
    .groups="drop"
  ) %>%
  arrange(desc(net_sales))
ttm_core_market %>% head(10)

ttm_core_port <- fact_sales %>%
  group_by(portfolio_type.x) %>%
  summarise(
    net_sales = sum(sales_usd, na.rm = TRUE),
    ttm_prod_w = sum(std_prod_lead_time_days * sales_usd, na.rm = TRUE) / sum(sales_usd, na.rm = TRUE),
    .groups="drop"
  ) %>%
  arrange(desc(net_sales))
ttm_core_port

ttm_top <- ttm_core_market %>% slice_head(n = 10) %>% arrange(ttm_prod_w)
hchart(ttm_top, "bar", hcaes(x = country_code, y = ttm_prod_w)) %>%
  hc_title(text = "Core Production Lead Time (weighted) - Top markets") %>%
  hc_yAxis(title = list(text = "Days")) %>%
  hc_tooltip(pointFormat = "Prod LT: <b>{point.y:.1f} days</b>") %>%
  hc_add_theme(hc_theme_flat())
##state gate
readiness_scorecard <- tibble::tibble(
  metric = c("Active materials fully compliant (Yes)",
             "Active materials partial compliant",
             "Non-China suppliers with score >=4",
             "China suppliers with score >=4",
             "China-origin exposure (active materials)"),
  value = c(
    compliance_mix$pct[compliance_mix$compliance_status=="Yes"],
    compliance_mix$pct[compliance_mix$compliance_status %in% c ("Partial", "No")],
    supplier_readiness$pct_score_ge4[supplier_readiness$is_china==FALSE],
    supplier_readiness$pct_score_ge4[supplier_readiness$is_china==TRUE],
    china_exposure$pct_china
  )
) %>%
  mutate(value_pct = round(value*100,1))
readiness_scorecard
ceo_stack2 <- ceo_stack %>%
  left_join(ttm_core_market %>% select(country_code, ttm_prod_w), by="country_code")
ceo_stack2

##pnl
# PnL total net sales
pnl_sales <- pnl %>%
  filter(pnl_usd == "Total Net Sales") %>%
select(fy2024, fy2025) %>%
  pivot_longer(
    cols = everything(),
    names_to = "year_char",
    values_to = "pnl_net_sales"
  ) %>%
  mutate(
    year = as.integer(gsub("fy", "", year_char))
  ) %>%
  select(year, pnl_net_sales)
fact_sales_sum <- kpi_year %>%
  mutate(year = as.integer(year)) %>%
  select(year, fact_net_sales = net_sales)

# Reconcile
recon_sales <- fact_sales_sum %>%
  left_join(pnl_sales, by = "year") %>%
  mutate(
    abs_diff = fact_net_sales - pnl_net_sales,
    pct_diff = abs_diff / pnl_net_sales
  )
total_2026 <- pnl_clean %>%
  filter(pnl_usd == "Total Net Sales") %>%
  pull(fy2026_projected)

cap_table <- tibble::tibble(
  total_net_sales_2026 = total_2026,
  dev_cap_2026 = total_2026 * 0.02,
  dev_pipeline = dev_summary$dev_expected_sales,
  pipeline_vs_cap = dev_summary$dev_expected_sales / (total_2026 * 0.02)
)
cap_table
## Market profitability
market_profit <- fact_sales %>%
  group_by(country_code) %>%
  summarise(
    net_sales = sum(sales_usd, na.rm = TRUE),
    gross_profit = sum(gross_profit_usd, na.rm = TRUE),
    logistics_cost = sum(logistics_cost_usd, na.rm = TRUE),
    contrib_profit = gross_profit - logistics_cost,
    gm_pct = gross_profit / net_sales,
    cts_pct = logistics_cost / net_sales,
    contrib_margin_pct = contrib_profit / net_sales,
    .groups = "drop"
  ) %>%
  arrange(desc(net_sales))

market_profit
mp_top <- market_profit %>% slice_head(n = 10) %>% arrange(contrib_margin_pct)

hchart(mp_top, "bar", hcaes(x = country_code, y = contrib_margin_pct)) %>%
  hc_title(text = "Contribution Margin Proxy by Market (GP - Outbound CTS)") %>%
  hc_yAxis(labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = "Contribution margin: <b>{point.y:.2%}</b>") %>%
  hc_add_theme(hc_theme_flat())