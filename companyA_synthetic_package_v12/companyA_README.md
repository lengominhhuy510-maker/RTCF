# Company A – Synthetic Raw Operations Dataset (Fashion/Apparel)

This package contains **raw, event-level** synthetic data for an anonymous fashion/apparel manufacturer (**Company A**).
No Lean/Operations KPIs are precomputed in the raw tables (no cycle_time / defect_rate / WIP columns).  
Instead, the tables are rich enough that you can compute those metrics in EDA.

## Scenarios
- `before/` : intentionally contains bottlenecks, longer setups, higher defects, more late shipments, more overproduction.
- `after/`  : simulates Lean improvements (SMED, better balancing/pull, quality stabilization).

## Tables (per scenario)
**Core (for Lean + capacity):**
- `production_events.csv` – step-level timestamps (CUT/SEW/QC/FINISH/PACK) + rework events
- `quality_inspections.csv` – inspection events with pass/fail/defect_code
- `work_orders.csv` – work orders mapped to sales orders

**Supply chain (for network flow / constraints):**
- `demand_orders.csv` – demand + due_date + penalty
- `shipments.csv` – planned vs actual delivery (lateness)
- `inventory_snapshots.csv` – on_hand / reserved / backorder with holding & stockout costs
- `purchase_orders.csv` – inbound PO with expected vs received dates

**Master/parameter tables (for LP):**
- `transportation_lanes.csv` – lane costs + capacities + transit times
- `supplier_materials.csv` + `suppliers.csv` – unit_cost + lead_time + max_supply_qty
- `sku_bom.csv` – materials per SKU
- `production_lines.csv` – shift pattern + daily capacity parameters

## Where Lean waste is “baked in” (so you can discover it)
- **Waiting**: bottleneck lines `F1-L2` and `F2-L3` create longer queue/wait gaps between steps (compute from timestamps).
- **Defects/Rework**: a defect spike window around **2025-04-10 → 2025-04-20** (subset of SKUs, QC steps).
- **Overproduction**: `before` scenario plans above demand; you can see the effect in inventory snapshots.
- **Transportation / delay**: `before` shipments have higher probability of delivery delay.
- **Inventory / stockout**: some DC snapshots include `backorder_qty` due to short-ship and volatility.

## How to run
```bash
python companyA_synthetic_generate.py
python companyA_eda_metrics.py
# optional optimization demo (requires PuLP)
python companyA_optimization_demo.py
```

Then open R and run:
```r
source("companyA_visualize.R")
```

Outputs from `companyA_eda_metrics.py` are saved in:
`companyA_synthetic/analysis_outputs/`


## R EDA (compute metrics in R)
Run:
```r
source("companyA_eda_R.R")
```
Outputs:
- companyA_synthetic/analysis_outputs_r/metrics_weekly_r.csv
- line_utilization_weekly_r.csv
- bottleneck_lines_r.csv
- defect_spike_summary_r.csv


## Full pipeline (Lean EDA + Production LP + Network Flow) in R
Run:
```r
source("companyA_full_pipeline.R")
```
Outputs:
- companyA_synthetic/analysis_outputs_r/
- companyA_synthetic/optimization_outputs_r/


## Windows / Custom Directory Usage (e.g., C:/Users/PC/Documents/tfc)
1) Extract the zip into: `C:/Users/PC/Documents/tfc/`
2) Keep the following files/folders together in the same extracted directory:
   - `companyA_synthetic/`
   - `companyA_full_pipeline.R` (and other .R scripts)
3) Run in R:
```r
source("C:/Users/PC/Documents/tfc/companyA_full_pipeline.R")
```
All scripts auto-detect their own folder, so you can run from anywhere.


### lpSolve compatibility note
Some CRAN builds of `lpSolve::lp()` do not accept `lower`/`upper` arguments. The provided LP scripts enforce bounds using explicit constraints (x<=cap, x<=0 for ineligible).


## R Markdown (one file, no `source()` needed)
Open and Knit:
- `companyA_full_report.Rmd`
Or render:
```r
rmarkdown::render("C:/Users/PC/Documents/tfc/companyA_full_report.Rmd")
```
This report runs EDA + LP + Flow and shows charts while hiding code.
