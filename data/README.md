\# NPA Data — Cleaned and Tableau-Ready



\## Files in this folder



\*\*npa\_final.csv\*\*

\- Complete cleaned dataset: 70 rows × 5 columns

\- All fiscal years 2012–2024, 5 bank categories

\- Columns: year, bank, gross\_npa\_pct, net\_npa\_pct, gross\_npa (absolute amount)

\- Nulls: 2025 net\_npa\_pct (not yet published), Nationalised Banks net NPA (never published)

\- Ready for any ad hoc analysis



\*\*tableau\_trend.csv\*\*

\- Trend line chart: NPA % over time by bank group

\- 60 rows: 3 bank groups × 20 years (2012–2024) × 2 metrics (gross + net)

\- Used in Tableau view: "NPA Trends by Bank Group (2012–2024)"



\*\*tableau\_prepost.csv\*\*

\- Bar chart: Mean NPA % across three periods per bank group

\- 9 rows: 3 banks × 3 periods (Pre-IBC, Stress Peak, Recovery)

\- Used in Tableau view: "Mean NPA % across three periods per bank group"



\*\*tableau\_recovery.csv\*\*

\- Indexed recovery chart: How fast each group recovered from peak

\- 21 rows: 3 banks × 7 years-since-peak (0–6)

\- indexed\_npa = 100 at peak year, declines toward full recovery

\- Used in Tableau view: "Speed of NPA Recovery..."



\*\*tableau\_full.csv\*\*

\- Complete flat table for scatter plots and ad hoc Tableau use

\- Same as npa\_final.csv but nulls removed



\## Data Quality Notes

\- All values cross-validated against published RBI annual reports

\- Gross NPA ratio: recomputed from gross\_npa / gross\_advances; 

&#x20; zero discrepancies found vs RBI published

\- State names: standardized to RBI DBIE naming convention

\- Foreign Banks net > gross anomaly (2021, 2024): source-level RBI 

&#x20; reporting difference between publications — flagged, not corrected



\## Updates

Last cleaned: \[16-05-2026]. RBI DBIE data through FY 2023-24.

