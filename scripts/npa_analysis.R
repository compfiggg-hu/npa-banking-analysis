# Check the NA bank rows
NPA_13_25 |> filter(is.na(Banks))
# Check Year column: note it says 2012 but data likely represents 2012-13 (end-March)
cat("\n--- Year labeling: what does year 2012 represent? ---\n")
NPA_13_25 |> filter(Year == 2012, Banks == "ALL SCHEDULED COMMERCIAL BANKS")
# Check the Gross_and_net_NPA: what are the unnamed columns?
cat("\n--- Gross_and_net_NPA: last row (footnote) ---\n")
tail(Gross_and_net_NPA, 3)
cat("\n--- Gross_and_net_NPA: column 5-9 for first real data block ---\n")
Gross_and_net_NPA[3:17, ]
# Get the full column values for a real data row to understand what ...5 through ...9 contain
cat("Row 4 full values (2023-24, All SCBs):\n")
Gross_and_net_NPA[4, ] |> t()
cat("\nRow 3 full values (header row):\n")
Gross_and_net_NPA[3, ] |> t()
# Check footnote text
cat("\nFootnote text:\n")
Gross_and_net_NPA[113, 1]
# Print full footnote text
cat(Gross_and_net_NPA[[113, 1]])
# Check Old Private Sector Banks block - has gaps (only 3 years of data)
cat("\n\n--- Old Private Sector Banks block ---\n")
Gross_and_net_NPA[42:54, ] |> print(width = Inf)
# Verify year alignment: compare gross advances for ALL SCBs across both datasets
# NPA_13_25 year vs Gross_and_net_NPA fiscal year "YYYY-YY"
scb_npa1 <- NPA_13_25 |>
dplyr::filter(Banks == "ALL SCHEDULED COMMERCIAL BANKS") |>
dplyr::transmute(year_npa1 = Year, gross_adv_npa1 = round(as.numeric(`Gross Advances`)))
scb_gn <- Gross_and_net_NPA |>
dplyr::filter(grepl("^\\d{4}-\\d{2}$", `Year (end-March)`)) |>
dplyr::slice(1:14) |>  # first block = All SCBs
dplyr::transmute(
fiscal_yr = `Year (end-March)`,
year_as_endyr = as.integer(substr(`Year (end-March)`, 1, 4)) + 1L,
gross_adv_gn  = round(as.numeric(`Gross Advances`))
)
# Join on assumed mapping: NPA_13_25 year = end year of fiscal period
dplyr::left_join(scb_npa1, scb_gn, by = c("year_npa1" = "year_as_endyr")) |>
dplyr::select(year_npa1, fiscal_yr, gross_adv_npa1, gross_adv_gn) |>
print(n = Inf)
View(scb_npa1)
View(scb_gn)
View(bank_year_coverage)
library(tidyverse)
# ── STEP 1: Extract group rows from NPA_13_25 ────────────────────────────────
npa1_groups <- NPA_13_25 |>
filter(Banks %in% c("ALL SCHEDULED COMMERCIAL BANKS",
"NATIONALISED BANKS",
"PRIVATE SECTOR BANKS",
"PUBLIC SECTOR BANKS",
"FOREIGN BANKS")) |>
transmute(
year      = as.integer(Year),
bank      = case_when(
Banks == "ALL SCHEDULED COMMERCIAL BANKS" ~ "All Scheduled Commercial Banks",
Banks == "NATIONALISED BANKS"             ~ "Nationalised Banks",
Banks == "PRIVATE SECTOR BANKS"           ~ "Private Sector Banks",
Banks == "PUBLIC SECTOR BANKS"            ~ "Public Sector Banks",
Banks == "FOREIGN BANKS"                  ~ "Foreign Banks"
),
gross_advances = as.numeric(`Gross Advances`),
gross_npa      = as.numeric(`Gross NPAs`),
gross_npa_pct  = round(as.numeric(`Gross NPAs to Gross Advances Ratio (%)`), 2)
)
cat("Groups and year coverage:\n")
npa1_groups |> count(bank, year) |> tidyr::pivot_wider(names_from = year, values_from = n)
View(npa1_groups)
# ── STEP 2: Parse Gross_and_net_NPA into labelled blocks ─────────────────────
gn_raw <- Gross_and_net_NPA |>
rename(
year_str             = `Year (end-March)`,
gross_advances2      = `Gross Advances`,
net_advances         = `Net Advances`,
gross_npa2           = `Non-Performing Assets (NPAs)`,
gross_npa_pct_adv    = ...5,
gross_npa_pct_assets = ...6,
net_npa              = ...7,
net_npa_pct_adv      = ...8,
net_npa_pct_assets   = ...9
) |>
mutate(row_n = row_number())
# Row numbers of block label rows
label_rows <- gn_raw |>
filter(year_str %in% c("Public Sector Banks", "Old Private Sector Banks",
"Private Sector Banks *", "Foreign Banks In India",
"Small Finance Banks")) |>
select(row_n, year_str)
r_psb  <- label_rows$row_n[label_rows$year_str == "Public Sector Banks"]
r_opvt <- label_rows$row_n[label_rows$year_str == "Old Private Sector Banks"]
r_pvt  <- label_rows$row_n[label_rows$year_str == "Private Sector Banks *"]
r_forb <- label_rows$row_n[label_rows$year_str == "Foreign Banks In India"]
r_sfb  <- label_rows$row_n[label_rows$year_str == "Small Finance Banks"]
gn_net <- gn_raw |>
mutate(bank = case_when(
row_n < r_psb                        ~ "All Scheduled Commercial Banks",
row_n >= r_psb  & row_n < r_opvt    ~ "Public Sector Banks",
row_n >= r_opvt & row_n < r_pvt     ~ "Old Private Sector Banks",
row_n >= r_pvt  & row_n < r_forb    ~ "Private Sector Banks",
row_n >= r_forb & row_n < r_sfb     ~ "Foreign Banks",
row_n >= r_sfb                       ~ "Small Finance Banks"
)) |>
# Keep only actual fiscal-year data rows
filter(grepl("^\\d{4}-\\d{2}$", year_str)) |>
# Only the 4 groups that have net NPA data
filter(bank %in% c("All Scheduled Commercial Banks", "Public Sector Banks",
"Private Sector Banks", "Foreign Banks")) |>
transmute(
# "2023-24" → end year 2024
year         = as.integer(substr(year_str, 1, 4)) + 1L,
bank,
net_advances = round(as.numeric(net_advances), 2),
net_npa      = round(as.numeric(net_npa), 2),
net_npa_pct  = round(as.numeric(net_npa_pct_adv), 2)
)
cat("Net NPA data coverage:\n")
gn_net |> count(bank, year) |> tidyr::pivot_wider(names_from = year, values_from = n)
# ── STEP 3: Join and build final table ────────────────────────────────────────
npa_clean <- npa1_groups |>
left_join(gn_net, by = c("year", "bank")) |>
select(
year,
bank,
gross_advances,
gross_npa,
net_advances,
net_npa,
gross_npa_pct,
net_npa_pct
) |>
arrange(desc(year), bank)
cat("Final table dimensions:", dim(npa_clean), "\n\n")
cat("Rows per bank:\n")
npa_clean |> count(bank)
cat("\nNA summary:\n")
npa_clean |> summarise(across(everything(), ~sum(is.na(.))))
# Check which rows have NA net data
npa_clean |> filter(is.na(net_advances)) |> select(year, bank, gross_advances, net_advances)
# Print the final clean table
npa_clean
# Save as final object in session and print full table
npa_final <- npa_clean
print(npa_final, n = 56, width = Inf)
View(npa_clean)
write.csv(npa_final,
"C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/npa_final.csv",
row.names = FALSE)
cat("Exported successfully.\n")
library(ggplot2)
ggplot(npa_final, aes(x = year, y = gross_npa_pct, color = bank)) +
geom_line() +
geom_point() +
labs(title = "Gross NPA % of Gross Advances by Bank Group",
x = "Year", y = "Gross NPA (%)", color = NULL)
ggplot(npa_final |> dplyr::filter(!is.na(net_npa_pct)),
aes(x = year, y = net_npa_pct, color = bank)) +
geom_line() +
geom_point() +
labs(title = "Net NPA % of Net Advances by Bank Group",
x = "Year", y = "Net NPA (%)", color = NULL)
library(tidyverse)
# Add period labels; filter to 2012-2023 (exclude 2024-25 as "transition" years)
npa_periods <- npa_final |>
filter(year >= 2012, year <= 2023) |>
mutate(period = if_else(year <= 2016, "Pre-IBC (2012-16)", "Post-IBC (2017-23)"))
# Mean NPA ratio per group per period
mean_by_period <- npa_periods |>
group_by(bank, period) |>
summarise(
n          = n(),
mean_gross = round(mean(gross_npa_pct, na.rm = TRUE), 2),
.groups    = "drop"
) |>
tidyr::pivot_wider(names_from = period, values_from = c(mean_gross, n))
cat("Mean Gross NPA % by Bank Group and Period:\n")
mean_by_period
# Plot: trend lines with pre/post shading
ggplot(npa_periods |> filter(!is.na(gross_npa_pct)),
aes(x = year, y = gross_npa_pct, color = bank)) +
annotate("rect", xmin = 2012, xmax = 2016.5,
ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.08) +
annotate("rect", xmin = 2016.5, xmax = 2023,
ymin = -Inf, ymax = Inf, fill = "tomato", alpha = 0.08) +
annotate("text", x = 2014, y = 14.8, label = "Pre-IBC", size = 3.5, color = "steelblue4") +
annotate("text", x = 2020, y = 14.8, label = "Post-IBC", size = 3.5, color = "tomato4") +
geom_vline(xintercept = 2016.5, linetype = "dashed", color = "grey40") +
geom_line(linewidth = 0.8) +
geom_point(size = 2) +
scale_x_continuous(breaks = 2012:2023) +
labs(title = "Gross NPA % by Bank Group: Pre vs Post IBC (2016)",
x = "Year", y = "Gross NPA (% of Gross Advances)", color = NULL) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Statistical test: Wilcoxon rank-sum (Mann-Whitney U)
# Only groups with data in BOTH periods, and enough observations
# All SCBs, Foreign Banks, Private Sector Banks: 5 pre + 7 post each
testable_groups <- c("All Scheduled Commercial Banks", "Foreign Banks", "Private Sector Banks")
test_results <- map_dfr(testable_groups, function(grp) {
pre  <- npa_periods |> filter(bank == grp, period == "Pre-IBC (2012-16)") |> pull(gross_npa_pct)
post <- npa_periods |> filter(bank == grp, period == "Post-IBC (2017-23)") |> pull(gross_npa_pct)
wt <- wilcox.test(post, pre, alternative = "two.sided", exact = FALSE)
tibble(
bank          = grp,
mean_pre      = round(mean(pre), 2),
mean_post     = round(mean(post), 2),
change        = round(mean(post) - mean(pre), 2),
W_statistic   = wt$statistic,
p_value       = round(wt$p.value, 4)
)
})
test_results
library(tidyverse)
# YoY change in gross NPA % per group
npa_yoy <- npa_final |>
filter(!is.na(gross_npa_pct)) |>
arrange(bank, year) |>
group_by(bank) |>
mutate(yoy_change = gross_npa_pct - lag(gross_npa_pct)) |>
ungroup()
# Identify peak year per group (within 2017-2021 window where peaks occurred)
peaks <- npa_yoy |>
filter(year >= 2017, year <= 2021) |>
group_by(bank) |>
slice_max(gross_npa_pct, n = 1) |>
ungroup() |>
select(bank, peak_year = year, peak_pct = gross_npa_pct)
cat("Peak year and gross NPA % per group:\n")
peaks
# Post-peak recovery stats per group
# Use 2023 as end (last year with data for all groups in npa_final)
recovery <- npa_yoy |>
left_join(peaks, by = "bank") |>
filter(year > peak_year, year <= 2023) |>
group_by(bank, peak_year, peak_pct) |>
summarise(
recovery_end_pct  = last(gross_npa_pct),      # NPA% at 2023
total_decline     = round(peak_pct - last(gross_npa_pct), 2),
years_of_recovery = n(),
avg_annual_decline = round((peak_pct - last(gross_npa_pct)) / n(), 2),
.groups = "drop"
) |>
arrange(desc(avg_annual_decline))
cat("Recovery stats (post-peak through 2023):\n")
recovery
# Fix: peak_pct is being joined as a vector per group — use first() to resolve
recovery <- npa_yoy |>
left_join(peaks, by = "bank") |>
filter(year > peak_year, year <= 2023) |>
group_by(bank) |>
summarise(
peak_year          = first(peak_year),
peak_pct           = first(peak_pct),
recovery_end_pct   = last(gross_npa_pct),
total_decline      = round(first(peak_pct) - last(gross_npa_pct), 2),
years_of_recovery  = n(),
avg_annual_decline = round((first(peak_pct) - last(gross_npa_pct)) / n(), 2),
.groups = "drop"
) |>
arrange(desc(avg_annual_decline))
recovery
# Plot 1: YoY change in gross NPA % (post-peak years only, per group)
yoy_postpeak <- npa_yoy |>
left_join(peaks, by = "bank") |>
filter(year > peak_year, year <= 2023, !is.na(yoy_change)) |>
mutate(direction = if_else(yoy_change < 0, "Decline", "Rise"))
ggplot(yoy_postpeak, aes(x = year, y = yoy_change, fill = direction)) +
geom_col() +
geom_hline(yintercept = 0, linewidth = 0.4) +
facet_wrap(~bank, scales = "free_x") +
scale_fill_manual(values = c("Decline" = "steelblue", "Rise" = "tomato")) +
labs(title = "Year-on-Year Change in Gross NPA % (Post-Peak Recovery)",
x = "Year", y = "YoY Change (percentage points)", fill = NULL) +
theme(legend.position = "bottom",
axis.text.x = element_text(angle = 45, hjust = 1))
# Plot 2: Indexed recovery — NPA % rebased to 100 at each group's peak
# Makes speed of decline directly comparable across groups
recovery_indexed <- npa_yoy |>
left_join(peaks, by = "bank") |>
filter(year >= peak_year, year <= 2023, !is.na(gross_npa_pct)) |>
group_by(bank) |>
mutate(
years_since_peak  = year - peak_year,
indexed_npa       = round((gross_npa_pct / first(gross_npa_pct)) * 100, 1)
) |>
ungroup() |>
filter(bank != "Nationalised Banks")  # only 1 post-peak data point
ggplot(recovery_indexed, aes(x = years_since_peak, y = indexed_npa, color = bank)) +
geom_line(linewidth = 0.9) +
geom_point(size = 2.5) +
geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
scale_x_continuous(breaks = 0:6) +
scale_y_continuous(limits = c(0, 105)) +
labs(title = "Speed of NPA Recovery: Indexed to Peak = 100",
subtitle = "How much of peak NPA % remains at each year after peak",
x = "Years Since Peak", y = "NPA % (peak = 100)", color = NULL)
# Comprehensive audit of npa_final
library(tidyverse)
cat("=== 1. DIMENSIONS & STRUCTURE ===\n")
cat("Rows:", nrow(npa_final), "| Cols:", ncol(npa_final), "\n")
cat("Year range:", min(npa_final$year), "to", max(npa_final$year), "\n\n")
cat("=== 2. COMPLETENESS: all year x bank combos ===\n")
expand_grid(
year = 2012:2025,
bank = unique(npa_final$bank)
) |>
left_join(npa_final |> mutate(exists = TRUE), by = c("year", "bank")) |>
filter(is.na(exists)) |>
print(n = Inf)
cat("=== 3. IMPOSSIBLE VALUES: net_advances > gross_advances ===\n")
npa_final |>
filter(!is.na(net_advances), net_advances > gross_advances) |>
select(year, bank, gross_advances, net_advances, gross_npa, net_npa)
cat("\n=== 4. IMPOSSIBLE VALUES: net_npa > net_advances ===\n")
npa_final |>
filter(!is.na(net_npa), !is.na(net_advances), net_npa > net_advances) |>
select(year, bank, net_advances, net_npa)
cat("\n=== 5. IMPOSSIBLE VALUES: gross_npa > gross_advances ===\n")
npa_final |>
filter(!is.na(gross_npa), gross_npa > gross_advances) |>
select(year, bank, gross_advances, gross_npa)
cat("\n=== 6. RATIO CONSISTENCY CHECK (recomputed vs stored) ===\n")
npa_final |>
filter(!is.na(gross_npa_pct)) |>
mutate(
recomputed_pct = round((gross_npa / gross_advances) * 100, 2),
diff           = abs(gross_npa_pct - recomputed_pct)
) |>
filter(diff > 0.05) |>
select(year, bank, gross_npa_pct, recomputed_pct, diff)
cat("=== 7. FOREIGN BANKS net_advances > gross_advances: is it a source issue? ===\n")
cat("--- From NPA_13_25 (gross side) ---\n")
NPA_13_25 |>
filter(Banks == "FOREIGN BANKS", Year %in% c(2012, 2013, 2021, 2024)) |>
mutate(across(c(`Gross Advances`, `Gross NPAs`), as.numeric)) |>
select(Year, `Gross Advances`, `Gross NPAs`, `Gross NPAs to Gross Advances Ratio (%)`)
cat("\n--- From Gross_and_net_NPA (net side, Foreign Banks block) ---\n")
Gross_and_net_NPA[83:96, ] |>
filter(grepl("^\\d{4}-\\d{2}$", `Year (end-March)`)) |>
filter(`Year (end-March)` %in% c("2011-12", "2012-13", "2020-21", "2023-24")) |>
select(`Year (end-March)`, `Gross Advances`, `Net Advances`, `...7`)
cat("=== 8. ROOT CAUSE ANALYSIS ===\n")
cat("\nFor Foreign Banks 2013 (NPA_13_25 year=2013 vs GN '2012-13'):\n")
cat("  NPA_13_25 gross_advances:", 260405, "\n")
cat("  GN '2012-13' gross_advances:", 268966, "| net_advances:", 263680, "\n")
cat("  → YEAR MISMATCH: NPA_13_25 year=2013 ≠ GN '2012-13'\n")
cat("  → NPA_13_25 year=2013 actually matches GN '2011-12' gross (234727)? No...\n")
cat("  → Or it matches a different year entirely\n\n")
# Let's compare ALL years for Foreign Banks across both sources to find correct mapping
cat("Foreign Banks gross_advances comparison:\n")
fb_npa1 <- NPA_13_25 |>
filter(Banks == "FOREIGN BANKS") |>
transmute(year_npa1 = Year, ga_npa1 = round(as.numeric(`Gross Advances`)))
fb_gn <- Gross_and_net_NPA[83:96,] |>
filter(grepl("^\\d{4}-\\d{2}$", `Year (end-March)`)) |>
transmute(
fiscal_yr    = `Year (end-March)`,
end_year     = as.integer(substr(`Year (end-March)`, 1, 4)) + 1L,
ga_gn        = round(as.numeric(`Gross Advances`)),
na_gn        = round(as.numeric(`Net Advances`))
)
# Show side by side
left_join(fb_npa1, fb_gn, by = c("year_npa1" = "end_year")) |>
mutate(match = ga_npa1 == ga_gn) |>
print(n = Inf)
library(tidyverse)
# Fix 1: Null out net columns for the 3 year-join mismatch rows
mismatch_keys <- tribble(
~year, ~bank,
2012L, "All Scheduled Commercial Banks",
2012L, "Foreign Banks",
2013L, "Foreign Banks"
)
npa_fixed <- npa_final |>
left_join(mismatch_keys |> mutate(mismatch = TRUE), by = c("year", "bank")) |>
mutate(
net_advances = if_else(!is.na(mismatch), NA_real_, net_advances),
net_npa      = if_else(!is.na(mismatch), NA_real_, net_npa),
net_npa_pct  = if_else(!is.na(mismatch), NA_real_, net_npa_pct)
) |>
select(-mismatch)
cat("Rows affected (should be 3):\n")
npa_fixed |>
filter(year %in% c(2012, 2013), bank %in% c("All Scheduled Commercial Banks", "Foreign Banks")) |>
select(year, bank, gross_advances, net_advances, net_npa)
# Fix 2: Add 14 missing year × bank skeleton rows
skeleton <- bind_rows(
# Public Sector Banks: no data 2012–2017
tibble(year = 2012:2017, bank = "Public Sector Banks"),
# Nationalised Banks: discontinued 2018–2025
tibble(year = 2018:2025, bank = "Nationalised Banks")
) |>
mutate(across(c(gross_advances, gross_npa, net_advances, net_npa,
gross_npa_pct, net_npa_pct), ~NA_real_))
npa_fixed <- bind_rows(npa_fixed, skeleton)
cat("New row count (expect 70):", nrow(npa_fixed), "\n")
cat("Rows per bank:\n")
npa_fixed |> count(bank)
# Fix: add NA columns after creating the skeleton
skeleton <- bind_rows(
tibble(year = 2012:2017, bank = "Public Sector Banks"),
tibble(year = 2018:2025, bank = "Nationalised Banks")
) |>
mutate(
gross_advances = NA_real_,
gross_npa      = NA_real_,
net_advances   = NA_real_,
net_npa        = NA_real_,
gross_npa_pct  = NA_real_,
net_npa_pct    = NA_real_
)
npa_fixed <- bind_rows(npa_fixed, skeleton)
cat("New row count (expect 70):", nrow(npa_fixed), "\n")
npa_fixed |> count(bank)
# Fix 3 & 4: Round, sort, final verification
npa_final <- npa_fixed |>
mutate(across(where(is.double), ~round(., 2))) |>
arrange(desc(year), bank)
# ── Final verification ──────────────────────────────────────────────────────
cat("=== FINAL CHECKS ===\n\n")
cat("1. Dimensions (expect 70 x 8):", dim(npa_final), "\n\n")
cat("2. Impossible: net_advances > gross_advances:\n")
npa_final |>
filter(!is.na(net_advances), net_advances > gross_advances) |>
select(year, bank, gross_advances, net_advances) |>
print()
cat("\n3. NA summary per column:\n")
npa_final |> summarise(across(everything(), ~sum(is.na(.)))) |> print(width = Inf)
cat("\n4. All 14 year × bank combos present:\n")
expand_grid(year = 2012:2025, bank = unique(npa_final$bank)) |>
anti_join(npa_final, by = c("year", "bank")) |>
nrow() |>
cat("Missing combinations:", ... = _, "\n")
# The 2 remaining Foreign Banks anomalies are source-level (both RBI tables match on gross,
# but net > gross in the GN publication itself — different scope, not fixable)
# Re-export and display
write.csv(npa_final,
"C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/npa_final.csv",
row.names = FALSE)
cat("CSV re-exported.\n\n")
print(npa_final, n = 70, width = Inf)
View(bank_year_coverage)
library(tidyverse)
# Read your clean file
npa_final <- read.csv("C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/npa_final.csv")
# Export 1: Trend lines view — all groups, all years, gross + net NPA %
# Exclude skeleton NA rows, exclude Nationalised Banks (discontinuous series)
trend_data <- npa_final |>
filter(!is.na(gross_npa_pct)) |>
filter(!bank %in% c("Nationalised Banks", "All Scheduled Commercial Banks")) |>
select(year, bank, gross_npa_pct, net_npa_pct)
write.csv(trend_data,
"C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/tableau_trend.csv",
row.names = FALSE)
# Export 2: Pre/Post IBC comparison — mean NPA % per group per period
prepost_data <- npa_final |>
filter(!is.na(gross_npa_pct)) |>
filter(year >= 2012, year <= 2023) |>
filter(bank %in% c("Public Sector Banks", "Private Sector Banks", "Foreign Banks")) |>
mutate(period = case_when(
year <= 2016 ~ "Pre-IBC (2012-16)",
year >= 2020 ~ "Post-IBC Recovery (2020-23)",
TRUE         ~ "Stress Peak (2017-19)"
)) |>
group_by(bank, period) |>
summarise(mean_gross_npa_pct = round(mean(gross_npa_pct), 2), .groups = "drop")
write.csv(prepost_data,
"C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/tableau_prepost.csv",
row.names = FALSE)
# Export 3: Recovery speed — indexed NPA % since peak
peaks <- tribble(
~bank,                  ~peak_year,
"Public Sector Banks",  2018,
"Private Sector Banks", 2020,
"Foreign Banks",        2017
)
recovery_indexed <- npa_final |>
filter(!is.na(gross_npa_pct)) |>
inner_join(peaks, by = "bank") |>
filter(year >= peak_year, year <= 2023) |>
group_by(bank) |>
mutate(
years_since_peak = year - peak_year,
indexed_npa      = round((gross_npa_pct / first(gross_npa_pct)) * 100, 1)
) |>
ungroup() |>
select(bank, year, years_since_peak, gross_npa_pct, indexed_npa)
write.csv(recovery_indexed,
"C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/tableau_recovery.csv",
row.names = FALSE)
# Export 4: Full clean table for any ad hoc Tableau use
write.csv(
npa_final |> filter(!is.na(gross_npa_pct)),
"C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/tableau_full.csv",
row.names = FALSE
)
cat("All 4 Tableau CSVs exported.\n")
View(npa_final)
View(peaks)
View(prepost_data)
View(recovery_indexed)
View(trend_data)
trend_data <- npa_final |>
filter(!is.na(gross_npa_pct)) |>
filter(year <= 2024) |>  # add this line
filter(!bank %in% c("Nationalised Banks", "All Scheduled Commercial Banks")) |>
select(year, bank, gross_npa_pct, net_npa_pct)
npa_final|>
filter(!is.na(gross_advances)) |>
filter(year<=2024) |> #add this line
filter(!bank %in% c("Nationalised Banks", "All Scheduled Commercial Banks")) |>
select(year, bank, gross_advances, gross_npa, net_advances,net_npa, gross_npa_pct, net_npa_pct)
View(npa_final)
View(npa_final)
savehistory("C:/Users/prath/Desktop/data analysts/projects/NPA/New NPA/npa-banking-analysis/scripts/npa_analysis.Rhistory")
