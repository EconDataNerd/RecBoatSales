# Prep step for Recreational Boating Dashboard 1
# Reads the full Rec Boating xlsx, keeps only the fields this dashboard needs,
# aggregates to monthly units + value, and writes a small boat_sales.csv.
# Re-run this whenever new data is added. Source: Kevin Decker | rec boating dashboards

library(readxl)
library(dplyr)
library(readr)

# ---- Config --------------------------------------------------------------
# Point this at your live file. Update the path if it moves.
input_path  <- "/Users/kevindecker/Library/CloudStorage/GoogleDrive-kadecker@uw.edu/My Drive/Recreational Boating/Rec Boating Data.xlsx"
output_path <- "boat_sales.csv"            # monthly totals -> Monthly Summary tab
source_path <- "boat_sales_by_source.csv"  # monthly totals by source -> Trends tab

# Which date drives a "sale": use the transaction fields (default).
# To key off purchase instead, change to "Purchase Year" / (no month) below.
year_col   <- "Transaction Year"
month_col  <- "Transaction Month Value"   # numeric 1-12
value_col  <- "Purchase Cost"
source_col <- "Source of Sales"           # e.g. "Used Private", "New Dealer"

# ---- Read ----------------------------------------------------------------
message("Reading ", input_path, " ...")
raw <- read_excel(input_path, guess_max = 100000)

needed <- c(year_col, month_col, value_col, source_col)
missing <- setdiff(needed, names(raw))
if (length(missing) > 0) {
  stop("These expected columns weren't found: ", paste(missing, collapse = ", "),
       "\nColumns in file: ", paste(names(raw), collapse = " | "))
}

# ---- Aggregate -----------------------------------------------------------
summary <- raw %>%
  transmute(
    year  = suppressWarnings(as.integer(.data[[year_col]])),
    month = suppressWarnings(as.integer(.data[[month_col]])),
    value = suppressWarnings(as.numeric(.data[[value_col]]))
  ) %>%
  filter(!is.na(year), !is.na(month), month >= 1, month <= 12) %>%
  group_by(year, month) %>%
  summarise(
    units = n(),                          # boats sold = number of transactions
    value = sum(value, na.rm = TRUE),     # sales value = sum of purchase cost
    .groups = "drop"
  ) %>%
  arrange(year, month)

# ---- Monthly totals by source (for the Trends tab) -----------------------
by_source <- raw %>%
  transmute(
    year   = suppressWarnings(as.integer(.data[[year_col]])),
    month  = suppressWarnings(as.integer(.data[[month_col]])),
    value  = suppressWarnings(as.numeric(.data[[value_col]])),
    source = as.character(.data[[source_col]])
  ) %>%
  filter(!is.na(year), !is.na(month), month >= 1, month <= 12) %>%
  mutate(source = ifelse(is.na(source) | trimws(source) == "",
                         "Unknown", trimws(source))) %>%
  group_by(year, month, source) %>%
  summarise(units = n(), value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, month, source)

# ---- Reconciliation check ------------------------------------------------
# Both files are cut from the same rows, so monthly units and value must match
# between the totals and the by-source file. A mismatch means a bad/partial
# upload — surface it loudly rather than ship it.
recon <- summary %>%
  rename(units_total = units, value_total = value) %>%
  left_join(by_source %>% group_by(year, month) %>%
              summarise(units_src = sum(units), value_src = sum(value), .groups = "drop"),
            by = c("year", "month"))

bad <- recon %>%
  filter(units_total != units_src | abs(value_total - value_src) >= 1)

if (nrow(bad) == 0) {
  message("Reconciliation OK: monthly units and value match between the totals and by-source files.")
} else {
  warning("RECONCILIATION MISMATCH in ", nrow(bad),
          " month(s) — check the source file. Offending rows:")
  print(as.data.frame(head(bad, 10)))
}

# ---- Write ---------------------------------------------------------------
write_csv(summary, output_path)
write_csv(by_source, source_path)
message("Working directory: ", getwd())
message("Wrote ", nrow(summary), " month rows to: ", normalizePath(output_path),
        "  (", min(summary$year), "-", max(summary$year), ")")
message("Wrote ", nrow(by_source), " source rows to: ", normalizePath(source_path))
message("Done. Open those files, or re-run app.R to see the update.")
print(tail(summary, 6))

