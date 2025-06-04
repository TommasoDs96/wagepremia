source("R/00_packages.R")

# CPI data loading (once)
if (!file.exists("data/CPI.csv")) {
  cat("Downloading CPI data...\n")
  curl_download("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current",
                "data/CPI_raw.txt")
  cpi_raw <- fread("data/CPI_raw.txt", fill = TRUE)
  cpi_dt <- cpi_raw[series_id == "CUUR0000SA0" & period == "M13"]
  cpi_dt[, year := as.integer(substr(date, 1, 4))]
  fwrite(cpi_dt[, .(year, cpi = value)], "data/CPI.csv")
  cat("CPI data downloaded and processed.\n")
}
cpi <- fread("data/CPI.csv")

# Merges premia data with CPI data.

# Input file from the pooled regression (R/03_process_acs.R on feature/pooled-regression branch)
pooled_premia_file <- "data/tidy/acs/acs_pooled_msa_year_premia.csv"

# CPI data file (created manually or downloaded)
cpi_file <- "data/CPI.csv" 

# Output directory and file name
output_dir <- "data/output/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
final_output_file <- file.path(output_dir, "msa_wage_premia_pooled_simplified_cpi.csv")

# Loading data
cat("--- Loading data ---\n")

if (!file.exists(pooled_premia_file)) {
  stop(paste("Pooled premia file not found:", pooled_premia_file, " - Please run R/03_process_acs.R on the correct branch."))
}
premia_data <- fread(pooled_premia_file)
cat(sprintf("Loaded pooled premia data (from R/03). Rows: %d, Columns: %d\n", nrow(premia_data), ncol(premia_data)))
cat("First few rows of loaded premia_data (input to R/05_deflate.R):\n")
print(head(premia_data))
cat("Structure of loaded premia_data (input to R/05_deflate.R):\n")
str(premia_data)

if (!file.exists(cpi_file)) {
  stop(paste("CPI file not found:", cpi_file))
}
cpi_data <- fread(cpi_file)
cat(sprintf("Loaded CPI data. Rows: %d, Columns: %d\n", nrow(cpi_data), ncol(cpi_data)))

# Preparing and merging data
cat("--- Preparing and merging data ---\n")

# Ensure 'year' is integer in CPI data for merging
cpi_data[, year := as.integer(year)]

# Select relevant CPI column and rename for clarity if needed. Assume CPIAUCSL is the target series.
# Let's keep all columns from CPI for now, or select specific ones.
# cpi_to_merge <- cpi_data[, .(year, CPIAUCSL)] 

# Merge premia data with CPI data
# The premia_data already has 'year'.
merged_data <- merge(premia_data, cpi_data, by = "year", all.x = TRUE)

if (any(is.na(merged_data$CPIAUCSL))) {
  warning("Warning: Some observations did not match CPI data for their year. CPI will be NA for these.")
  print(merged_data[is.na(CPIAUCSL), .(year, n_missing_cpi = .N), by = year])
}

cat(sprintf("Data merged. Rows in merged_data: %d\n", nrow(merged_data)))

# The 'theta_normalized' from the pooled model with year fixed effects already accounts for average national inflation.
# No further "deflation" of theta_normalized by CPI is performed here as it would be redundant or misapplied.
# CPI is included for informational purposes or for specific calculations like expressing premia in constant dollars of a base year.

# Saving final merged data
cat("--- Saving final merged data ---\n")

# Select and order columns for the final output
# Keep all relevant columns from premia_data and the merged CPI columns
# Standardizing column names for downstream use if needed (e.g. if other scripts expect 'fe_adj_lnw')
# For now, we will use 'theta_normalized' as the primary premium measure.

# Ensure met2013 is character
merged_data[, met2013 := as.character(met2013)]

# No RPP data to merge at this point.

setcolorder(merged_data, c("met2013", "year", "theta_raw", "theta_normalized", "se_theta", "n_observations"))

fwrite(merged_data, final_output_file)
cat(paste("Final data with pooled premia and CPI saved to:", final_output_file, "\n"))

cat("--- 05_deflate.R: Finished ---\n") 