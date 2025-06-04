# R/03_process_acs.R
# Processes ACS data using a pooled regression approach with year and MSA-year fixed effects.

# This script now SOLELY implements the POOLED REGRESSION approach.
# The previous year-by-year processing loop has been removed.

source("R/00_packages.R")

# No DDI needed if reading Stata file directly
# ddi_file_path <- Sys.glob("data/extracts/usa_*.xml")[1]
# if (is.na(ddi_file_path)) {
#   stop("No USA DDI file (e.g., usa_XXXXX.xml) found in data/extracts/. Please ensure R/02_download.R ran successfully.")
# }
# acs_ddi <- read_ipums_ddi(ddi_file_path)

stata_file_path <- "data/extracts/extract19.dta"
rds_file_path <- "data/extracts/extract19.rds" # New RDS file path

if (file.exists(rds_file_path)) {
  cat("Reading entire ACS dataset from RDS file. This should be faster...\n")
  full_acs_data <- readRDS(rds_file_path)
  cat("Finished reading entire ACS dataset from RDS file.\n")
} else if (file.exists(stata_file_path)) {
  cat("RDS file not found. Reading entire ACS dataset from Stata file (all years). This may take some time and memory...\n")
  full_acs_data <- as.data.table(haven::read_dta(stata_file_path))
  cat("Finished reading entire ACS dataset from Stata file.\n")
  cat("Saving data to RDS file for faster loading next time:", rds_file_path, "\n")
  saveRDS(full_acs_data, rds_file_path)
  cat("Finished saving RDS file.\n")
} else {
  stop(paste("Neither Stata file:", stata_file_path, "nor RDS file:", rds_file_path, "found. Please ensure data is in data/extracts/"))
}

# Determine years from the data if possible, or use the known range
# Assuming the Stata file contains years 2005-2023 as per previous downloads
years_in_data <- sort(unique(full_acs_data$year))
cat("Years found in Stata file:", paste(years_in_data, collapse=", "), "\n")
# We will use the years found in the data for the loop
# years   <- 2005:2023 # Original assumption, now dynamic

dir.create("data/tidy/acs", recursive = TRUE, showWarnings = FALSE)

# --- 1. Configuration & Setup for POOLED REGRESSION ---
cat("--- 03_process_acs.R: Starting Pooled ACS Data Processing ---\n")
data_dir <- "data/tidy/"
data_extracts_dir <- "data/extracts/"
output_dir_acs <- file.path(data_dir, "acs")
dir.create(output_dir_acs, showWarnings = FALSE, recursive = TRUE)

stata_file_path <- file.path(data_extracts_dir, "extract19.dta")
rds_file_path <- file.path(data_extracts_dir, "extract19.rds")

output_csv_pooled <- file.path(output_dir_acs, "acs_pooled_msa_year_premia.csv")

# --- 2. Load Data (All Years) for POOLED REGRESSION ---
cat("--- Loading full dataset for Pooled Regression ---\n")
if (file.exists(rds_file_path)) {
  cat(paste("Loading cached RDS file:", rds_file_path, "\n"))
  dt <- readRDS(rds_file_path)
} else if (file.exists(stata_file_path)) {
  cat(paste("Loading Stata file (will cache as RDS afterward):", stata_file_path, "\n"))
  dt <- as.data.table(haven::read_dta(stata_file_path))
  cat(paste("Saving data to RDS for faster loading next time:", rds_file_path, "\n"))
  saveRDS(dt, rds_file_path)
} else {
  stop("Neither Stata DTA nor RDS file found. Please run 02_download.R or place extract19.dta in data/extracts/.")
}
cat("Full dataset loaded. Rows:", nrow(dt), "Cols:", ncol(dt), "\n")

# --- 3. Data Preparation (All Years) for POOLED REGRESSION ---
cat("--- Preparing data for pooled regression ---\n")
if (!is.data.table(dt)) dt <- as.data.table(dt) # Ensure it's a data.table

# Ensure necessary columns are present and lowercase (as per Stata file inspection)
original_names <- names(dt)
setnames(dt, tolower(original_names))

required_cols <- c("year", "met2013", "incwage", "uhrswork", "wkswork1", 
                   "age", "sex", "race", "educ", "ind", "occ", "perwt")
missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns from Stata data after lowercasing:", paste(missing_cols, collapse = ", ")))
}

# Filter for sample selection criteria
dt <- dt[incwage > 0 & uhrswork > 0 & wkswork1 > 0 & age >= 16 & age <= 64 & perwt > 0]
if (nrow(dt) == 0) stop("No data after initial filtering (wages, hours, age, perwt).")

dt[, lnw := log(incwage / (uhrswork * wkswork1))] # Calculate log hourly wage
dt <- dt[is.finite(lnw)] # Clean Inf/NaN in lnw
if (nrow(dt) == 0) stop("No data after lnw calculation and finite filtering.")

# MET2013: ensure it's character and handle NAs/zeros/empty strings
dt[, met2013 := as.character(met2013)]
dt <- dt[!is.na(met2013) & met2013 != "0" & met2013 != "" & !grepl("^[0]+$", met2013)]
if (nrow(dt) == 0) stop("No data after MET2013 cleaning.")

# Year: ensure it's integer first, then factor for fixed effects
dt[, year := as.integer(year)]
dt[, year_fe := factor(year)]

# Create the MANUAL MSA-YEAR INTERACTION factor
dt[, msa_year_interaction_fe := factor(paste(met2013, year, sep = "_"))]
cat("Sample of created msa_year_interaction_fe levels:", paste(head(levels(dt$msa_year_interaction_fe), 5), collapse=", "), "...\n")

# Basic factors for controls
dt[, sex_fe := factor(sex)]
dt[, race_fe := factor(race)]
dt[, educ_fe := factor(educ)]
# Add factor versions of IND and OCC (not used in simplified model controls, but good to have)
if (!("ind" %in% names(dt))) dt[, ind := 0] # Placeholder if missing
if (!("occ" %in% names(dt))) dt[, occ := 0] # Placeholder if missing
dt[, ind_fe := factor(ind)]
dt[, occ_fe := factor(occ)]

cat("Data preparation complete. Rows after filtering:", nrow(dt), "\n")
if (nrow(dt) == 0) {
  stop("No data remaining after full preparation. Check input data and filtering criteria.")
}

# --- 4. Pooled Regression Model (SIMPLIFIED Specification) ---
cat("--- Estimating Pooled Regression Model (SIMPLIFIED Specification) ---\n")

# Model: year_fe absorbs common year trends (delta_t)
#        msa_year_interaction_fe absorbs MSA-specific deviations from year trends (theta_mt)
model_formula_pooled <- lnw ~ age + I(age^2) + sex_fe + race_fe + educ_fe | year_fe + msa_year_interaction_fe
cat("Using formula:", deparse(model_formula_pooled), "\n")

start_time_feols <- Sys.time()
model_pooled <- feols(
    model_formula_pooled, 
    data = dt, 
    weights = ~perwt, 
    ssc = ssc(adj = FALSE, fixef.K = "none")
)
end_time_feols <- Sys.time()
cat("feols model estimation time:", format(end_time_feols - start_time_feols), "\n")
print(summary(model_pooled)) # Good to see summary of main model

# --- 5. Extract MSA-YEAR Interaction Fixed Effects (theta_mt) ---
cat("--- Extracting MSA-YEAR Interaction Fixed Effects (theta_mt) ---\n")

fe_pooled_effects <- fixef(model_pooled, names.as.factor = TRUE)
if (!("msa_year_interaction_fe" %in% names(fe_pooled_effects))){
    cat("ERROR: 'msa_year_interaction_fe' not found in fixef() output names!\n")
    cat("Available FE names from fixef():", paste(names(fe_pooled_effects), collapse=", "), "\n")
    stop("Could not find 'msa_year_interaction_fe' in the extracted fixed effects list. Check model and FE name.")
}
theta_mt_raw_values <- fe_pooled_effects$msa_year_interaction_fe

if (is.null(theta_mt_raw_values) || length(theta_mt_raw_values) == 0) {
  stop("Extracted 'msa_year_interaction_fe' fixed effects are NULL or empty.")
}

theta_dt <- data.table(
  id = names(theta_mt_raw_values), # Should be "MET2013_YEAR"
  fe_raw = theta_mt_raw_values
)

cat("Raw id from fixef names (head) - expecting MET2013_YEAR format:\n"); print(head(theta_dt$id))
# Parse the id string "MET2013_YEAR"
# Use fill=NA in tstrsplit for robustness, though IDs should be well-formed.
theta_dt[, c("met2013_parsed", "year_parsed_char") := tstrsplit(id, "_", fixed=TRUE, fill=NA)]
theta_dt[, year := as.integer(year_parsed_char)]
theta_dt[, met2013 := as.character(met2013_parsed)]

# Filter out any rows where parsing failed (met2013 or year is NA)
original_row_count_theta <- nrow(theta_dt)
theta_dt <- theta_dt[!is.na(met2013) & !is.na(year)]
if(nrow(theta_dt) < original_row_count_theta){
    cat(sprintf("Warning: %d rows removed from theta_dt due to NA in met2013 or year after parsing.\n", original_row_count_theta - nrow(theta_dt)))
}

if (nrow(theta_dt) > 0) {
    theta_dt <- theta_dt[, .(id, met2013, year, fe_raw)] # Keep essential columns
} else {
    stop("theta_dt is empty after parsing ids and filtering NAs. Check FE names and tstrsplit.")
}
    
cat("theta_dt after tstrsplit, type conversion, and NA filter (head):\n"); print(head(theta_dt))
str(theta_dt[, .(met2013, year)])

theta_dt[, se_fe := NA_real_] # Placeholder for SEs of these specific FEs
cat("MSA-Year interaction fixed effects extracted and parsed. Count:", nrow(theta_dt), "\n")

# --- 6. Normalization of theta_mt (MSA-Year Fixed Effects) ---
cat("--- Normalizing MSA-Year Fixed Effects (theta_mt) using median MSA in median year ---\n")

valid_years_in_theta_dt <- sort(unique(theta_dt$year))
if (length(valid_years_in_theta_dt) == 0) {
    stop("No valid years in theta_dt to proceed with normalization.")
}
cat("Unique sorted valid years from theta_dt for median calculation:", paste(valid_years_in_theta_dt, collapse=", "), "\n")

median_year_for_norm <- as.integer(stats::median(valid_years_in_theta_dt))
cat(sprintf("Calculated median year for normalization from theta_dt: %d\n", median_year_for_norm))

theta_median_year <- theta_dt[year == median_year_for_norm]
if (nrow(theta_median_year) == 0) {
    cat(sprintf("Warning: No fixed effects (theta_mt) found for the median year %d. Using raw FEs for normalization.\n", median_year_for_norm))
    theta_dt[, fe_adj := fe_raw]
} else {
    median_fe_in_median_year <- median(theta_median_year$fe_raw, na.rm = TRUE)
    cat(sprintf("Median raw FE (theta_mt) in year %d: %f\n", median_year_for_norm, median_fe_in_median_year))
    
    theta_median_year[, diff_from_median := abs(fe_raw - median_fe_in_median_year)]
    setorder(theta_median_year, diff_from_median, met2013) # Tie-break by met2013 code
    reference_candidate <- head(theta_median_year, 1)
    
    if (nrow(reference_candidate) == 1 && !is.na(reference_candidate$fe_raw)) {
        reference_met2013_val <- reference_candidate$met2013 
        reference_year_val <- reference_candidate$year 
        ref_fe_value <- reference_candidate$fe_raw
        cat(sprintf("Using reference for theta_mt: MET2013=%s, Year=%d. Raw FE value: %f\n", 
                    reference_met2013_val, reference_year_val, ref_fe_value))
        theta_dt[, fe_adj := fe_raw - ref_fe_value]
    } else {
        cat(sprintf("Warning: Could not uniquely identify a reference MSA in median year %d or ref_fe_value is NA. Using raw FEs (theta_mt) as adjusted.\n", median_year_for_norm))
        theta_dt[, fe_adj := fe_raw]
    }
}
cat("theta_dt after normalization (head) - checking fe_adj:\n")
if (nrow(theta_dt) > 0) print(head(theta_dt[, .(met2013, year, fe_raw, fe_adj)])) else cat("theta_dt is empty, cannot print head after normalization.\n")

# --- 7. Add Sample Sizes --- 
cat("--- Calculating sample sizes for each MSA-Year cell ---\n")
# Ensure dt_for_counts uses the same types for met2013 (character) and year (integer) as in theta_dt
dt_for_counts <- dt[, .(met2013 = as.character(met2013), year = as.integer(year))] # perwt is not needed for .N
sample_sizes <- dt_for_counts[, .N, by = .(met2013, year)] 
setnames(sample_sizes, "N", "n_obs")
cat("sample_sizes calculated (head):\n"); print(head(sample_sizes)); str(sample_sizes)

if(nrow(theta_dt) == 0) stop("theta_dt is empty before merging with sample_sizes. Cannot proceed.")

theta_dt_final <- merge(theta_dt, sample_sizes, by = c("met2013", "year"), all.x = TRUE)

if(any(is.na(theta_dt_final$n_obs))){
    cat("WARNING: NA values in n_obs after merging sample_sizes! This indicates a mismatch for some MET2013-Year combinations that had FEs.\n")
    print(head(theta_dt_final[is.na(n_obs), .(id, met2013, year, .N)])) # Show problematic FEs from theta_dt_final
    # To see which original FEs from theta_dt didn't get a match:
    # matched_ids <- theta_dt_final[!is.na(n_obs), unique(id)]
    # print(head(theta_dt[!id %in% matched_ids]))
    theta_dt_final[is.na(n_obs), n_obs := 0] # Set to 0 as a fallback, but investigate warning if it occurs.
} else {
    cat("Merge with sample_sizes successful, no NAs in n_obs initially.\n")
}
    
cat("theta_dt_final after merging sample_sizes (head) - checking n_obs:\n"); print(head(theta_dt_final[, .(met2013, year, fe_adj, n_obs)])); str(theta_dt_final[, .(met2013, year, n_obs)])

# --- 8. Save Results ---
cat("--- Saving pooled MSA-Year premia ---\n")
if (!("fe_adj" %in% names(theta_dt_final))) { 
    cat("Warning: 'fe_adj' column not found in theta_dt_final. Defaulting theta_normalized to fe_raw.\n")
    if ("fe_raw" %in% names(theta_dt_final)) theta_dt_final[, fe_adj := fe_raw] else stop("Neither fe_adj nor fe_raw found!")
}
output_data <- theta_dt_final[, .(met2013, year, theta_raw = fe_raw, theta_normalized = fe_adj, se_theta = se_fe, n_observations = n_obs)]
cat("Final output_data to be saved (head):\n"); print(head(output_data)); str(output_data)

# Final checks before saving
critical_cols <- c("met2013", "year", "theta_normalized", "n_observations")
na_counts <- sapply(output_data[, ..critical_cols], function(x) sum(is.na(x)))
if(any(na_counts > 0)){
    cat("CRITICAL WARNING: NA values found in key columns of output_data before saving!\n")
    print(na_counts[na_counts > 0])
}
num_zero_obs <- sum(output_data$n_observations == 0, na.rm = TRUE)
if(nrow(output_data) > 0 && num_zero_obs > 0){
    cat(sprintf("WARNING: %d rows (%.1f%%) in output_data have n_observations == 0!\n", 
                num_zero_obs, 100*num_zero_obs/nrow(output_data)))
    if(num_zero_obs > 0.9 * nrow(output_data)){
         cat("CRITICAL WARNING: More than 90% of observations have n_observations == 0! This is highly problematic.\n")
    }
} else if (nrow(output_data) == 0) {
    cat("WARNING: output_data is empty before saving!\n")
}

fwrite(output_data, output_csv_pooled)
cat(paste("Pooled premia data saved to:", output_csv_pooled, "\n"))

cat("--- 03_process_acs.R: Script Finished ---\n") 