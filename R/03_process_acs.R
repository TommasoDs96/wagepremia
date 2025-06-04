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

process_year <- function(yr, all_data, model_type = "simplified") {
  cat(sprintf("Processing year: %d, Model: %s\n", yr, model_type))
  dt <- all_data[year == yr] # Use lowercase 'year' for filtering
  
  # Ensure met2013 is character for consistent merging/grouping
  if (exists("met2013", where = dt)) {
    dt[, met2013 := as.character(met2013)]
  } else {
    warning(sprintf("Column 'met2013' not found in year %d. Cannot run model.", yr))
    return(invisible(NULL))
  }

  if (nrow(dt) == 0) {
    warning(sprintf("No data found for year %d after filtering. Skipping.", yr))
    return(invisible(NULL))
  }
  
  # --- DEBUG: Print data dimensions and factor cardinalities for the first processed year in the loop --- 
  if (yr == years_in_data[1]) { 
    cat(sprintf("--- Debug info for year %d (immediately before feols) ---\n", yr))
    cat(sprintf("Number of rows in dt: %d\n", nrow(dt)))
    cat(sprintf("Number of unique met2013: %d\n", length(unique(dt$met2013))))
    cat(sprintf("Number of unique educ: %d\n", length(unique(dt$educ))))
    cat(sprintf("Number of unique ind: %d\n", length(unique(dt$ind))))
    cat(sprintf("Number of unique occ: %d\n", length(unique(dt$occ))))
    cat(sprintf("Number of unique sex: %d\n", length(unique(dt$sex))))
    cat(sprintf("Number of unique race: %d\n", length(unique(dt$race))))
    cat("-----------------------------------------------------------\n")
  }
  # --- END DEBUG ---

  # Use lowercase variable names from Stata file
  dt[, wage_hr := incwage / (wkswork1 * uhrswork)]
  dt <- dt[wage_hr > 0 & is.finite(wage_hr)]
  dt[, lnw := log(wage_hr)]

  if (nrow(dt) < 2 || (exists("met2013", where = dt) && length(unique(dt$met2013)) < 2)) {
    warning(sprintf("Not enough data or met2013 groups for year %d (%s model) after wage filtering. Skipping model.", yr, model_type))
    return(invisible(NULL))
  }
  
  if (model_type == "simplified") {
    cat(sprintf("Running SIMPLIFIED formula for year %d: lnw ~ age + I(age^2) + factor(sex) + factor(race) + factor(educ) | met2013\n", yr))
    model <- feols(
      lnw ~ age + I(age^2) + factor(sex) + factor(race) +
             factor(educ) |
             met2013,
      weights = dt$perwt, cluster = ~met2013, data = dt)
  } else if (model_type == "full") {
    cat(sprintf("Running FULL formula for year %d: lnw ~ age + I(age^2) + factor(sex) + factor(race) + factor(educ) + factor(ind) + factor(occ) | met2013\n", yr))
    # Ensure 'ind' and 'occ' exist, otherwise skip this model for the year or fall back
    if (!("ind" %in% names(dt) && "occ" %in% names(dt))) {
        warning(sprintf("Columns 'ind' or 'occ' not found for year %d. Skipping FULL model.", yr))
        return(invisible(NULL))
    }
    model <- feols(
      lnw ~ age + I(age^2) + factor(sex) + factor(race) +
             factor(educ) + factor(ind) + factor(occ) |
             met2013,
      weights = dt$perwt, cluster = ~met2013, data = dt)
  } else {
    stop(sprintf("Unknown model_type: %s", model_type))
  }

  fixef_results <- fixef(model, se.fixef = TRUE)
  estimates <- fixef_results$met2013 
  std_errors <- attr(estimates, "se")

  fe <- data.table(
    met2013 = names(estimates), # names() always returns character
    V1 = estimates, 
    se = std_errors,
    year = yr 
  )
  
  raw_calc_possible = nrow(dt) > 0
  if (raw_calc_possible) {
    raw <- dt[, .(
                raw_lnw = weighted.mean(lnw, w = perwt, na.rm = TRUE), 
                N       = .N), by = met2013] # met2013 in dt is now character
    
    # --- DEBUG: Check types before merge for the first year ---
    if (yr == years_in_data[1]) {
      cat("--- Type check before merge for year ", yr, " ---\n")
      cat("class(fe$met2013): ", class(fe$met2013), "\n")
      cat("class(raw$met2013): ", class(raw$met2013), "\n")
      cat("Sample fe$met2013: ", head(fe$met2013, 3), "\n")
      cat("Sample raw$met2013: ", head(raw$met2013, 3), "\n")
      cat("-----------------------------------------------\n")
    }
    # --- END DEBUG ---
    
    out <- merge(fe, raw, by = "met2013", all.x = TRUE) 
  } else {
    raw_placeholder <- data.table(met2013=fe$met2013, raw_lnw=as.double(NA), N=as.integer(NA))
    out <- merge(fe, raw_placeholder, by = "met2013", all.x = TRUE)
  }
  
  out[, model_type := model_type]
  fwrite(out, file = sprintf("data/tidy/acs/msa_%d_%s.csv", yr, model_type))
  cat(sprintf("Finished processing and saved data for year: %d, Model: %s\n", yr, model_type))
  invisible(TRUE)
}

for (year_val in years_in_data) { # Loop over years found in the Stata file
  process_year(year_val, full_acs_data, model_type = "simplified")
  # cat(sprintf("Attempting FULL model for year %d. This might be memory intensive.\n", year_val))
  # process_year(year_val, full_acs_data, model_type = "full") # Commented out/removed full model call
}
cat("All years processed for the simplified model.\n") # Updated cat message 