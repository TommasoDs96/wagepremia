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

process_and_save_model_output <- function(model_type_suffix, cpi_data) {
  cat(sprintf("Processing output for model type: %s\n", model_type_suffix))
  
  file_pattern <- sprintf("data/tidy/acs/msa_*_%s.csv", model_type_suffix)
  files <- Sys.glob(file_pattern)
  
  if (length(files) == 0) {
    warning(sprintf("No files found for pattern: %s. Skipping this model type.", file_pattern))
    return(invisible(NULL))
  }
  
  panel <- rbindlist(lapply(files, fread))
  
  if (nrow(panel) == 0) {
    warning(sprintf("No data loaded for model type: %s after reading files. Skipping.", model_type_suffix))
    return(invisible(NULL))
  }
  
  # Ensure 'year' column exists for merging with CPI
  if (!"year" %in% names(panel)) {
    stop(sprintf("'year' column not found in the panel data for model type: %s", model_type_suffix))
  }

  panel <- merge(panel, cpi_data, by = "year", all.x = TRUE)
  
  # V1 is the estimate from feols, raw_lnw is the raw log wage
  panel[, `:=`(fe_adj_real = V1 / cpi, 
                 raw_real = raw_lnw / cpi)]
  setnames(panel, "V1", "fe_adj_lnw") # V1 contains the fixed effect estimate
  
  output_filename <- sprintf("data/output/msa_wage_premia_2005_2023_%s.csv", model_type_suffix)
  fwrite(panel, output_filename)
  cat(sprintf("Saved deflated data for %s model to: %s\n", model_type_suffix, output_filename))
  invisible(TRUE)
}

dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# Process for simplified model
process_and_save_model_output(model_type_suffix = "simplified", cpi_data = cpi)

# Process for full model
process_and_save_model_output(model_type_suffix = "full", cpi_data = cpi)

cat("All model types processed by 05_deflate.R.\n") 