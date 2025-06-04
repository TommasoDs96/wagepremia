library(targets)
tar_option_set(packages = c("ipumsr","fixest","data.table","tidyverse","arrow","curl"))

list(
  tar_target(acs_ddi, read_ipums_ddi(Sys.glob("data/extracts/*ACS_Wage_Panel*.xml"))),
  tar_target(year, 2005:2024),
  tar_target(year_data, {
      dt <- as.data.table(read_ipums_micro(acs_ddi, select_cases = YEAR == year))
      # ...  # identical to process_year() above
      # Placeholder for the rest of process_year() logic
      # This part needs to be filled in based on 03_process_acs.R
      # For now, let's assume dt is processed and returned as 'out'
      # dt[, wage_hr := INCWAGE / (WKSWORK1 * UHRSWORK)]
      # dt <- dt[wage_hr > 0 & is.finite(wage_hr)]
      # dt[, lnw := log(wage_hr)]
      # model <- feols(
      #   lnw ~ AGE + I(AGE^2) + factor(SEX) + factor(RACE) +
      #          factor(EDUC) + factor(IND) + factor(OCC) |
      #          MET2013,
      #   weights = PERWT, cluster = ~MET2013, data = dt)
      # fe  <- fixef(model)$MET2013 |> as.data.table(keep.rownames = "MET2013")
      # fe[, `:=`(year = year,
      #           se   = attr(model$fixef, "se")$MET2013)]
      # raw <- dt[, .(
      #             raw_lnw = weighted.mean(lnw, w = PERWT),
      #             N       = .N), by = MET2013]
      # out <- merge(fe, raw, by = "MET2013")
      # return(out) # or fwrite and return path
      # For demonstration, returning a simple data.table structure
      # Actual implementation should replicate the full logic from 03_process_acs.R
      
      # --- REPLICATING 03_process_acs.R process_year() logic --- 
      dt[, wage_hr := INCWAGE / (WKSWORK1 * UHRSWORK)]
      dt <- dt[wage_hr > 0 & is.finite(wage_hr)]
      dt[, lnw := log(wage_hr)]

      model <- feols(
        lnw ~ AGE + I(AGE^2) + factor(SEX) + factor(RACE) +
               factor(EDUC) + factor(IND) + factor(OCC) |
               MET2013,
        weights = PERWT, cluster = ~MET2013, data = dt)

      # Extract fixed effects and their standard errors correctly
      fixef_results <- fixef(model, se.fixef = TRUE)
      estimates <- fixef_results$MET2013
      std_errors <- attr(estimates, "se")

      fe_table <- data.table(
        MET2013 = names(estimates),
        V1 = estimates, # This will be used for fe_adj_lnw
        se = std_errors,
        year = year # Note: 'year' is the iterator from tar_map
      )

      raw <- dt[, .(
                  raw_lnw = weighted.mean(lnw, w = PERWT),
                  N       = .N), by = MET2013]
      out <- merge(fe_table, raw, by = "MET2013")
      # Instead of fwrite, we return the data.table for rbindlist
      return(out)
      # --- END OF REPLICATED LOGIC ---
  }, pattern = map(year)),
  tar_target(panel, rbindlist(year_data)),
  tar_target(cpi, {
    # Replicating CPI logic from 05_deflate.R
    if (!file.exists("data/CPI.csv")) {
      curl_download("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current",
                    "data/CPI_raw.txt")
      cpi_raw_dt <- fread("data/CPI_raw.txt", fill = TRUE)
      cpi_dt <- cpi_raw_dt[series_id == "CUUR0000SA0" & period == "M13"]
      cpi_dt[, year := as.integer(substr(date, 1, 4))]
      fwrite(cpi_dt[, .(year, cpi = value)], "data/CPI.csv")
    }
    fread("data/CPI.csv")
  }),
  tar_target(final, {
    # Replicating final merge and select from 05_deflate.R
    # Note: In 05_deflate.R, 'V1' from fixef output is renamed to 'fe_adj_lnw'
    # Assuming 'fe' in year_data already has 'fe_adj_lnw' or equivalent (e.g., 'V1' or similar from fixef)
    # Let's assume the output of fixef directly is 'fe_lnw' for clarity if not V1
    # The merge in 03_process_acs.R results in 'V1' from fixef output.
    # So panel will have 'V1'. Let's rename it here before merging with CPI.
    setnames(panel, "V1", "fe_adj_lnw") # Ensure this column name is correct
    merged_panel <- merge(panel, cpi, by = "year", all.x = TRUE)
    merged_panel[, `:=`(fe_adj_real = fe_adj_lnw / cpi, 
                       raw_real = raw_lnw / cpi)]
    # Selecting specific columns as per user's tar_target(final, ...)
    merged_panel[, .(MET2013, year, fe_adj_lnw, se, raw_lnw, N, fe_adj_real, raw_real)] 
    # Added N as it was in the panel from 03_process_acs.R and likely useful
  }),
  tar_target(out_csv, fwrite(final, "data/output/msa_wage_premia_2005_2024.csv"))
) 