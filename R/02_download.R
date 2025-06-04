source("R/00_packages.R")
defs <- readRDS("R/extract_defs.rds")

submit_and_wait <- function(def) {
  # API Key will be read from .Renviron by ipumsr functions
  current_api_key <- Sys.getenv("IPUMS_API_KEY")
  if (!nzchar(current_api_key)) {
    stop("IPUMS_API_KEY is not set in the environment. Please set it (e.g., in .Renviron) and retry.")
  }
  cat("Using API Key from environment starting with:", substr(current_api_key, 1, 4),
      "... and ending with:", substr(current_api_key, nchar(current_api_key)-3, nchar(current_api_key)), "\n")
  
  sub  <- submit_extract(def) # Rely on ipumsr to find key via Sys.getenv or its internal logic
  wait_for_extract(sub)
}

dir.create("data/extracts", recursive = TRUE, showWarnings = FALSE)

acs_job <- submit_and_wait(defs$acs)
download_extract(acs_job, download_dir = "data/extracts", overwrite = TRUE)

## uncomment if CPS wanted
# cps_job <- submit_and_wait(defs$cps)
# download_extract(cps_job, download_dir = "data/extracts", overwrite = TRUE) 