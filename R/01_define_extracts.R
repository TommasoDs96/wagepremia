library(ipumsr)

acs_extract <- define_extract_micro(
  collection  = "usa",
  description = "ACS_Wage_Panel_2005_2023",
  samples     = paste0("us", 2005:2023, "a"),
  variables   = c("YEAR","SERIAL","PERWT",
                  "AGE","SEX","RACE","EDUC","IND","OCC",
                  "INCWAGE","WKSWORK1","UHRSWORK",
                  "MET2013","PUMA","STATEFIP"),
  data_format = "fixed_width"
)

cps_extract <- define_extract_cps(
  description = "CPS_ORG_Wage_Panel_2000_2025",
  samples     = paste0("cps", 2000:2025, "m"),
  variables   = c("YEAR","MONTH","CPSIDP","CPSOCCWT",
                  "AGE","SEX","RACE","EDUC",
                  "EARNWEEK","UHRSWORKT","HOURWAGE",
                  "METRO","METAREA"),
  data_format = "fixed_width"
)

saveRDS(list(acs = acs_extract, cps = cps_extract),
        file = "R/extract_defs.rds") 