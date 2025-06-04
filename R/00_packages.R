pkgs <- c("ipumsr","fixest","data.table","tidyverse","arrow","curl", "haven")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE) 
