# R/08_rank_wage_premia.R

source("R/00_packages.R") # Load common packages (data.table, dplyr, readr)

# --- 1. Define File Paths ---
cat("--- Defining file paths ---\n")
wage_premia_file <- "data/output/msa_wage_premia_2005_2023_simplified.csv"
fertility_data_file <- "/Users/tommasodesanto/Desktop/Projects/Fertility/Codes/R/msa_panel_output/msa_panel_complete.csv"
output_dir <- "data/output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# --- 2. Load Data ---
cat("--- Loading data ---\n")

if (!file.exists(wage_premia_file)) {
  stop(paste("Wage premia file not found:", wage_premia_file))
}
wage_data <- fread(wage_premia_file)
cat("Wage premia data loaded.\n")

if (!file.exists(fertility_data_file)) {
  stop(paste("Fertility data (for MSA names) file not found:", fertility_data_file))
}
fertility_data_for_names <- fread(fertility_data_file)
cat("Fertility data for MSA names loaded.\n")

# --- 3. Prepare MSA Name Lookup ---
cat("--- Preparing MSA name lookup ---\n")
# Keep only distinct GEOID and NAME from fertility data
# Ensure GEOID is character for merging with met2013
msa_name_lookup <- unique(fertility_data_for_names[, .(GEOID = as.character(GEOID), NAME)])
if (any(duplicated(msa_name_lookup$GEOID))) {
  warning("Duplicate GEOIDs found in fertility data name lookup. Taking the first NAME associated.")
  msa_name_lookup <- msa_name_lookup[!duplicated(GEOID)]
}
setnames(msa_name_lookup, "GEOID", "met2013") # Rename to match wage_data

# Also get population data for plotting
msa_pop_lookup <- unique(fertility_data_for_names[, .(met2013 = as.character(GEOID), year, total_population)])
if (any(duplicated(msa_pop_lookup[,.(met2013, year)]))) {
    warning("Duplicate met2013-year combinations in population lookup. Taking first population entry.")
    msa_pop_lookup <- msa_pop_lookup[!duplicated(msa_pop_lookup[,.(met2013, year)])]
}

# --- 4. Function to Get and Save Ranked Data ---
get_and_save_ranked_premia <- function(data, year_to_filter, name_lookup, pop_lookup, output_file_path, output_plot_dir) {
  cat(sprintf("\n--- Processing rankings and plots for year: %d ---\n", year_to_filter))
  
  year_data <- data[year == year_to_filter]
  
  if (nrow(year_data) == 0) {
    cat(sprintf("No data found for year %d. Skipping ranking.\n", year_to_filter))
    return(invisible(NULL))
  }
  
  # Merge with MSA names
  year_data_named <- merge(year_data, name_lookup, by = "met2013", all.x = TRUE)
  year_data_named[is.na(NAME), NAME := paste("Unknown MSA (Code:", met2013, ")")]
  
  # Merge population data for the specific year
  pop_data_for_year <- pop_lookup[year == year_to_filter, .(met2013, total_population)]
  year_data_named_pop <- merge(year_data_named, pop_data_for_year, by = "met2013", all.x = TRUE)
  year_data_named_pop[is.na(total_population), total_population := 0] # Handle if pop is missing

  # Ensure fe_adj_lnw is numeric for ordering
  year_data_named_pop[, fe_adj_lnw := as.numeric(fe_adj_lnw)]
  
  # Sort by wage premium
  ranked_data_pop <- year_data_named_pop[order(-fe_adj_lnw)] # Descending for top
  
  top_10_df <- head(ranked_data_pop[, .(MSA_Code = met2013, MSA_Name = NAME, Log_Wage_Premium = fe_adj_lnw, Population = total_population)], 10)
  
  bottom_10_from_desc_sort_pop <- tail(ranked_data_pop[, .(MSA_Code = met2013, MSA_Name = NAME, Log_Wage_Premium = fe_adj_lnw, Population = total_population)], 10)
  bottom_10_df <- bottom_10_from_desc_sort_pop[order(Log_Wage_Premium)]
  
  # Print to console
  cat(sprintf("\nTop 10 MSAs by Log-Wage Premium in %d:\n", year_to_filter))
  print(top_10_df)
  cat(sprintf("\nBottom 10 MSAs by Log-Wage Premium in %d:\n", year_to_filter))
  print(bottom_10_df)
  
  # Save to file (text output)
  sink(output_file_path)
  cat(sprintf("Analysis of MSA Log-Wage Premia for Year: %d\n", year_to_filter))
  cat("=======================================================\n\n")
  cat("Top 10 MSAs by Log-Wage Premium (Population shown in plots):\n")
  print(top_10_df[, .(MSA_Code, MSA_Name, Log_Wage_Premium)]) # Print without pop to text file for brevity
  cat("\nBottom 10 MSAs by Log-Wage Premium (Population shown in plots):\n")
  print(bottom_10_df[, .(MSA_Code, MSA_Name, Log_Wage_Premium)]) # Print without pop to text file for brevity
  sink()
  cat(sprintf("\nRanked premia text for %d saved to: %s\n", year_to_filter, output_file_path))

  # Create and save plots
  # Top 10 Plot
  plot_top_10 <- ggplot(top_10_df, aes(x = reorder(MSA_Name, -Log_Wage_Premium), y = Log_Wage_Premium)) +
    geom_col(aes(fill = Population), alpha = 0.8) +
    geom_text(aes(label = scales::comma(Population, accuracy=1)), hjust = -0.1, size = 3, color = "black") +
    scale_fill_viridis_c(name = "Population", labels = scales::comma) +
    coord_flip() +
    labs(title = paste("Top 10 MSAs by Log-Wage Premium,", year_to_filter),
         x = "MSA Name", y = "Log-Wage Premium (fe_adj_lnw)",
         caption = "Bars colored by population. Population numbers shown.") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
  ggsave(filename = file.path(output_plot_dir, paste0("ranked_msa_premia_top10_pop_", year_to_filter, ".png")),
         plot = plot_top_10, width = 12, height = 7, dpi = 300)
  cat(sprintf("Saved Top 10 plot for %d.\n", year_to_filter))

  # Bottom 10 Plot
  plot_bottom_10 <- ggplot(bottom_10_df, aes(x = reorder(MSA_Name, Log_Wage_Premium), y = Log_Wage_Premium)) +
    geom_col(aes(fill = Population), alpha = 0.8) +
    geom_text(aes(label = scales::comma(Population, accuracy=1)), hjust = -0.1, size = 3, color = "black") +
    scale_fill_viridis_c(name = "Population", labels = scales::comma) +
    coord_flip() +
    labs(title = paste("Bottom 10 MSAs by Log-Wage Premium,", year_to_filter),
         x = "MSA Name", y = "Log-Wage Premium (fe_adj_lnw)",
         caption = "Bars colored by population. Population numbers shown.") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
  ggsave(filename = file.path(output_plot_dir, paste0("ranked_msa_premia_bottom10_pop_", year_to_filter, ".png")),
         plot = plot_bottom_10, width = 12, height = 7, dpi = 300)
  cat(sprintf("Saved Bottom 10 plot for %d.\n", year_to_filter))
  
  invisible(TRUE)
}

# --- 5. Process and Save for 2005 and 2023 ---
wage_data[, met2013 := as.character(met2013)] # Ensure met2013 is character for merging

get_and_save_ranked_premia(
  data = wage_data, 
  year_to_filter = 2005, 
  name_lookup = msa_name_lookup, 
  pop_lookup = msa_pop_lookup,
  output_file_path = file.path(output_dir, "ranked_msa_premia_2005.txt"),
  output_plot_dir = output_dir
)

get_and_save_ranked_premia(
  data = wage_data, 
  year_to_filter = 2023, 
  name_lookup = msa_name_lookup, 
  pop_lookup = msa_pop_lookup,
  output_file_path = file.path(output_dir, "ranked_msa_premia_2023.txt"),
  output_plot_dir = output_dir
)

cat("\n--- Ranking script finished ---\n")

# --- 6. Create Combined Scatter Plot for Top/Bottom 10 across 2005 and 2023 ---
cat("\n--- Creating combined scatter plot for 2005 & 2023 Top/Bottom 10 MSAs ---\n")

# Function to get top/bottom N for a given year (re-using some logic from above for clarity here)
get_ranked_data_for_year <- function(full_wage_data, target_year, name_lkp, pop_lkp, n_msas = 10) {
  year_data_subset <- full_wage_data[year == target_year]
  
  if (nrow(year_data_subset) == 0) {
    cat(sprintf("No wage data found for year %d for combined plot. Skipping.\n", target_year))
    return(NULL)
  }
  
  year_data_named <- merge(year_data_subset, name_lkp, by = "met2013", all.x = TRUE)
  year_data_named[is.na(NAME), NAME := paste("Unknown MSA (Code:", met2013, ")")]
  
  pop_data_for_year_subset <- pop_lkp[year == target_year, .(met2013, total_population)]
  year_data_final <- merge(year_data_named, pop_data_for_year_subset, by = "met2013", all.x = TRUE)
  year_data_final[is.na(total_population), total_population := 0] # Handle if pop is missing, though should ideally be present
  year_data_final[, fe_adj_lnw := as.numeric(fe_adj_lnw)]
  
  ranked_data_all <- year_data_final[order(-fe_adj_lnw)]
  
  top_n <- head(ranked_data_all, n_msas)
  top_n[, group := paste0("Top ", n_msas)]
  
  # Get bottom N from the full ranked list for the year
  bottom_n_from_desc_sort <- tail(ranked_data_all, n_msas)
  bottom_n <- bottom_n_from_desc_sort[order(fe_adj_lnw)] # order them by premium for consistency if needed, though not strictly for plotting points
  bottom_n[, group := paste0("Bottom ", n_msas)]
  
  return(rbind(top_n, bottom_n))
}

# Get data for 2005
data_2005_ranked <- get_ranked_data_for_year(
  full_wage_data = wage_data,
  target_year = 2005,
  name_lkp = msa_name_lookup,
  pop_lkp = msa_pop_lookup,
  n_msas = 10
)
if (!is.null(data_2005_ranked)) {
  data_2005_ranked[, plot_year := "2005"]
}

# Get data for 2023
data_2023_ranked <- get_ranked_data_for_year(
  full_wage_data = wage_data,
  target_year = 2023,
  name_lkp = msa_name_lookup,
  pop_lkp = msa_pop_lookup,
  n_msas = 10
)
if (!is.null(data_2023_ranked)) {
  data_2023_ranked[, plot_year := "2023"]
}

# Combine
if (!is.null(data_2005_ranked) && !is.null(data_2023_ranked)){
  combined_ranked_data <- rbind(data_2005_ranked, data_2023_ranked)
  combined_ranked_data[, Population_missing_flag := (total_population == 0)] # Flag if we used 0 for missing

  # Create the scatter plot
  scatter_plot_combined <- ggplot(combined_ranked_data, 
                                aes(x = total_population, y = fe_adj_lnw, 
                                    color = plot_year, shape = group)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_hline(yintercept = median(combined_ranked_data$fe_adj_lnw, na.rm=TRUE), linetype="dashed", color="grey50") +
    scale_x_log10(labels = scales::comma, name = "Total Population (Log Scale)") +
    scale_y_continuous(name = "Log-Wage Premium (fe_adj_lnw)") +
    scale_color_manual(values = c("2005" = "blue", "2023" = "red"), name = "Year") +
    scale_shape_manual(values = c("Top 10" = 16, "Bottom 10" = 17), name = "Rank Group") +
    labs(title = "MSA Wage Premia vs. Population (Top/Bottom 10)",
         subtitle = "Comparing 2005 and 2023",
         caption = "Points colored by year, shaped by rank group (Top/Bottom 10).") +
    theme_bw() + # White background theme
    theme(legend.position = "top") +
    ggrepel::geom_text_repel(data = subset(combined_ranked_data, Population_missing_flag | total_population > 15e6 | fe_adj_lnw > 1.8 | fe_adj_lnw < 0.6 ), 
                             aes(label = NAME), size = 2.5, box.padding = 0.5, max.overlaps = Inf) # Label some notable points

  # Save the plot
  plot_file_path_combined <- file.path(output_dir, "scatter_premia_pop_ranked_groups_2005_2023.png")
  ggsave(filename = plot_file_path_combined, plot = scatter_plot_combined, width = 12, height = 8, dpi = 300)
  cat(sprintf("\nCombined scatter plot saved to: %s\n", plot_file_path_combined))
  
} else {
  cat ("\nSkipping combined scatter plot as data for one or both years was missing.\n")
}

cat("\n--- Script R/08_rank_wage_premia.R finished successfully ---\n") 