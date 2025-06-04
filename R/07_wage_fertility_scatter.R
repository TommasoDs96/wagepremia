# R/07_wage_fertility_scatter.R

source("R/00_packages.R") # Load common packages, ensure ggplot2, data.table, dplyr, readr are there

# --- 1. Define File Paths ---
cat("--- Defining file paths ---\n")
wage_premia_file <- "data/output/msa_wage_premia_2005_2023_simplified.csv"
fertility_data_file <- "/Users/tommasodesanto/Desktop/Projects/Fertility/Codes/R/msa_panel_output/msa_panel_complete.csv"

output_plot_dir <- "data/output"
dir.create(output_plot_dir, showWarnings = FALSE, recursive = TRUE)

# --- 2. Load Data ---
cat("--- Loading data ---\n")

if (!file.exists(wage_premia_file)) {
  stop(paste("Wage premia file not found:", wage_premia_file))
}
wage_data <- fread(wage_premia_file)
cat("Wage premia data loaded. Columns: ", paste(names(wage_data), collapse=", "), "\n")

if (!file.exists(fertility_data_file)) {
  stop(paste("Fertility data file not found:", fertility_data_file))
}
fertility_data <- fread(fertility_data_file) # Assuming fread can handle it, or use read_csv if needed
cat("Fertility data loaded. Columns: ", paste(names(fertility_data), collapse=", "), "\n")

# --- 3. Data Preparation and Merging ---
cat("--- Preparing and merging data ---\n")

# Select relevant columns and rename for clarity if needed
# Wage data: met2013, year, fe_adj_real (real wage premium)
setnames(wage_data, "met2013", "GEOID") # Rename to match fertility data for merging
wage_data[, GEOID := as.character(GEOID)]
wage_data <- wage_data[, .(GEOID, year, fe_adj_real, N)] # Keep N for potential weighting or filtering

# Fertility data: GEOID, year, birth_rate_per_1000, pct_households_with_children
# Ensure GEOID is character in fertility data as well
fertility_data[, GEOID := as.character(GEOID)]
fertility_data <- fertility_data[, .(GEOID, year, birth_rate_per_1000, pct_households_with_children)]

# Merge data
merged_data <- merge(wage_data, fertility_data, by = c("GEOID", "year"), all = FALSE) # Inner join
cat(sprintf("Merged data has %d rows.\n", nrow(merged_data)))

if (nrow(merged_data) == 0) {
  stop("No common MSAs/years found after merging. Check GEOID/met2013 and year columns.")
}

# For a quick look, let's average by GEOID across years
# Alternative: use latest common year, or plot all points faceted by year or colored by year
plot_data_avg <- merged_data[, .(
  avg_fe_adj_real = mean(fe_adj_real, na.rm = TRUE),
  avg_birth_rate = mean(birth_rate_per_1000, na.rm = TRUE),
  avg_pct_hh_children = mean(pct_households_with_children, na.rm = TRUE),
  total_N = sum(N, na.rm = TRUE) # Sum of N from wage data as a potential size indicator
), by = GEOID]

cat(sprintf("Averaged data for plotting has %d MSAs.\n", nrow(plot_data_avg)))

if (nrow(plot_data_avg) == 0) {
  stop("No data available for plotting after averaging.")
}

# --- 4. Create Scatterplots ---
cat("--- Creating scatterplots ---\n")

# Plot 1: Wage Premia vs. Fertility Rate
p1 <- ggplot(plot_data_avg, aes(x = avg_birth_rate, y = avg_fe_adj_real)) +
  geom_point(aes(size = total_N), alpha = 0.6, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", na.rm = TRUE) +
  scale_size_continuous(name = "Total Observations (all years)", labels = scales::comma) +
  labs(
    title = "Wage Premia vs. Average Fertility Rate by MSA",
    subtitle = "Values averaged across 2005-2023. Wage premia are inflation-adjusted.",
    x = "Average Birth Rate per 1000 (2005-2023)",
    y = "Average Real Wage Premium (fe_adj_real, 2005-2023)",
    caption = "Source: ACS (Wage Premia) & User-provided Fertility Panel"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_plot_dir, "scatter_wage_premia_vs_fertility_rate.png"),
  plot = p1,
  width = 10, height = 7, dpi = 300, bg = "white"
)
cat("Saved scatter_wage_premia_vs_fertility_rate.png\n")

# Plot 2: Wage Premia vs. Share of Households with Kids
p2 <- ggplot(plot_data_avg, aes(x = avg_pct_hh_children, y = avg_fe_adj_real)) +
  geom_point(aes(size = total_N), alpha = 0.6, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
  scale_size_continuous(name = "Total Observations (all years)", labels = scales::comma) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) + # Assuming pct_households_with_children is 0-100 scale
  labs(
    title = "Wage Premia vs. Average Share of Households with Children by MSA",
    subtitle = "Values averaged across 2005-2023. Wage premia are inflation-adjusted.",
    x = "Average Pct. of Households with Children (2005-2023)",
    y = "Average Real Wage Premium (fe_adj_real, 2005-2023)",
    caption = "Source: ACS (Wage Premia) & User-provided Fertility Panel"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_plot_dir, "scatter_wage_premia_vs_hh_with_children.png"),
  plot = p2,
  width = 10, height = 7, dpi = 300, bg = "white"
)
cat("Saved scatter_wage_premia_vs_hh_with_children.png\n")

# --- 5. Yearly Regressions and Coefficient Plots ---
cat("--- Performing yearly regressions and plotting coefficients over time ---\n")

unique_years <- sort(unique(merged_data$year))
yearly_coeffs_list <- list()

for (yr in unique_years) {
  cat(sprintf("Processing year: %d for yearly regressions\n", yr))
  year_data <- merged_data[year == yr]
  
  # Ensure there's enough data and variance for regression, and N for weights
  if (nrow(year_data) > 5 && any(!is.na(year_data$N) & year_data$N > 0)) {
    
    # Regression 1: Wage Premia vs. Fertility Rate
    if (sum(!is.na(year_data$birth_rate_per_1000)) > 2 && sd(year_data$birth_rate_per_1000, na.rm = TRUE) > 0) {
      model1_fit <- tryCatch({
        lm(fe_adj_real ~ birth_rate_per_1000, data = year_data, weights = N)
      }, error = function(e) NULL)
      
      if (!is.null(model1_fit)) {
        tidy_model1 <- broom::tidy(model1_fit, conf.int = TRUE)
        coeff_model1 <- tidy_model1[tidy_model1$term == "birth_rate_per_1000", ]
        if (nrow(coeff_model1) > 0) {
          coeff_model1$year <- yr
          coeff_model1$metric <- "birth_rate_per_1000"
          yearly_coeffs_list[[paste0("br_", yr)]] <- coeff_model1
        }
      }
    } else {
      cat(sprintf("  Skipping birth_rate_per_1000 regression for year %d due to insufficient data/variance.\n", yr))
    }
    
    # Regression 2: Wage Premia vs. Share of Households with Kids
    if (sum(!is.na(year_data$pct_households_with_children)) > 2 && sd(year_data$pct_households_with_children, na.rm = TRUE) > 0) {
      model2_fit <- tryCatch({
        lm(fe_adj_real ~ pct_households_with_children, data = year_data, weights = N)
      }, error = function(e) NULL)
      
      if (!is.null(model2_fit)) {
        tidy_model2 <- broom::tidy(model2_fit, conf.int = TRUE)
        coeff_model2 <- tidy_model2[tidy_model2$term == "pct_households_with_children", ]
        if (nrow(coeff_model2) > 0) {
          coeff_model2$year <- yr
          coeff_model2$metric <- "pct_households_with_children"
          yearly_coeffs_list[[paste0("hhc_", yr)]] <- coeff_model2
        }
      }
    } else {
      cat(sprintf("  Skipping pct_households_with_children regression for year %d due to insufficient data/variance.\n", yr))
    }
  } else {
    cat(sprintf("  Skipping year %d for regressions due to insufficient overall data or missing weights.\n", yr))
  }
}

if (length(yearly_coeffs_list) > 0) {
  all_yearly_coeffs <- rbindlist(yearly_coeffs_list)
  
  # Plot for birth_rate_per_1000 coefficients
  coeffs_br <- all_yearly_coeffs[metric == "birth_rate_per_1000"]
  if(nrow(coeffs_br) > 0) {
    p_coeffs_br_time <- ggplot(coeffs_br, aes(x = year, y = estimate)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Coefficient of Birth Rate on Wage Premium Over Time",
        subtitle = "Yearly cross-sectional weighted OLS (fe_adj_real ~ birth_rate_per_1000, weights=N)",
        x = "Year",
        y = "Coefficient Estimate (Birth Rate per 1000)",
        caption = "Shaded area represents 95% confidence interval."
      ) +
      theme_minimal()
    ggsave(
      filename = file.path(output_plot_dir, "coefficient_birth_rate_vs_wage_premia_over_time.png"),
      plot = p_coeffs_br_time,
      width = 10, height = 7, dpi = 300, bg = "white"
    )
    cat("Saved coefficient_birth_rate_vs_wage_premia_over_time.png\n")
  } else {
    cat("No coefficient data for birth_rate_per_1000 to plot.\n")
  }
  
  # Plot for pct_households_with_children coefficients
  coeffs_hhc <- all_yearly_coeffs[metric == "pct_households_with_children"]
   if(nrow(coeffs_hhc) > 0) {
    p_coeffs_hhc_time <- ggplot(coeffs_hhc, aes(x = year, y = estimate)) +
      geom_line(color = "red") +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "red") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Coefficient of Pct. Households w/ Children on Wage Premium Over Time",
        subtitle = "Yearly cross-sectional weighted OLS (fe_adj_real ~ pct_households_with_children, weights=N)",
        x = "Year",
        y = "Coefficient Estimate (Pct. Households w/ Children)",
        caption = "Shaded area represents 95% confidence interval."
      ) +
      theme_minimal()
    ggsave(
      filename = file.path(output_plot_dir, "coefficient_hh_children_vs_wage_premia_over_time.png"),
      plot = p_coeffs_hhc_time,
      width = 10, height = 7, dpi = 300, bg = "white"
    )
    cat("Saved coefficient_hh_children_vs_wage_premia_over_time.png\n")
  } else {
    cat("No coefficient data for pct_households_with_children to plot.\n")
  }
  
} else {
  cat("No yearly coefficients were successfully estimated.\n")
}

cat("--- Scatterplot script finished ---\n") 