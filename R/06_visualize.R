library(ggplot2); library(data.table); library(scales)

panel <- fread("data/output/msa_wage_premia_2005_2023_simplified.csv")

if (nrow(panel) > 0) {
  # Calculate yearly mean of fe_adj_lnw
  panel[, mean_fe_adj_lnw_year := mean(fe_adj_lnw, na.rm = TRUE), by = year]
  # Calculate demeaned fe_adj_lnw
  panel[, fe_adj_lnw_demeaned := fe_adj_lnw - mean_fe_adj_lnw_year]

  plot_obj_demeaned <- ggplot(panel, aes(year, fe_adj_lnw_demeaned, group = met2013)) +
    geom_line(alpha = 0.2) +
    theme_light(base_size = 14) +
    labs(title = "Year-Demeaned Skill-Adjusted Log-Wage Premia, 2005-2023 (Simplified Model)",
         y = "Demeaned ln-wage premium (log points from yearly mean)", 
         x = "Year") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
  
  ggsave("data/output/msa_premia_trends_simplified_lnw_demeaned.png", plot = plot_obj_demeaned, width = 10, height = 6)
  cat("Saved msa_premia_trends_simplified_lnw_demeaned.png to data/output/\n")

  # Original plot for fe_adj_lnw (as before)
  plot_obj_original_lnw <- ggplot(panel, aes(year, fe_adj_lnw, group = met2013)) + 
    geom_line(alpha = 0.2) +
    theme_light(base_size = 14) +
    labs(title = "Skill-Adjusted Log-Wage Premia, 2005-2023 (Simplified Model)", 
         y = "ln-wage premium (log points)", x = "Year") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) 
  
  ggsave("data/output/msa_premia_trends_simplified_lnw.png", plot = plot_obj_original_lnw, width = 10, height = 6) 
  cat("Saved msa_premia_trends_simplified_lnw.png to data/output/ (re-saved for consistency)\n")

} else {
  cat("Panel data is empty. No plot generated.\n")
}
cat("Visualization script finished.\n") 