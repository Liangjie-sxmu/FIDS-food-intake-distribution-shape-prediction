# Reproduce manuscript figure components and sensitivity LOCO table from aggregate inputs.

# Figure 1: distribution of survey-specific consumption probability and sigma.
shape <- read_input("shape", "survey_shape_summary.csv")
shape <- shape[final_estimable %in% TRUE]
for (what in c("p_bar", "sigma_log_amount")) {
  x <- shape[is.finite(get(what))]
  x[, value := if (what == "p_bar") 100 * get(what) else get(what)]
  summary <- x[, .(middle = median(value),
                    lower = quantile(value, 0.25),
                    upper = quantile(value, 0.75)), by = gdd19_group]
  x[, food_label := food_axis(gdd19_group)]
  summary[, food_label := food_axis(gdd19_group)]
  p <- ggplot(x, aes(value, food_label)) +
    geom_point(colour = "grey75", alpha = 0.50, size = 1,
               position = position_jitter(height = 0.11, width = 0)) +
    geom_segment(data = summary, inherit.aes = FALSE,
                 aes(x = lower, xend = upper, y = food_label, yend = food_label),
                 linewidth = 1.0, colour = "black") +
    geom_point(data = summary, inherit.aes = FALSE,
               aes(x = middle, y = food_label), shape = 23, size = 2,
               fill = "white", colour = "black") +
    labs(x = if (what == "p_bar") "Consumption probability (%)" else
           "Positive-amount dispersion, log-scale sigma", y = NULL) + plot_theme()
  ggsave(file.path(fig_dir, if (what == "p_bar")
    "Figure1A_consumption_probability.png" else "Figure1B_positive_amount_shape.png"),
    p, width = 7, height = 7.4, dpi = 300)
}

# Figure 3A: country-level sigma; Figure 3B is cross_country_consistency.png.
country_fig <- country[is.finite(sigma) & sigma > 0]
country_fig[, food_label := food_axis(food_group)]
country_colours <- c("Brazil" = "#0072B2", "Mexico" = "#D55E00",
                     "Lao PDR" = "#009E73", "Peru" = "#CC79A7",
                     "Saint Kitts and Nevis" = "#E69F00")
p <- ggplot(country_fig, aes(sigma, food_label, colour = country)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.4)) +
  scale_colour_manual(values = country_colours) +
  labs(x = "Country-level sigma", y = NULL, colour = "Country") + plot_theme()
ggsave(file.path(fig_dir, "country_level_shape_evidence.png"), p,
       width = 9, height = 7.8, dpi = 300)

# Figure 4: country-balanced mean absolute PAF error by food and outcome.
# RR=1 numerical controls are displayed as zero, without entering the main summary.
risk <- read_input("PAF", "risk_parameters.csv")
risk <- risk[paf_eligible %in% TRUE, .(food_group, outcome,
                                      negative_control_rr_equals_1)]
error_heatmap <- main[, .(value_pp = mean(abs_error_pp)), by = .(food_group, outcome)]
heatmap <- merge(risk, error_heatmap, by = c("food_group", "outcome"),
                 all.x = TRUE, sort = FALSE)
heatmap[negative_control_rr_equals_1 %in% TRUE, value_pp := 0]
heatmap[, label := ifelse(negative_control_rr_equals_1 %in% TRUE, "0*",
                          ifelse(is.finite(value_pp), sprintf("%.3f", value_pp), ""))]
heatmap[, food_label := food_axis(food_group)]
heatmap[, outcome := factor(outcome, levels = c("CVD", "Stroke", "Hypertension"))]
fwrite(heatmap[, .(food_group, outcome, mean_absolute_error_pp = value_pp,
                   numerical_negative_control = negative_control_rr_equals_1)],
       file.path(tab_dir, "Figure4_PAF_food_outcome.csv"))
p <- ggplot(heatmap, aes(outcome, food_label, fill = value_pp)) +
  geom_tile(colour = "white", linewidth = .5) +
  geom_text(aes(label = label), size = 3.2) +
  scale_fill_gradient(low = "#F2F5F8", high = "#15578B", na.value = "grey94") +
  labs(x = NULL, y = NULL, fill = "Error (pp)",
       caption = "* Numerical negative control (RR = 1)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(face = "bold"),
        panel.grid = element_blank(), legend.position = "bottom")
ggsave(file.path(fig_dir, "Figure4_PAF_food_outcome_heatmap.png"), p,
       width = 7.8, height = 8.1, dpi = 300)

# Table S5: country-balanced LOCO accuracy and predictive sigma intervals.
# Within each left-out country, average errors and take median predictive sigma;
# then weight countries equally for the food-level summary.
pred_s <- read_input("prediction", "sensitivity", "predictions.csv")
pred_s[, `:=`(abs_error = abs(latent_mean - log_sigma),
              sq_error = (latent_mean - log_sigma)^2,
              covered = as.numeric(log_sigma >= predictive_lower95 &
                                   log_sigma <= predictive_upper95))]
fold_s <- pred_s[, .(
  MAE = mean(abs_error), MSE = mean(sq_error),
  coverage = mean(covered),
  median_predicted_sigma = median(exp(predictive_median)),
  median_PI_lower_sigma = median(exp(predictive_lower95)),
  median_PI_upper_sigma = median(exp(predictive_upper95))
), by = .(food_group, model_id, left_out_country)]
food_s <- fold_s[, .(
  n_left_out_countries = .N,
  MAE_log_sigma = mean(MAE),
  RMSE_log_sigma = sqrt(mean(MSE)),
  predictive_95_coverage = mean(coverage),
  median_predicted_sigma = median(median_predicted_sigma),
  median_95_PI_lower_sigma = median(median_PI_lower_sigma),
  median_95_PI_upper_sigma = median(median_PI_upper_sigma)
), by = .(food_group, model_id)]
s0 <- copy(food_s[model_id == "M0"])[, model_id := NULL]
s1 <- copy(food_s[model_id == "M1"])[, model_id := NULL]
setnames(s0, setdiff(names(s0), "food_group"),
         paste0("M0_", setdiff(names(s0), "food_group")))
setnames(s1, setdiff(names(s1), "food_group"),
         paste0("M1_", setdiff(names(s1), "food_group")))
table_s5 <- merge(s0, s1, by = "food_group", all = TRUE, sort = FALSE)
# Include the nineteenth food for which formal prediction was not assessable.
table_s5 <- merge(data.table(food_group = foods), table_s5,
                  by = "food_group", all.x = TRUE, sort = FALSE)
table_s5[, food_order := match(food_group, foods)]
setorder(table_s5, food_order)
table_s5[, food_order := NULL]
fwrite(table_s5, file.path(tab_dir, "Table_S5_LOCO_sensitivity_M0_M1.csv"))

# Figures S4 and S5: cycle- and wave-specific sigma heatmaps.
periods <- read_input("consistency", "shape_by_period.csv")
for (src in c("NHANES", "CHNS")) {
  dat <- periods[source == src]
  periods_order <- if (src == "CHNS") c("2004", "2006", "2009", "2011") else
    c("2005-2006", "2007-2008", "2009-2010", "2011-2012",
      "2013-2014", "2015-2016", "2017-2018")
  dat[, time_id := factor(time_id, levels = periods_order)]
  dat[, food_label := food_axis(food_group)]
  dat[, label := ifelse(status == "success" & is.finite(sigma_log_amount),
                        sprintf("%.2f", sigma_log_amount), "")]
  dat[status != "success", sigma_log_amount := NA_real_]
  p <- ggplot(dat, aes(time_id, food_label, fill = sigma_log_amount)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = label), size = 2.5) +
    scale_fill_gradient(low = "#EFF3F9", high = "#2878B8", na.value = "grey93") +
    labs(x = if (src == "NHANES") "NHANES cycle" else "CHNS wave",
         y = NULL, fill = "Sigma") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          axis.text.y = element_text(face = "bold"),
          panel.grid = element_blank(), legend.position = "bottom")
  ggsave(file.path(fig_dir, if (src == "NHANES")
    "Figure_S4_NHANES_cycle_sigma.png" else "Figure_S5_CHNS_wave_sigma.png"),
    p, width = if (src == "NHANES") 10.8 else 7.2, height = 8.2, dpi = 300)
}

# Figures S2 and S3 require two small, separately derived summary datasets.
source(file.path(root, "R", "plot_recording_sensitivity.R"), encoding = "UTF-8")
