# Reproduce manuscript-facing summary tables and figures from aggregate data.
# No survey records, CmdStan sampling or external downloads are needed.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

root <- Sys.getenv("FIDS_REPOSITORY_ROOT", unset = getwd())
out <- Sys.getenv("FIDS_OUTPUT_ROOT", unset = file.path(root, "outputs"))
tab_dir <- file.path(out, "tables")
fig_dir <- file.path(out, "figures")
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

foods <- c(
  "fruits", "non-starchy vegetables", "whole grains", "beans and legumes",
  "refined grains", "potatoes", "other starchy vegetables", "nuts and seeds",
  "processed meats", "unprocessed red meats", "seafood", "eggs",
  "sugar-sweetened beverages", "fruit juices", "coffee", "tea",
  "cheese", "yoghurt", "total milk"
)
food_title <- function(x) {
  s <- tools::toTitleCase(x)
  s[x == "seafood"] <- "Seafoods"
  s
}
food_axis <- function(x) factor(food_title(x), levels = rev(food_title(foods)))
source_colors <- c(CHNS = "#0072B2", FAO_GIFT = "#009E73", NHANES = "#D55E00")
source_shapes <- c(CHNS = 16, FAO_GIFT = 17, NHANES = 15)
read_input <- function(...) fread(file.path(root, "data", ...), na.strings = c("", "NA"))
plot_theme <- function() theme_bw(base_size = 11) +
  theme(axis.text.y = element_text(face = "bold"),
        panel.grid.minor = element_blank(), legend.position = "bottom")
cutoffs <- c(0.15, 0.30, 0.50)

# Across-country statistics: median sigma per country and all country pairs.
country <- read_input("cross_country", "country_food_shape.csv")
pair_mean <- function(s) {
  s <- s[is.finite(s) & s > 0]
  if (length(s) < 2L) return(NA_real_)
  ij <- combn(s, 2L)
  mean(abs(log(ij[1, ] / ij[2, ])))
}
cc <- rbindlist(lapply(c("Primary", "Extended"), function(analysis) {
  x <- if (analysis == "Primary") country[primary_set %in% TRUE] else country
  x[, .(n_countries = sum(is.finite(sigma) & sigma > 0),
        mean_abs_log_ratio = pair_mean(sigma)), by = food_group][, analysis := analysis]
}))
cc[, stability := fifelse(n_countries < 3, "insufficient",
                          fifelse(mean_abs_log_ratio <= 0.15, "very stable",
                          fifelse(mean_abs_log_ratio <= 0.30, "stable",
                          fifelse(mean_abs_log_ratio <= 0.50, "moderate", "unstable"))))]
cc[, food_order := match(food_group, foods)]
setorder(cc, food_order, analysis)
cc[, food_order := NULL]
fwrite(cc, file.path(tab_dir, "cross_country_consistency.csv"))
cc_plot <- cc[is.finite(mean_abs_log_ratio)]
cc_plot[, food_label := food_axis(food_group)]
cc_plot[, analysis := factor(analysis, levels = c("Primary", "Extended"))]
fig_cc <- ggplot(cc_plot, aes(mean_abs_log_ratio, food_label,
                              colour = analysis, shape = analysis)) +
  geom_vline(xintercept = cutoffs, colour = "grey65", linewidth = .3, linetype = "dashed") +
  geom_point(size = 2.6, position = position_dodge(width = .45)) +
  scale_color_manual(values = c(Primary = "#2878B8", Extended = "#D55E00")) +
  scale_shape_manual(values = c(Primary = 16, Extended = 15)) +
  labs(title = "Cross-country consistency of distribution shape",
       x = "Mean pairwise |log(sigma A / sigma B)|", y = NULL,
       colour = "Analysis", shape = "Analysis") + plot_theme()
ggsave(file.path(fig_dir, "cross_country_consistency.png"), fig_cc,
       width = 10, height = 8.5, dpi = 300)

# Sex, age and temporal consistency from source-level aggregates.
stability_specs <- list(
  sex = list(filename = "sex_sources.csv", column = "mean_abs_log_sigma_ratio",
             title = "Sex stability of distribution shape",
             xlabel = "Mean |log(sigma male / sigma female)|"),
  age = list(filename = "age_sources.csv", column = "mean_max_abs_log_sigma_ratio",
             title = "Age stability of distribution shape",
             xlabel = "Mean max |log(sigma age group A / sigma age group B)|"),
  temporal = list(filename = "temporal_sources.csv", column = "mean_pairwise_abs_log_sigma_ratio",
                  title = "Temporal stability of distribution shape",
                  xlabel = "Mean pairwise |log(sigma time A / sigma time B)|")
)
for (nm in names(stability_specs)) {
  spec <- stability_specs[[nm]]
  dat <- read_input("consistency", spec$filename)
  dat <- dat[is.finite(get(spec$column))]
  dat[, food_label := food_axis(food_group)]
  dat[, source := factor(source, levels = c("CHNS", "FAO_GIFT", "NHANES"))]
  p <- ggplot(dat, aes(x = .data[[spec$column]], y = food_label,
                       colour = source, shape = source)) +
    geom_vline(xintercept = cutoffs, colour = "grey60", linewidth = .35,
               linetype = c("dotted", "dashed", "dotdash")) +
    geom_point(size = 2.6, alpha = .95) +
    scale_color_manual(values = source_colors, drop = TRUE) +
    scale_shape_manual(values = source_shapes, drop = TRUE) +
    labs(title = spec$title, x = spec$xlabel, y = NULL,
         colour = "Data source", shape = "Data source") + plot_theme()
  ggsave(file.path(fig_dir, paste0(nm, "_consistency.png")), p,
         width = 8.5, height = 8, dpi = 300)
}

# LOCO error: equal weighting of left-out countries within each food.
all_loco <- list()
for (analysis in c("primary", "sensitivity")) {
  pred <- read_input("prediction", analysis, "predictions.csv")
  pred[, `:=`(
    error = latent_mean - log_sigma,
    covered = as.numeric(log_sigma >= predictive_lower95 & log_sigma <= predictive_upper95)
  )]
  folds <- pred[, .(n_test_units = .N,
                    MAE = mean(abs(error)),
                    MSE = mean(error^2),
                    coverage_95 = mean(covered)),
                by = .(food_group, left_out_country, model_id)]
  food <- folds[, .(
    n_countries = .N,
    n_test_units = sum(n_test_units),
    MAE_log_sigma = mean(MAE),
    RMSE_log_sigma = sqrt(mean(MSE)),
    predictive_95_coverage = mean(coverage_95)), by = .(food_group, model_id)]
  food[, analysis := analysis]
  all_loco[[analysis]] <- food
}
loco <- rbindlist(all_loco)
loco[, food_order := match(food_group, foods)]
setorder(loco, food_order, analysis, model_id)
loco[, food_order := NULL]
fwrite(loco, file.path(tab_dir, "LOCO_M0_M1_accuracy.csv"))

# PAF error: published main-model (M0), country-balanced, RR=1 controls omitted.
paf_file <- file.path(out, "PAF", "country_balanced.csv")
if (!file.exists(paf_file)) paf_file <- file.path(root, "data", "PAF", "country_balanced.csv")
paf <- fread(paf_file)
# Public summary data are already primary-only; full recalculations have this field.
if ("analysis_structure" %in% names(paf)) {
  paf <- paf[analysis_structure == "primary"]
}
main <- paf[abs(cap_probability - 0.999) < 1e-9 &
              !(negative_control_rr_equals_1 %in% TRUE)]
if (nrow(main) == 0L) stop("No main-model country-balanced PAF comparisons found.")
main[, abs_error_pp := 100 * mean_abs_error_M0]
paf_summary <- main[, .(
  n_country_food_outcomes = .N,
  mean_abs_error_pp = mean(abs_error_pp),
  median_abs_error_pp = median(abs_error_pp),
  proportion_gt_0_5pp = mean(abs_error_pp > 0.5),
  proportion_gt_1pp = mean(abs_error_pp > 1),
  proportion_gt_2pp = mean(abs_error_pp > 2)
)]
fwrite(paf_summary, file.path(tab_dir, "PAF_M0_summary.csv"))
# The PAF histogram is not a manuscript figure; do not generate it.
old_paf_figure <- file.path(fig_dir, "PAF_M0_error.png")
if (file.exists(old_paf_figure)) unlink(old_paf_figure)

# Manuscript figures and Table S5 from the supplied aggregate data.
source(file.path(root, "R", "manuscript_outputs.R"), encoding = "UTF-8")

cat("Reproduced tables in:", tab_dir, "\n")
cat("Reproduced figures in:", fig_dir, "\n")
cat(sprintf("PAF main M0: N=%d, mean=%.3f pp, median=%.3f pp\n",
            nrow(main), paf_summary$mean_abs_error_pp, paf_summary$median_abs_error_pp))
