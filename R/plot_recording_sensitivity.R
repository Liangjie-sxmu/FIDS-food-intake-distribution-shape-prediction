# Supplementary Figures S2 and S3 from non-individual summary data.

# Figure S2 reproduces the two-panel CHNS comparison only if the
# mapping-corrected wave-food unit results used for the final plot are supplied.
s2_path <- file.path(root, "data", "sensitivity", "chns_two_vs_three_days_units.csv")
if (file.exists(s2_path)) {
  u <- fread(s2_path)
  required <- c("food", "wave", "sigma_3day", "sigma_2day_mean",
                "zero_fraction_3day_complete", "n_reps_success",
                "absolute_relative_difference_pct")
  missing <- setdiff(required, names(u))
  if (length(missing)) stop("Figure S2 missing columns: ", paste(missing, collapse = ", "))

  s2_food_order <- c("fruits", "non-starchy vegetables", "whole grains",
                     "beans and legumes", "refined grains", "potatoes",
                     "other starchy vegetables", "nuts and seeds", "processed meats",
                     "unprocessed red meats", "seafood", "eggs",
                     "sugar-sweetened beverages", "yoghurt", "total milk")
  # Use explicit vector extraction to avoid data.table symbol lookup ambiguity.
  s2_keep <- as.character(u[["food"]]) %in% s2_food_order
  u <- u[which(s2_keep)]
  if (!nrow(u)) stop("Figure S2: no matching food groups.")
  u[, sparse := is.finite(zero_fraction_3day_complete) & zero_fraction_3day_complete > 0.95]
  u[, food_label := factor(food_title(food), levels = rev(food_title(s2_food_order)))]
  valid <- u[n_reps_success > 0 & is.finite(sigma_3day) &
               is.finite(sigma_2day_mean)]
  if (!nrow(valid)) stop("Figure S2: no successful two-day estimates.")

  # Panel A: same wave-food points, 1:1 reference and sparse-unit styling
  low <- max(0.25, min(c(valid$sigma_3day, valid$sigma_2day_mean)) - 0.02)
  high <- min(0.86, max(c(valid$sigma_3day, valid$sigma_2day_mean)) + 0.04)
  if (high <= low) stop("Figure S2: invalid sigma axis bounds.")
  p2a <- ggplot(valid, aes(sigma_3day, sigma_2day_mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey45") +
    geom_point(data = valid[sparse == FALSE], shape = 16, colour = "grey45", size = 2.5) +
    geom_point(data = valid[sparse == TRUE], shape = 21, colour = "grey45",
               fill = "white", stroke = 0.85, size = 2.7) +
    coord_equal(xlim = c(low, high), ylim = c(low, high)) +
    labs(x = "Three-day complete-case sigma",
         y = "Mean two-day sigma",
         title = "A  Agreement of two- and three-day estimates") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  ggsave(file.path(fig_dir, "Figure_S2A_CHNS_2day_vs_3day.png"), p2a,
         width = 6.5, height = 6.2, dpi = 300)

  # Panel B: per-wave food points, 5% line, hollow sparse units and failures
  b <- valid[is.finite(absolute_relative_difference_pct)]
  failed <- u[n_reps_success == 0]
  upper <- max(7.8, b$absolute_relative_difference_pct + 0.5)
  p2b <- ggplot(b, aes(absolute_relative_difference_pct, food_label)) +
    geom_vline(xintercept = 5, linetype = "dashed", colour = "grey45") +
    geom_point(data = b[sparse == FALSE], colour = "grey50", size = 2.4) +
    geom_point(data = b[sparse == TRUE], shape = 21, fill = "white",
               colour = "grey45", stroke = 0.9, size = 2.7) +
    labs(x = "Absolute relative difference in sigma (%)", y = NULL,
         title = "B  Food-specific sensitivity") +
    scale_x_continuous(limits = c(-0.5, upper)) +
    theme_bw(base_size = 11) +
    theme(axis.text.y = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  if (nrow(failed)) {
    p2b <- p2b + geom_point(data = failed,
                           aes(x = -0.28, y = food_label),
                           inherit.aes = FALSE, shape = 4,
                           colour = "grey30", size = 3, stroke = 1)
  }
  ggsave(file.path(fig_dir, "Figure_S2B_CHNS_food_sensitivity.png"), p2b,
         width = 8, height = 7.5, dpi = 300)
} else {
  message("Figure S2 not generated: final CHNS wave-food unit summary is not available.")
}

# Figure S3: exact six-food targeted NHANES comparison using published sigma.
s3_path <- file.path(root, "data", "sensitivity", "nhanes_direct_partial.csv")
if (file.exists(s3_path)) {
  s3 <- fread(s3_path)
  required <- c("food_group", "sigma_direct_only", "sigma_partial_decomposition")
  missing <- setdiff(required, names(s3))
  if (length(missing)) stop("Figure S3 missing columns: ", paste(missing, collapse = ", "))
  s3_food_order <- c("whole grains", "eggs", "refined grains",
                     "non-starchy vegetables", "beans and legumes", "seafood")
  # Evaluate filtering on plain vectors; do not use 'order' inside data.table i.
  s3_keep <- as.character(s3[["food_group"]]) %in% s3_food_order &
    is.finite(s3[["sigma_direct_only"]]) &
    is.finite(s3[["sigma_partial_decomposition"]])
  s3 <- s3[which(s3_keep)]
  if (nrow(s3) != 6L || anyDuplicated(s3$food_group)) {
    stop("Figure S3 requires one pair of estimates for each of the six target foods.")
  }
  s3[, food_label := factor(food_title(food_group), levels = rev(food_title(s3_food_order)))]
  p3 <- ggplot(s3, aes(y = food_label)) +
    geom_segment(aes(x = sigma_direct_only, xend = sigma_partial_decomposition,
                     yend = food_label), colour = "grey70", linewidth = 0.75) +
    geom_point(aes(x = sigma_direct_only, shape = "Direct-only"),
               colour = "black", size = 3.1) +
    geom_point(aes(x = sigma_partial_decomposition,
                   shape = "Ingredient-informed"),
               colour = "grey45", fill = "white", stroke = 1, size = 3.1) +
    scale_shape_manual(name = NULL,
      values = c("Direct-only" = 16, "Ingredient-informed" = 21),
      breaks = c("Direct-only", "Ingredient-informed")) +
    scale_x_continuous(limits = c(0, 1.02), breaks = c(0, 0.25, 0.50, 0.75, 1.0),
                       expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "Positive-amount dispersion (sigma)", y = NULL) +
    theme_bw(base_size = 11) +
    theme(axis.text.y = element_text(face = "bold"),
          panel.grid.minor = element_blank(), legend.position = "top")
  ggsave(file.path(fig_dir, "Figure_S3_NHANES_direct_partial.png"), p3,
         width = 8.3, height = 5.5, dpi = 300)
}
