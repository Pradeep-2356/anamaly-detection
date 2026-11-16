# anomaly_detection.R
# Uses the trained PCA model to flag anomalies.
# A sample is an ANOMALY if its T² OR its SPE is above the control limit.

library(ggplot2)

detect_anomalies <- function(model, data) {
  stats <- compute_pca_stats(model, data)

  out <- data[, intersect(c(ID_COLS, "label"), names(data))]
  out$PC1 <- stats$scores[, 1]
  out$PC2 <- stats$scores[, 2]
  out$T2  <- stats$T2
  out$SPE <- stats$SPE
  out$T2_alarm  <- as.integer(stats$T2  > model$t2_limit)
  out$SPE_alarm <- as.integer(stats$SPE > model$spe_limit)
  out$predicted <- as.integer(out$T2_alarm == 1 | out$SPE_alarm == 1)
  out
}

# ---- Plots of normal vs. anomalous observations ----

save_detection_plots <- function(model, scored, plot_dir = file.path("results", "plots")) {
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  scored$Actual <- factor(ifelse(scored$label == 1, "Anomaly", "Normal"),
                          levels = c("Normal", "Anomaly"))

  # 1. PCA score plot (PC1 vs PC2)
  set.seed(1)
  sample_rows <- scored[sample(nrow(scored), min(6000, nrow(scored))), ]
  p <- ggplot(sample_rows, aes(PC1, PC2, colour = Actual)) +
    geom_point(alpha = 0.4, size = 0.8) +
    scale_colour_manual(values = c(Normal = "#2C7FB8", Anomaly = "#D7301F")) +
    labs(title = "PCA score plot: normal vs. anomalous samples (test set)") +
    theme_minimal()
  ggsave(file.path(plot_dir, "pca_score_plot.png"), p, width = 8, height = 6, dpi = 120, bg = "white")

  # 2. T² and SPE over time for one normal run and a few faults
  show_faults <- c(0, 1, 4, 6)
  df <- scored[scored$faultNumber %in% show_faults, ]
  df <- df[df$faultNumber != 0 | df$simulationRun == min(df$simulationRun[df$faultNumber == 0]), ]
  df$run <- ifelse(df$faultNumber == 0, "Normal run", paste("Fault", df$faultNumber))
  long <- rbind(
    data.frame(run = df$run, sample = df$sample, statistic = "T2",
               value = df$T2, limit = model$t2_limit),
    data.frame(run = df$run, sample = df$sample, statistic = "SPE",
               value = df$SPE, limit = model$spe_limit)
  )
  p <- ggplot(long, aes(sample, value)) +
    geom_line(colour = "grey30", linewidth = 0.3) +
    geom_hline(aes(yintercept = limit), colour = "red", linetype = "dashed") +
    geom_vline(xintercept = FAULT_START, colour = "orange") +
    facet_grid(statistic ~ run, scales = "free_y") +
    scale_y_log10() +
    labs(title = "PCA monitoring charts (red = 99% control limit, orange = fault start)",
         x = "Sample", y = "Statistic (log scale)") +
    theme_minimal()
  ggsave(file.path(plot_dir, "pca_control_charts.png"), p, width = 12, height = 6, dpi = 120, bg = "white")

  # 3. T² vs SPE scatter with limits
  p <- ggplot(sample_rows, aes(T2, SPE, colour = Actual)) +
    geom_point(alpha = 0.4, size = 0.8) +
    geom_vline(xintercept = model$t2_limit, linetype = "dashed") +
    geom_hline(yintercept = model$spe_limit, linetype = "dashed") +
    scale_x_log10() + scale_y_log10() +
    scale_colour_manual(values = c(Normal = "#2C7FB8", Anomaly = "#D7301F")) +
    labs(title = "T² vs. SPE (dashed lines = control limits)") +
    theme_minimal()
  ggsave(file.path(plot_dir, "t2_vs_spe.png"), p, width = 8, height = 6, dpi = 120, bg = "white")
}
