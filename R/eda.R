# eda.R
# Exploratory Data Analysis and sensor correlation analysis.
# All plots are saved to results/plots/.

library(ggplot2)
library(corrplot)

PLOT_DIR <- file.path("results", "plots")

run_eda <- function(train, test) {
  dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
  sensors <- sensor_columns(train)

  # ---- 1. Basic summary ----
  cat("\n--- EDA ---\n")
  cat("Number of sensors:", length(sensors), "\n")
  cat("Training runs:", length(unique(train$simulationRun)),
      "| samples per run:", max(train$sample), "\n")
  cat("Test samples by fault number:\n")
  print(table(test$faultNumber))

  stats <- data.frame(
    sensor = sensors,
    mean   = sapply(train[, sensors], mean),
    sd     = sapply(train[, sensors], sd),
    min    = sapply(train[, sensors], min),
    max    = sapply(train[, sensors], max)
  )
  write.csv(stats, file.path("results", "metrics", "sensor_summary.csv"), row.names = FALSE)

  # ---- 2. Sensor time series: one normal run vs. one faulty run ----
  show_sensors <- c("xmeas_1", "xmeas_7", "xmeas_9", "xmv_10")
  show_sensors <- intersect(show_sensors, sensors)
  normal_run <- test[test$faultNumber == 0, ]
  normal_run <- normal_run[normal_run$simulationRun == min(normal_run$simulationRun), ]
  fault_run  <- test[test$faultNumber == 1, ]

  ts_df <- rbind(
    long_format(normal_run, show_sensors, "Normal run"),
    long_format(fault_run,  show_sensors, "Fault 1 run")
  )
  p <- ggplot(ts_df, aes(sample, value, colour = condition)) +
    geom_line(linewidth = 0.4) +
    geom_vline(xintercept = FAULT_START, linetype = "dashed") +
    facet_wrap(~ sensor, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = c("Normal run" = "#2C7FB8", "Fault 1 run" = "#D7301F")) +
    labs(title = "Sensor readings: normal vs. Fault 1 (dashed line = fault start)",
         x = "Sample (3 min each)", y = "Value", colour = NULL) +
    theme_minimal()
  ggsave(file.path(PLOT_DIR, "eda_sensor_timeseries.png"), p, width = 10, height = 6, dpi = 120, bg = "white")

  # ---- 3. Distribution of all sensors (standardised) ----
  z <- scale(train[, sensors])
  box_df <- data.frame(sensor = rep(sensors, each = nrow(z)), value = as.vector(z))
  box_df$sensor <- factor(box_df$sensor, levels = sensors)
  p <- ggplot(box_df, aes(sensor, value)) +
    geom_boxplot(outlier.size = 0.2, fill = "#A6CEE3") +
    labs(title = "Standardised sensor distributions (normal training data)",
         x = NULL, y = "z-score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, size = 6))
  ggsave(file.path(PLOT_DIR, "eda_sensor_boxplots.png"), p, width = 12, height = 5, dpi = 120, bg = "white")

  # ---- 4. Correlation analysis ----
  corr <- cor(train[, sensors])
  png(file.path(PLOT_DIR, "eda_correlation_heatmap.png"), width = 1100, height = 1100, res = 110)
  corrplot(corr, method = "color", order = "hclust", tl.cex = 0.55, tl.col = "black",
           title = "Sensor correlation (normal data)", mar = c(0, 0, 2, 0))
  dev.off()

  # Strongest correlated sensor pairs
  idx <- which(upper.tri(corr), arr.ind = TRUE)
  pairs <- data.frame(sensor_1 = rownames(corr)[idx[, 1]],
                      sensor_2 = colnames(corr)[idx[, 2]],
                      correlation = corr[idx])
  pairs <- pairs[order(-abs(pairs$correlation)), ]
  write.csv(head(pairs, 20), file.path("results", "metrics", "top_correlated_pairs.csv"),
            row.names = FALSE)
  cat("Top 5 correlated sensor pairs:\n")
  print(head(pairs, 5), row.names = FALSE)

  invisible(corr)
}

# Helper: turn selected sensor columns into long format for ggplot
long_format <- function(df, sensors, condition) {
  data.frame(
    sample    = rep(df$sample, times = length(sensors)),
    sensor    = rep(sensors, each = nrow(df)),
    value     = unlist(df[, sensors], use.names = FALSE),
    condition = condition
  )
}
