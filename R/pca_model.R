# pca_model.R
# Trains a PCA model on NORMAL operating data only.
#
# Steps:
#   1. Standardise each sensor (mean 0, sd 1) using the training data.
#   2. Run PCA (prcomp).
#   3. Keep the number of components that explains >= 90% of the variance.
#   4. Compute control limits for the two PCA monitoring statistics
#      (T² and SPE) as the 99th percentile of their values on normal data.

library(ggplot2)

VARIANCE_TO_KEEP <- 0.90
CONTROL_LIMIT_Q  <- 0.99

train_pca_model <- function(train) {
  sensors <- sensor_columns(train)
  X <- as.matrix(train[, sensors])

  center <- colMeans(X)
  scl    <- apply(X, 2, sd)
  Z <- scale(X, center = center, scale = scl)

  pca <- prcomp(Z, center = FALSE, scale. = FALSE)

  var_explained <- pca$sdev^2 / sum(pca$sdev^2)
  cum_var <- cumsum(var_explained)
  k <- which(cum_var >= VARIANCE_TO_KEEP)[1]

  model <- list(
    sensors       = sensors,
    center        = center,
    scale         = scl,
    loadings      = pca$rotation[, 1:k, drop = FALSE],   # P  (sensors x k)
    eigenvalues   = pca$sdev[1:k]^2,                     # variance of each kept PC
    all_loadings  = pca$rotation,
    var_explained = var_explained,
    k             = k
  )

  # Control limits from the normal training data
  stats <- compute_pca_stats(model, train)
  model$t2_limit  <- unname(quantile(stats$T2,  CONTROL_LIMIT_Q))
  model$spe_limit <- unname(quantile(stats$SPE, CONTROL_LIMIT_Q))

  cat(sprintf("\nPCA: kept %d of %d components (%.1f%% of variance)\n",
              k, length(sensors), 100 * cum_var[k]))
  cat(sprintf("Control limits (%.0f%%): T2 = %.2f, SPE = %.2f\n",
              100 * CONTROL_LIMIT_Q, model$t2_limit, model$spe_limit))
  model
}

# Hotelling's T² and SPE (Q statistic) for every row of new data.
#   T²  = sum over kept PCs of score^2 / eigenvalue   -> unusual values inside the PCA model
#   SPE = squared reconstruction error                 -> unusual sensor relationships
compute_pca_stats <- function(model, data) {
  X <- as.matrix(data[, model$sensors])
  Z <- scale(X, center = model$center, scale = model$scale)

  scores <- Z %*% model$loadings                      # n x k
  T2  <- rowSums(sweep(scores^2, 2, model$eigenvalues, "/"))
  Z_hat <- scores %*% t(model$loadings)               # reconstruction
  SPE <- rowSums((Z - Z_hat)^2)

  list(scores = scores, T2 = T2, SPE = SPE)
}

save_scree_plot <- function(model, path = file.path("results", "plots", "pca_scree.png")) {
  n <- min(25, length(model$var_explained))
  df <- data.frame(PC = 1:n,
                   variance = model$var_explained[1:n],
                   cumulative = cumsum(model$var_explained)[1:n])
  p <- ggplot(df, aes(PC)) +
    geom_col(aes(y = variance), fill = "#74A9CF") +
    geom_line(aes(y = cumulative), colour = "#D7301F") +
    geom_point(aes(y = cumulative), colour = "#D7301F", size = 1) +
    geom_hline(yintercept = VARIANCE_TO_KEEP, linetype = "dashed") +
    geom_vline(xintercept = model$k + 0.5, linetype = "dotted") +
    labs(title = sprintf("PCA scree plot (%d components kept)", model$k),
         x = "Principal component", y = "Variance explained (bars) / cumulative (line)") +
    theme_minimal()
  ggsave(path, p, width = 8, height = 5, dpi = 120, bg = "white")
}
