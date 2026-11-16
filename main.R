# main.R
# Runs the full project pipeline. Run from the project folder:
#   Rscript main.R
#
# 1. Load data   2. Preprocess   3. EDA   4. Train PCA on normal data
# 5. Detect anomalies (T² / SPE)  6. Evaluate  7. Save results for the dashboard

source("R/data_loading.R")
source("R/preprocessing.R")
source("R/eda.R")
source("R/pca_model.R")
source("R/anomaly_detection.R")
source("R/evaluation.R")

cat("=== Manufacturing Process Anomaly Detection (TEP + PCA) ===\n\n")

# ---- 1 & 2. Load and preprocess ----
if (processed_exists()) {
  cat("Using processed data in data/processed/ (delete it to rebuild from raw files)\n")
  sets <- load_processed()
} else {
  download_tep_data()                 # only downloads files that are missing
  fault_free <- load_fault_free()
  faulty     <- load_faulty()
  sets <- preprocess_tep(fault_free, faulty)
  rm(fault_free, faulty); invisible(gc())
}
train <- sets$train
test  <- sets$test
cat(sprintf("Train: %d rows | Test: %d rows | Sensors: %d\n",
            nrow(train), nrow(test), length(sensor_columns(train))))

# ---- 3. EDA ----
run_eda(train, test)

# ---- 4. PCA model (normal data only) ----
model <- train_pca_model(train)
save_scree_plot(model)

# Sanity check: false alarms on the TRAINING data should be about 1-2%
train_scored <- detect_anomalies(model, train)
cat(sprintf("Alarm rate on normal training data: %.2f%%\n", 100 * mean(train_scored$predicted)))

# ---- 5. Anomaly detection on the test set ----
scored <- detect_anomalies(model, test)
save_detection_plots(model, scored)

# ---- 6. Evaluation ----
results <- evaluate_model(scored)

# ---- 7. Save outputs used by the Shiny dashboard ----
saveRDS(model, file.path("results", "pca_model.rds"))
write.csv(scored, file.path("results", "test_scored.csv"), row.names = FALSE)

cat("\nDone. Plots in results/plots/, metrics in results/metrics/.\n")
cat("Launch the dashboard with:  shiny::runApp(\"app\")\n")
