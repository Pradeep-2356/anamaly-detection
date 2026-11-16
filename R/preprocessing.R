# preprocessing.R
# Builds small, clean training and test sets from the raw TEP data.
#
# Design (kept small so it runs on a normal laptop):
#   * Training set : fault-free runs 1-20            -> normal data only (PCA is fit on this)
#   * Test set     : fault-free runs 21-25           -> normal samples
#                    + faulty run 1 of each fault 1-20 -> fault starts after sample 20
#
# In the faulty training simulations the fault is introduced 1 hour into the run
# (samples are every 3 minutes), so samples 1-20 are still normal and
# samples 21-500 are labelled as anomalies.

PROCESSED_DIR <- file.path("data", "processed")

TRAIN_RUNS       <- 1:20
TEST_NORMAL_RUNS <- 21:25
TEST_FAULT_RUN   <- 1
FAULT_START      <- 20     # last normal sample in a faulty run

ID_COLS <- c("faultNumber", "simulationRun", "sample")

# 52 process variables: 41 measurements (xmeas) + 11 manipulated variables (xmv)
sensor_columns <- function(df) setdiff(names(df), c(ID_COLS, "label", "source"))

build_datasets <- function(fault_free, faulty) {
  train <- fault_free[fault_free$simulationRun %in% TRAIN_RUNS, ]
  train$label <- 0

  test_normal <- fault_free[fault_free$simulationRun %in% TEST_NORMAL_RUNS, ]
  test_normal$label <- 0

  test_fault <- faulty[faulty$simulationRun == TEST_FAULT_RUN, ]
  test_fault$label <- ifelse(test_fault$sample > FAULT_START, 1, 0)

  test <- rbind(test_normal, test_fault)

  # Convert IDs to plain integers and sort
  for (col in ID_COLS) {
    train[[col]] <- as.integer(train[[col]])
    test[[col]]  <- as.integer(test[[col]])
  }
  train <- train[order(train$simulationRun, train$sample), ]
  test  <- test[order(test$faultNumber, test$simulationRun, test$sample), ]
  rownames(train) <- NULL
  rownames(test)  <- NULL

  list(train = train, test = test)
}

# Missing values: count them and fill with the training-set median.
handle_missing <- function(train, test) {
  sensors <- sensor_columns(train)
  n_missing <- sum(is.na(train[, sensors])) + sum(is.na(test[, sensors]))
  cat("Missing sensor values found:", n_missing, "\n")

  if (n_missing > 0) {
    for (s in sensors) {
      med <- median(train[[s]], na.rm = TRUE)
      train[[s]][is.na(train[[s]])] <- med
      test[[s]][is.na(test[[s]])]   <- med
    }
    cat("Missing values replaced with training medians.\n")
  }
  list(train = train, test = test)
}

# Remove sensors that never change in normal data (they carry no information
# and would break scaling).
remove_constant_sensors <- function(train, test) {
  sensors <- sensor_columns(train)
  sds <- sapply(train[, sensors], sd)
  constant <- sensors[sds < 1e-8]
  if (length(constant) > 0) {
    cat("Removing constant sensors:", paste(constant, collapse = ", "), "\n")
    train <- train[, setdiff(names(train), constant)]
    test  <- test[, setdiff(names(test), constant)]
  }
  list(train = train, test = test)
}

# Full preprocessing step: build sets, clean them and save as CSV.
preprocess_tep <- function(fault_free, faulty) {
  sets <- build_datasets(fault_free, faulty)
  sets <- handle_missing(sets$train, sets$test)
  sets <- remove_constant_sensors(sets$train, sets$test)

  dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
  write.csv(sets$train, file.path(PROCESSED_DIR, "train_normal.csv"), row.names = FALSE)
  write.csv(sets$test,  file.path(PROCESSED_DIR, "test_mixed.csv"),   row.names = FALSE)

  cat(sprintf("Training set: %d rows (all normal)\n", nrow(sets$train)))
  cat(sprintf("Test set    : %d rows (%d normal, %d anomaly)\n",
              nrow(sets$test), sum(sets$test$label == 0), sum(sets$test$label == 1)))
  sets
}

# Load the processed CSVs (fast) - used by main.R when they already exist.
load_processed <- function() {
  list(
    train = read.csv(file.path(PROCESSED_DIR, "train_normal.csv")),
    test  = read.csv(file.path(PROCESSED_DIR, "test_mixed.csv"))
  )
}

processed_exists <- function() {
  file.exists(file.path(PROCESSED_DIR, "train_normal.csv")) &&
    file.exists(file.path(PROCESSED_DIR, "test_mixed.csv"))
}
