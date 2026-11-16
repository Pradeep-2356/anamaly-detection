# evaluation.R
# Compares predicted anomalies with the true labels.

classification_metrics <- function(actual, predicted) {
  TP <- sum(predicted == 1 & actual == 1)
  TN <- sum(predicted == 0 & actual == 0)
  FP <- sum(predicted == 1 & actual == 0)
  FN <- sum(predicted == 0 & actual == 1)

  accuracy  <- (TP + TN) / length(actual)
  precision <- if ((TP + FP) > 0) TP / (TP + FP) else 0
  recall    <- if ((TP + FN) > 0) TP / (TP + FN) else 0
  f1        <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
  false_alarm_rate <- if ((FP + TN) > 0) FP / (FP + TN) else 0

  data.frame(TP = TP, TN = TN, FP = FP, FN = FN,
             accuracy = accuracy, precision = precision, recall = recall,
             f1_score = f1, false_alarm_rate = false_alarm_rate)
}

evaluate_model <- function(scored, metrics_dir = file.path("results", "metrics")) {
  dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

  overall <- classification_metrics(scored$label, scored$predicted)

  # Detection rate per fault type (only the samples after the fault starts)
  faults <- sort(unique(scored$faultNumber[scored$faultNumber > 0]))
  per_fault <- do.call(rbind, lapply(faults, function(f) {
    rows <- scored$faultNumber == f & scored$label == 1
    data.frame(faultNumber = f,
               detection_rate = mean(scored$predicted[rows] == 1),
               T2_detection   = mean(scored$T2_alarm[rows] == 1),
               SPE_detection  = mean(scored$SPE_alarm[rows] == 1))
  }))

  write.csv(overall,   file.path(metrics_dir, "overall_metrics.csv"),   row.names = FALSE)
  write.csv(per_fault, file.path(metrics_dir, "per_fault_detection.csv"), row.names = FALSE)

  cat("\n--- Evaluation on test set ---\n")
  cat(sprintf("Confusion matrix: TP=%d  FP=%d  TN=%d  FN=%d\n",
              overall$TP, overall$FP, overall$TN, overall$FN))
  cat(sprintf("Accuracy : %.4f\nPrecision: %.4f\nRecall   : %.4f\nF1-score : %.4f\nFalse alarm rate on normal data: %.4f\n",
              overall$accuracy, overall$precision, overall$recall,
              overall$f1_score, overall$false_alarm_rate))
  cat("\nDetection rate per fault:\n")
  print(transform(per_fault, detection_rate = round(detection_rate, 3),
                  T2_detection = round(T2_detection, 3),
                  SPE_detection = round(SPE_detection, 3)), row.names = FALSE)

  list(overall = overall, per_fault = per_fault)
}
