# data_loading.R
# Loads the Tennessee Eastman Process (TEP) .RData files from data/raw/.
#
# Source: Harvard Dataverse, Rieth et al. (2017)
#   "Additional Tennessee Eastman Process Simulation Data for Anomaly Detection Evaluation"
#   DOI: 10.7910/DVN/6C3JR1
#
# Files used (place them in data/raw/):
#   TEP_FaultFree_Training.RData  - normal operation (500 runs x 500 samples)
#   TEP_Faulty_Training.RData     - 20 fault types (500 runs each x 500 samples)

RAW_DIR <- file.path("data", "raw")

FAULT_FREE_FILE <- file.path(RAW_DIR, "TEP_FaultFree_Training.RData")
FAULTY_FILE     <- file.path(RAW_DIR, "TEP_Faulty_Training.RData")

DATASET_URLS <- c(
  "TEP_FaultFree_Training.RData" = "https://dataverse.harvard.edu/api/access/datafile/3031241",
  "TEP_Faulty_Training.RData"    = "https://dataverse.harvard.edu/api/access/datafile/3031242"
)

# Try to download any missing file from Harvard Dataverse.
# If this fails (no internet / blocked), download manually - see README.md.
download_tep_data <- function() {
  dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
  old_timeout <- options(timeout = 3600)
  on.exit(options(old_timeout))

  for (fname in names(DATASET_URLS)) {
    dest <- file.path(RAW_DIR, fname)
    if (file.exists(dest)) next
    cat("Downloading", fname, "from Harvard Dataverse ...\n")
    ok <- tryCatch({
      utils::download.file(DATASET_URLS[[fname]], dest, mode = "wb")
      TRUE
    }, error = function(e) {
      message("Download failed: ", conditionMessage(e))
      if (file.exists(dest)) file.remove(dest)
      FALSE
    })
    if (!ok) {
      stop("Could not download ", fname, ". Please download it manually from\n  ",
           DATASET_URLS[[fname]], "\nand save it in ", RAW_DIR, " (see README.md).")
    }
  }
}

# Load one .RData file and return the data frame stored inside it.
load_rdata <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, "\nRun download_tep_data() or see README.md.")
  }
  env <- new.env()
  obj_names <- load(path, envir = env)
  df <- as.data.frame(env[[obj_names[1]]])
  cat(sprintf("Loaded %s: %d rows x %d columns\n", basename(path), nrow(df), ncol(df)))
  df
}

load_fault_free <- function() load_rdata(FAULT_FREE_FILE)
load_faulty     <- function() load_rdata(FAULTY_FILE)
