# requirements.R
# Installs the R packages used by this project (run once).
#   Rscript requirements.R

packages <- c(
  "ggplot2",        # plots
  "corrplot",       # correlation heatmap
  "shiny",          # dashboard
  "shinydashboard", # dashboard layout
  "DT"              # interactive tables in the dashboard
)

missing <- packages[!packages %in% rownames(installed.packages())]

if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  cat("All required packages are already installed.\n")
}
