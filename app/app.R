# app.R - Shiny dashboard for PCA-based anomaly detection on TEP data.
# Run main.R first (it creates the files this app reads), then from the project folder:
#   shiny::runApp("app")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# ---------------------------------------------------------------------------
# Load results produced by main.R
# ---------------------------------------------------------------------------
root <- if (file.exists("../main.R")) ".." else "."
needed <- file.path(root, c("results/pca_model.rds", "results/test_scored.csv",
                            "data/processed/test_mixed.csv", "data/processed/train_normal.csv"))
if (!all(file.exists(needed))) {
  stop("Results not found. Please run main.R first (Rscript main.R).")
}

model     <- readRDS(needed[1])
scored    <- read.csv(needed[2])
test_data <- read.csv(needed[3])
train     <- read.csv(needed[4])
overall   <- read.csv(file.path(root, "results/metrics/overall_metrics.csv"))
per_fault <- read.csv(file.path(root, "results/metrics/per_fault_detection.csv"))
sensors   <- model$sensors
FAULT_START <- 20

# Sensor descriptions (Downs & Vogel, 1993)
sensor_desc <- c(
  xmeas_1 = "A feed (stream 1)", xmeas_2 = "D feed (stream 2)", xmeas_3 = "E feed (stream 3)",
  xmeas_4 = "A and C feed (stream 4)", xmeas_5 = "Recycle flow", xmeas_6 = "Reactor feed rate",
  xmeas_7 = "Reactor pressure", xmeas_8 = "Reactor level", xmeas_9 = "Reactor temperature",
  xmeas_10 = "Purge rate", xmeas_11 = "Separator temperature", xmeas_12 = "Separator level",
  xmeas_13 = "Separator pressure", xmeas_14 = "Separator underflow", xmeas_15 = "Stripper level",
  xmeas_16 = "Stripper pressure", xmeas_17 = "Stripper underflow", xmeas_18 = "Stripper temperature",
  xmeas_19 = "Stripper steam flow", xmeas_20 = "Compressor work",
  xmeas_21 = "Reactor cooling water outlet temp", xmeas_22 = "Separator cooling water outlet temp",
  xmeas_23 = "Reactor feed: A", xmeas_24 = "Reactor feed: B", xmeas_25 = "Reactor feed: C",
  xmeas_26 = "Reactor feed: D", xmeas_27 = "Reactor feed: E", xmeas_28 = "Reactor feed: F",
  xmeas_29 = "Purge gas: A", xmeas_30 = "Purge gas: B", xmeas_31 = "Purge gas: C",
  xmeas_32 = "Purge gas: D", xmeas_33 = "Purge gas: E", xmeas_34 = "Purge gas: F",
  xmeas_35 = "Purge gas: G", xmeas_36 = "Purge gas: H", xmeas_37 = "Product: D",
  xmeas_38 = "Product: E", xmeas_39 = "Product: F", xmeas_40 = "Product: G", xmeas_41 = "Product: H",
  xmv_1 = "D feed flow (valve)", xmv_2 = "E feed flow (valve)", xmv_3 = "A feed flow (valve)",
  xmv_4 = "A and C feed flow (valve)", xmv_5 = "Compressor recycle valve", xmv_6 = "Purge valve",
  xmv_7 = "Separator liquid flow (valve)", xmv_8 = "Stripper product flow (valve)",
  xmv_9 = "Stripper steam valve", xmv_10 = "Reactor cooling water flow",
  xmv_11 = "Condenser cooling water flow"
)
sensor_label <- function(s) paste0(s, " - ", ifelse(is.na(sensor_desc[s]), "", sensor_desc[s]))
sensor_choices <- setNames(sensors, sensor_label(sensors))

fault_desc <- c(
  "1" = "A/C feed ratio (step)", "2" = "B composition (step)", "3" = "D feed temperature (step)",
  "4" = "Reactor cooling water temp (step)", "5" = "Condenser cooling water temp (step)",
  "6" = "A feed loss (step)", "7" = "C header pressure loss (step)",
  "8" = "A, B, C feed composition (random)", "9" = "D feed temperature (random)",
  "10" = "C feed temperature (random)", "11" = "Reactor cooling water temp (random)",
  "12" = "Condenser cooling water temp (random)", "13" = "Reaction kinetics (drift)",
  "14" = "Reactor cooling water valve (sticking)", "15" = "Condenser cooling water valve (sticking)",
  "16" = "Unknown", "17" = "Unknown", "18" = "Unknown", "19" = "Unknown", "20" = "Unknown"
)

# Run choices: normal runs + one run per fault
scored$run_id    <- paste(scored$faultNumber, scored$simulationRun, sep = "_")
test_data$run_id <- paste(test_data$faultNumber, test_data$simulationRun, sep = "_")
normal_runs <- unique(scored$run_id[scored$faultNumber == 0])
fault_runs  <- unique(scored$run_id[scored$faultNumber > 0])
run_choices <- c(
  setNames(normal_runs, paste("Normal operation - run", sub("0_", "", normal_runs))),
  setNames(fault_runs, sapply(fault_runs, function(r) {
    f <- sub("_.*", "", r); paste0("Fault ", f, " - ", fault_desc[f])
  }))
)

# Normal ranges for sensors (mean +/- 3 sd of training data)
sensor_mean <- model$center
sensor_sd   <- model$scale

# Background cloud of normal training scores for the PCA plot
set.seed(1)
train_sample <- train[sample(nrow(train), 3000), ]
Z_train <- scale(as.matrix(train_sample[, sensors]), center = model$center, scale = model$scale)
train_scores <- as.data.frame(Z_train %*% model$loadings[, 1:2])
names(train_scores) <- c("PC1", "PC2")

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "TEP Anomaly Monitor", titleWidth = 260),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Live Monitoring", tabName = "monitor", icon = icon("gauge")),
      menuItem("PCA & Model Performance", tabName = "model", icon = icon("chart-line"))
    ),
    selectInput("run", "Process run", choices = run_choices, selected = "1_1"),
    sliderInput("time", "Current sample (3 min each)", min = 1, max = 500, value = 100, step = 1,
                animate = animationOptions(interval = 300)),
    selectInput("sensors", "Sensors to plot", choices = sensor_choices,
                selected = c("xmeas_7", "xmeas_9", "xmv_10"), multiple = TRUE),
    tags$p(style = "padding: 0 15px; font-size: 12px; color: #b8c7ce;",
           "Press the play button under the slider to simulate live monitoring.")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "monitor",
        fluidRow(
          valueBoxOutput("status_box", width = 3),
          valueBoxOutput("t2_box", width = 3),
          valueBoxOutput("spe_box", width = 3),
          valueBoxOutput("alarm_box", width = 3)
        ),
        fluidRow(
          box(title = "Anomaly detection timeline (T² and SPE)", width = 12, status = "primary",
              solidHeader = TRUE, plotOutput("timeline_plot", height = 330))
        ),
        fluidRow(
          box(title = "Sensor monitoring", width = 7, status = "primary", solidHeader = TRUE,
              plotOutput("sensor_plot", height = 360)),
          box(title = "Sensor readings at current sample", width = 5, status = "primary",
              solidHeader = TRUE, DTOutput("sensor_table"))
        )
      ),
      tabItem(tabName = "model",
        fluidRow(
          valueBox(sprintf("%.1f%%", 100 * overall$accuracy),  "Accuracy",  icon = icon("bullseye"), color = "blue",   width = 3),
          valueBox(sprintf("%.1f%%", 100 * overall$precision), "Precision", icon = icon("crosshairs"), color = "green", width = 3),
          valueBox(sprintf("%.1f%%", 100 * overall$recall),    "Recall",    icon = icon("magnifying-glass"), color = "yellow", width = 3),
          valueBox(sprintf("%.3f", overall$f1_score),          "F1-score",  icon = icon("star"), color = "purple", width = 3)
        ),
        fluidRow(
          box(title = "PCA score plot (PC1 vs PC2)", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("pca_plot", height = 380),
              tags$small("Grey = normal training data. Coloured = selected run up to the current sample.")),
          box(title = "Detection rate per fault type (test set)", width = 6, status = "primary",
              solidHeader = TRUE, plotOutput("fault_plot", height = 380))
        ),
        fluidRow(
          box(title = "Model summary", width = 12, status = "info", solidHeader = TRUE,
              htmlOutput("model_summary"))
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  run_scores <- reactive(scored[scored$run_id == input$run, ])
  run_data   <- reactive(test_data[test_data$run_id == input$run, ])
  is_fault_run <- reactive(!startsWith(input$run, "0_"))

  # Keep the slider within the run length
  observeEvent(input$run, {
    n <- nrow(run_scores())
    updateSliderInput(session, "time", max = n, value = min(input$time, n))
  })

  current <- reactive({
    rs <- run_scores()
    rs[min(input$time, nrow(rs)), ]
  })

  # ---- Status boxes ----
  output$status_box <- renderValueBox({
    if (current()$predicted == 1) {
      valueBox("ANOMALY", "Process status: anomaly detected", icon = icon("triangle-exclamation"), color = "red")
    } else {
      valueBox("NORMAL", "Process status: operating normally", icon = icon("circle-check"), color = "green")
    }
  })
  output$t2_box <- renderValueBox({
    valueBox(sprintf("%.1f", current()$T2), sprintf("T² score (limit %.1f)", model$t2_limit),
             icon = icon("chart-area"), color = if (current()$T2_alarm == 1) "red" else "aqua")
  })
  output$spe_box <- renderValueBox({
    valueBox(sprintf("%.1f", current()$SPE), sprintf("SPE score (limit %.1f)", model$spe_limit),
             icon = icon("wave-square"), color = if (current()$SPE_alarm == 1) "red" else "aqua")
  })
  output$alarm_box <- renderValueBox({
    rs <- run_scores()[1:input$time, ]
    valueBox(sprintf("%d / %d", sum(rs$predicted), nrow(rs)), "Samples flagged so far",
             icon = icon("bell"), color = "orange")
  })

  # ---- Timeline ----
  output$timeline_plot <- renderPlot({
    rs <- run_scores()[1:input$time, ]
    long <- rbind(
      data.frame(sample = rs$sample, stat = "T²",  value = rs$T2,  limit = model$t2_limit,
                 alarm = rs$T2_alarm == 1),
      data.frame(sample = rs$sample, stat = "SPE", value = rs$SPE, limit = model$spe_limit,
                 alarm = rs$SPE_alarm == 1)
    )
    p <- ggplot(long, aes(sample, value)) +
      geom_line(colour = "grey40") +
      geom_point(aes(colour = alarm), size = 1) +
      geom_hline(aes(yintercept = limit), colour = "red", linetype = "dashed") +
      scale_colour_manual(values = c(`FALSE` = "#2C7FB8", `TRUE` = "#D7301F"),
                          labels = c("Normal", "Anomaly"), name = NULL) +
      scale_y_log10() +
      facet_wrap(~ stat, ncol = 2, scales = "free_y") +
      xlim(1, nrow(run_scores())) +
      labs(x = "Sample", y = "Score (log scale)") +
      theme_minimal(base_size = 13) + theme(legend.position = "top")
    if (is_fault_run()) {
      p <- p + geom_vline(xintercept = FAULT_START, colour = "orange") +
        labs(caption = "Orange line = true fault start (sample 20); red dashed = 99% control limit")
    }
    p
  })

  # ---- Sensor plot ----
  output$sensor_plot <- renderPlot({
    req(input$sensors)
    rd <- run_data()[1:input$time, ]
    long <- do.call(rbind, lapply(input$sensors, function(s) {
      data.frame(sample = rd$sample, sensor = sensor_label(s), value = rd[[s]],
                 lo = unname(sensor_mean[s] - 3 * sensor_sd[s]),
                 hi = unname(sensor_mean[s] + 3 * sensor_sd[s]))
    }))
    ggplot(long, aes(sample, value)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#C7E9C0", alpha = 0.6) +
      geom_line(colour = "#08519C") +
      facet_wrap(~ sensor, ncol = 1, scales = "free_y") +
      xlim(1, nrow(run_scores())) +
      labs(x = "Sample", y = "Reading", caption = "Green band = normal range (mean ± 3 sd)") +
      theme_minimal(base_size = 12)
  })

  # ---- Sensor table ----
  output$sensor_table <- renderDT({
    row <- run_data()[min(input$time, nrow(run_data())), sensors]
    vals <- as.numeric(row)
    z <- (vals - sensor_mean) / sensor_sd
    df <- data.frame(
      Sensor = sensors,
      Description = unname(ifelse(is.na(sensor_desc[sensors]), "", sensor_desc[sensors])),
      Reading = round(vals, 3),
      `z-score` = round(z, 2),
      Status = ifelse(abs(z) > 3, "Out of range", "OK"),
      check.names = FALSE
    )
    df <- df[order(-abs(df$`z-score`)), ]
    datatable(df, rownames = FALSE, options = list(pageLength = 8, dom = "tp", scrollX = TRUE)) |>
      formatStyle("Status", color = styleEqual(c("OK", "Out of range"), c("green", "red")),
                  fontWeight = "bold")
  })

  # ---- PCA plot ----
  output$pca_plot <- renderPlot({
    rs <- run_scores()[1:input$time, ]
    rs$Detected <- ifelse(rs$predicted == 1, "Anomaly", "Normal")
    ggplot() +
      geom_point(data = train_scores, aes(PC1, PC2), colour = "grey75", size = 0.8) +
      geom_point(data = rs, aes(PC1, PC2, colour = Detected), size = 1.5, alpha = 0.8) +
      geom_point(data = rs[nrow(rs), ], aes(PC1, PC2), shape = 21, size = 5,
                 stroke = 1.5, colour = "black") +
      scale_colour_manual(values = c(Normal = "#2C7FB8", Anomaly = "#D7301F")) +
      labs(colour = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "top")
  })

  # ---- Per-fault detection ----
  output$fault_plot <- renderPlot({
    df <- per_fault
    df$fault <- factor(df$faultNumber, levels = df$faultNumber)
    ggplot(df, aes(fault, 100 * detection_rate)) +
      geom_col(fill = "#3182BD") +
      geom_text(aes(label = sprintf("%.0f", 100 * detection_rate)), vjust = -0.3, size = 3.5) +
      ylim(0, 105) +
      labs(x = "Fault number", y = "Detection rate (%)") +
      theme_minimal(base_size = 13)
  })

  output$model_summary <- renderUI({
    HTML(sprintf(
      "<ul>
        <li><b>Algorithm:</b> Principal Component Analysis trained on normal operating data only
            (%d samples, %d sensors).</li>
        <li><b>Components kept:</b> %d (%.1f%% of variance).</li>
        <li><b>Detection rule:</b> anomaly if T² &gt; %.2f <i>or</i> SPE &gt; %.2f (99th percentile of normal data).</li>
        <li><b>Test set:</b> %d samples &mdash; TP = %d, FP = %d, TN = %d, FN = %d.
            False alarm rate on normal samples: %.1f%%.</li>
      </ul>",
      nrow(train), length(sensors), model$k, 100 * sum(model$var_explained[1:model$k]),
      model$t2_limit, model$spe_limit, nrow(scored),
      overall$TP, overall$FP, overall$TN, overall$FN, 100 * overall$false_alarm_rate))
  })
}

shinyApp(ui, server)
