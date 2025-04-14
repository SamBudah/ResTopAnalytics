server <- function(input, output, session) {
  # Source the learn page file with error handling
  learn_page_available <- tryCatch({
    source("learn_page.R", local = TRUE)
    TRUE
  }, error = function(e) {
    message("Failed to load learn_page.R: ", e$message)
    FALSE
  })
  
  # Reactive value to track page state: 'intro', 'learn', or 'app'
  page_state <- reactiveVal("intro")
  
  output$app_ui <- renderUI({
    if (page_state() == "intro") {
      tagList(
        img(src = "logo.png", id = "logo", class = "logo clickable", alt = "Logo not found", onclick = "Shiny.setInputValue('logo_click', Math.random())"),
        div(class = "intro-page",
            h1("Welcome to ResTop Analytics"),
            p("Unlock the power of your data with ResTop Analytics! Our cutting-edge platform provides intuitive tools for data exploration, visualization, and advanced statistical analysis. Whether you're a beginner or a seasoned analyst, we’re here to help you make data-driven decisions."),
            h2("How It Works"),
            p("Upload your CSV or Excel file, explore your data with automated insights, and visualize trends with interactive charts. Click below to get started!"),
            p("Supported Analyses: Summary Statistics, Correlation, Regression, ANOVA, Chi-Square, Clustering, and more."),
            actionButton("start_app", "Start Analyzing", class = "action-button", `aria-label` = "Begin data analysis"),
            actionButton("learn_more", "Learn More", class = "action-button", `aria-label` = "Learn more about data analysis")
        ),
        div(class = "footer", "© 2025 ResTop Analytics")
      )
    } else if (page_state() == "learn") {
      if (learn_page_available && exists("render_learn_page")) {
        tryCatch({
          tagList(
            img(src = "logo.png", id = "logo", class = "logo clickable", alt = "Logo not found", onclick = "Shiny.setInputValue('logo_click', Math.random())"),
            render_learn_page()
          )
        }, error = function(e) {
          showNotification("Error rendering learn page: " %||% e$message, type = "error")
          tagList(
            img(src = "logo.png", id = "logo", class = "logo clickable", alt = "Logo not found", onclick = "Shiny.setInputValue('logo_click', Math.random())"),
            div(class = "learn-page",
                div(class = "learn-content",
                    h1("Error"),
                    p("Sorry, the learning page is currently unavailable. Please try again later or contact support."),
                    actionButton("back_to_intro", "Back to Home", class = "action-button")
                )
            ),
            div(class = "footer", "© 2025 ResTop Analytics")
          )
        })
      } else {
        showNotification("Learn page module not loaded.", type = "warning")
        tagList(
          img(src = "logo.png", id = "logo", class = "logo clickable", alt = "Logo not found", onclick = "Shiny.setInputValue('logo_click', Math.random())"),
          div(class = "learn-page",
              div(class = "learn-content",
                  h1("Error"),
                  p("Sorry, the learning page is currently unavailable. Please try again later or contact support."),
                  actionButton("back_to_intro", "Back to Home", class = "action-button")
              )
          ),
          div(class = "footer", "© 2025 ResTop Analytics")
        )
      }
    } else {
      tagList(
        img(src = "logo.png", id = "logo", class = "logo clickable", alt = "Logo not found", onclick = "Shiny.setInputValue('logo_click', Math.random())"),
        div(class = "app-page",
            div(class = "working-frame",
                fluidRow(
                  column(4,
                         div(class = "section-container",
                             h3("Upload Data"),
                             fileInput("datafile", "CSV/Excel File",
                                       accept = c(".csv", ".xlsx"),
                                       placeholder = "Drag & drop or click to upload",
                                       buttonLabel = "Browse...")
                         )
                  ),
                  column(4,
                         div(class = "section-container",
                             h3("Select Variables"),
                             selectInput("xcol", "X Column (Graphs)", choices = NULL),
                             selectInput("ycol", "Y Column (Graphs)", choices = NULL),
                             selectInput("numcol", "Numeric Column", choices = NULL)
                         )
                  ),
                  column(4,
                         div(class = "section-container",
                             h3("Analyze"),
                             actionButton("view_data", "View Data", class = "action-button"),
                             bsTooltip("view_data", "Preview data in column", "top"),
                             actionButton("auto_analyze", "Auto Analyze", class = "action-button"),
                             bsTooltip("auto_analyze", "Run suggested analyses", "top"),
                             actionButton("view_summary", "Summary", class = "action-button"),
                             bsTooltip("view_summary", "Basic statistics", "top"),
                             actionButton("view_correlation", "Correlation", class = "action-button"),
                             bsTooltip("view_correlation", "Numeric relationships", "top"),
                             actionButton("view_regression", "Regression", class = "action-button"),
                             bsTooltip("view_regression", "Predictive modeling", "top"),
                             actionButton("view_anova", "ANOVA", class = "action-button"),
                             bsTooltip("view_anova", "Compare group means", "top"),
                             actionButton("view_chisq", "Chi-Square", class = "action-button"),
                             bsTooltip("view_chisq", "Categorical relationships", "top"),
                             actionButton("view_cluster", "Clustering", class = "action-button"),
                             bsTooltip("view_cluster", "Group similar data", "top"),
                             actionButton("view_scatter", "Scatter Plot", class = "action-button"),
                             bsTooltip("view_scatter", "X vs Y visualization", "top"),
                             actionButton("view_histogram", "Histogram", class = "action-button"),
                             bsTooltip("view_histogram", "Numeric distribution", "top"),
                             actionButton("view_boxplot", "Boxplot", class = "action-button"),
                             bsTooltip("view_boxplot", "Group distributions", "top"),
                             downloadButton("download_report", "Download Report", class = "action-button")
                         )
                  )
                ),
                div(class = "output-container",
                    uiOutput("output_ui"),
                    uiOutput("recommendations_ui")
                )
            )
        ),
        div(class = "footer", "© 2025 ResTop Analytics")
      )
    }
  })
  
  # Navigation logic
  observeEvent(input$start_app, {
    page_state("app")
  })
  
  observeEvent(input$learn_more, {
    page_state("learn")
  })
  
  observeEvent(input$back_to_intro, {
    page_state("intro")
  })
  
  observeEvent(input$logo_click, {
    page_state("intro")
  })
  
  data <- reactiveVal(NULL)
  analysis_results <- reactiveValues(plots = list())
  
  observeEvent(input$datafile, {
    req(input$datafile)
    withProgress(message = "Loading data...", value = 0, {
      ext <- tools::file_ext(input$datafile$name)
      uploaded_data <- switch(ext, 
                             csv = read.csv(input$datafile$datapath), 
                             xlsx = readxl::read_excel(input$datafile$datapath), 
                             stop("Unsupported file type"))
      incProgress(0.5)
      clean_data <- uploaded_data %>% na.omit() %>% dplyr::filter_all(all_vars(is.finite(.)))
      data(clean_data)
      incProgress(1)
      updateSelectInput(session, "xcol", choices = names(clean_data))
      updateSelectInput(session, "ycol", choices = names(clean_data))
      updateSelectInput(session, "numcol", choices = names(clean_data))
    })
  })
  
  observeEvent(input$view_data, {
    req(data())
    clean_data <- na.omit(data())
    data(clean_data)
    col_types <- sapply(clean_data, class)
    output$output_ui <- renderUI({ 
      tagList(h3("Data Overview"), 
              verbatimTextOutput("col_types_output"), 
              verbatimTextOutput("data_preview")) 
    })
    output$col_types_output <- renderPrint(col_types)
    output$data_preview <- renderPrint(head(clean_data, 10))
  })
  
  observeEvent(input$auto_analyze, {
    req(data())
    clean_data <- data()
    numeric_cols <- names(clean_data)[sapply(clean_data, is.numeric)]
    factor_cols <- names(clean_data)[sapply(clean_data, is.factor)]
    analysis_results$auto_text <- "Suggested Analyses:\n"
    if (length(numeric_cols) >= 2) {
      analysis_results$correlation <- cor(clean_data[numeric_cols], use = "complete.obs")
      reg_data <- clean_data %>% select(all_of(numeric_cols[1:2])) %>% na.omit()
      if (nrow(reg_data) > 0) {
        analysis_results$regression <- tryCatch(
          lm(as.formula(paste(numeric_cols[1], "~", numeric_cols[2])), data = reg_data), 
          error = function(e) "Regression failed."
        )
      }
      analysis_results$auto_text <- paste0(analysis_results$auto_text, "- Correlation and Regression\n")
    }
    if (length(factor_cols) >= 1 && length(numeric_cols) >= 1) {
      analysis_results$anova <- tryCatch(
        aov(as.formula(paste(numeric_cols[1], "~", factor_cols[1])), data = clean_data), 
        error = function(e) "ANOVA failed."
      )
      analysis_results$auto_text <- paste0(analysis_results$auto_text, "- ANOVA\n")
    }
    if (length(factor_cols) >= 2) {
      analysis_results$chisq <- chisq.test(clean_data[[factor_cols[1]]], clean_data[[factor_cols[2]]])
      analysis_results$auto_text <- paste0(analysis_results$auto_text, "- Chi-Square\n")
    }
    if (length(numeric_cols) >= 2) {
      analysis_results$cluster <- tryCatch(
        kmeans(clean_data[numeric_cols], centers = 3),
        error = function(e) "Clustering failed: insufficient data variation."
      )
      analysis_results$auto_text <- paste0(analysis_results$auto_text, "- Clustering\n")
    }
    output$output_ui <- renderUI({ 
      tagList(h3("Auto-Detected Analysis"), verbatimTextOutput("auto_output")) 
    })
    output$auto_output <- renderPrint({
      list(Suggestions = analysis_results$auto_text,
           Correlation = analysis_results$correlation,
           Regression = if (is.character(analysis_results$regression)) analysis_results$regression else summary(analysis_results$regression),
           ANOVA = if (is.character(analysis_results$anova)) analysis_results$anova else summary(analysis_results$anova),
           ChiSquare = analysis_results$chisq,
           Clusters = if (!is.null(analysis_results$cluster) && !is.character(analysis_results$cluster)) analysis_results$cluster$centers else analysis_results$cluster)
    })
    analysis_results$recommendations <- "Recommendations:\n"
    if (!is.null(analysis_results$correlation) && max(abs(analysis_results$correlation), na.rm = TRUE) > 0.7) {
      analysis_results$recommendations <- paste0(analysis_results$recommendations, "- Strong correlations detected; consider predictive modeling.\n")
    }
    if (!is.null(analysis_results$anova) && !is.character(analysis_results$anova) && summary(analysis_results$anova)[[1]]$`Pr(>F)`[1] < 0.05) {
      analysis_results$recommendations <- paste0(analysis_results$recommendations, "- Significant group differences; explore further.\n")
    }
    output$recommendations_ui <- renderUI({ 
      tagList(h3("Recommendations"), verbatimTextOutput("recs_output")) 
    })
    output$recs_output <- renderPrint(analysis_results$recommendations)
  })
  
  observeEvent(input$view_summary, { 
    req(data())
    analysis_results$summary <- summary(data())
    output$output_ui <- renderUI({ 
      tagList(h3("Summary Statistics"), verbatimTextOutput("summary_output")) 
    })
    output$summary_output <- renderPrint(analysis_results$summary) 
  })
  
  observeEvent(input$view_correlation, { 
    req(data())
    numeric_data <- data() %>% select(where(is.numeric))
    req(ncol(numeric_data) > 0, "No numeric columns available for correlation.")
    analysis_results$correlation <- cor(numeric_data, use = "complete.obs")
    output$output_ui <- renderUI({ 
      tagList(h3("Correlation Matrix"), verbatimTextOutput("corr_output")) 
    })
    output$corr_output <- renderPrint(analysis_results$correlation) 
  })
  
  observeEvent(input$view_regression, { 
    req(data(), input$xcol, input$ycol)
    reg_data <- data() %>% select(all_of(c(input$xcol, input$ycol))) %>% na.omit() %>% dplyr::filter_all(all_vars(is.finite(.)))
    if (nrow(reg_data) > 0) {
      analysis_results$regression <- tryCatch(
        lm(as.formula(paste(input$ycol, "~", input$xcol)), data = reg_data), 
        error = function(e) "Regression failed."
      )
    } else {
      analysis_results$regression <- "No valid data."
    }
    output$output_ui <- renderUI({ 
      tagList(h3("Regression Analysis"), verbatimTextOutput("reg_output")) 
    })
    output$reg_output <- renderPrint({ 
      if (is.character(analysis_results$regression)) analysis_results$regression else summary(analysis_results$regression) 
    })
  })
  
  observeEvent(input$view_anova, { 
    req(data(), input$xcol, input$numcol)
    anova_data <- data() %>% select(all_of(c(input$xcol, input$numcol))) %>% na.omit()
    analysis_results$anova <- tryCatch(
      aov(as.formula(paste(input$numcol, "~", input$xcol)), data = anova_data), 
      error = function(e) "ANOVA failed."
    )
    output$output_ui <- renderUI({ 
      tagList(h3("ANOVA Results"), verbatimTextOutput("anova_output")) 
    })
    output$anova_output <- renderPrint({ 
      if (is.character(analysis_results$anova)) analysis_results$anova else summary(analysis_results$anova) 
    })
  })
  
  observeEvent(input$view_chisq, { 
    req(data(), input$xcol, input$ycol)
    chisq_data <- data() %>% select(all_of(c(input$xcol, input$ycol))) %>% na.omit()
    analysis_results$chisq <- tryCatch(
      chisq.test(chisq_data[[input$xcol]], chisq_data[[input$ycol]]),
      error = function(e) "Chi-Square test failed."
    )
    output$output_ui <- renderUI({ 
      tagList(h3("Chi-Square Test"), verbatimTextOutput("chisq_output")) 
    })
    output$chisq_output <- renderPrint(analysis_results$chisq) 
  })
  
  observeEvent(input$view_cluster, { 
    req(data())
    numeric_data <- data() %>% select(where(is.numeric)) %>% na.omit()
    if (ncol(numeric_data) >= 2) {
      analysis_results$cluster <- tryCatch(
        kmeans(numeric_data, centers = 3),
        error = function(e) "Clustering failed: insufficient data variation."
      )
    } else {
      analysis_results$cluster <- "Not enough numeric columns."
    }
    output$output_ui <- renderUI({ 
      tagList(h3("Cluster Analysis"), verbatimTextOutput("cluster_output")) 
    })
    output$cluster_output <- renderPrint({ 
      if (is.character(analysis_results$cluster)) analysis_results$cluster else analysis_results$cluster$centers 
    })
  })
  
  observeEvent(input$view_scatter, { 
    req(data(), input$xcol, input$ycol)
    scatter_plot <- ggplot(data(), aes_string(x = input$xcol, y = input$ycol)) + 
      geom_point(color = "blue") + 
      theme_minimal() + 
      labs(title = "Scatter Plot", x = input$xcol, y = input$ycol)
    analysis_results$plots$scatter <- scatter_plot
    output$output_ui <- renderUI({ 
      tagList(h3("Scatter Plot"), plotOutput("scatter_plot_output")) 
    })
    output$scatter_plot_output <- renderPlot({ print(analysis_results$plots$scatter) })
  })
  
  observeEvent(input$view_histogram, { 
    req(data(), input$numcol)
    plot_data <- data() %>% filter(!is.na(!!sym(input$numcol)), is.finite(!!sym(input$numcol)))
    histogram <- ggplot(plot_data, aes_string(x = input$numcol)) + 
      geom_histogram(bins = 30, fill = "green", color = "black") + 
      theme_minimal() + 
      labs(title = "Histogram", x = input$numcol, y = "Count")
    analysis_results$plots$histogram <- histogram
    output$output_ui <- renderUI({ 
      tagList(h3("Histogram"), plotOutput("histogram_output")) 
    })
    output$histogram_output <- renderPlot({ print(analysis_results$plots$histogram) })
  })
  
  observeEvent(input$view_boxplot, { 
    req(data(), input$numcol, input$xcol)
    boxplot <- ggplot(data(), aes_string(x = input$xcol, y = input$numcol)) + 
      geom_boxplot(fill = "orange", color = "black") + 
      theme_minimal() + 
      labs(title = "Boxplot", x = input$xcol, y = input$numcol)
    analysis_results$plots$boxplot <- boxplot
    output$output_ui <- renderUI({ 
      tagList(h3("Boxplot"), plotOutput("boxplot_output")) 
    })
    output$boxplot_output <- renderPlot({ print(analysis_results$plots$boxplot) })
  })
  
  output$download_report <- downloadHandler(
    filename = function() { paste0("analysis_", Sys.time(), ".zip") },
    content = function(file) {
      tryCatch({
        temp_dir <- tempdir()
        report_path <- file.path(temp_dir, "report.txt")
        scatter_path <- file.path(temp_dir, "scatter.png")
        hist_path <- file.path(temp_dir, "histogram.png")
        boxplot_path <- file.path(temp_dir, "boxplot.png")
        con <- file(report_path, "w")
        writeLines("ANALYSIS RESULTS\n================\n", con)
        for (name in names(analysis_results)) {
          if (name != "plots") { 
            writeLines(paste0("\n", toupper(name), "\n================\n"), con)
            capture.output(analysis_results[[name]], file = con, append = TRUE) 
          }
        }
        close(con)
        if (!is.null(analysis_results$plots$scatter)) { 
          ggsave(scatter_path, plot = analysis_results$plots$scatter, width = 8, height = 6, dpi = 300) 
        }
        if (!is.null(analysis_results$plots$histogram)) { 
          ggsave(hist_path, plot = analysis_results$plots$histogram, width = 8, height = 6, dpi = 300) 
        }
        if (!is.null(analysis_results$plots$boxplot)) { 
          ggsave(boxplot_path, plot = analysis_results$plots$boxplot, width = 8, height = 6, dpi = 300) 
        }
        files_to_zip <- c(report_path)
        if (file.exists(scatter_path)) files_to_zip <- c(files_to_zip, scatter_path)
        if (file.exists(hist_path)) files_to_zip <- c(files_to_zip, hist_path)
        if (file.exists(boxplot_path)) files_to_zip <- c(files_to_zip, boxplot_path)
        zip::zipr(zipfile = file, files = files_to_zip)
        unlink(c(report_path, scatter_path, hist_path, boxplot_path))
      }, error = function(e) {
        showNotification("Failed to generate report: " %||% e$message, type = "error")
      })
    }
  )
}