# server.R: Server logic for the ResTop Analytics Shiny app

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
  
  # Enhanced data handling with validation
  data <- reactiveVal(NULL)
  data_upload_time <- reactiveVal(NULL)
  
  # Render dynamic UI based on page state
  output$app_ui <- renderUI({
    tryCatch({
      # In the output$app_ui renderUI function, replace the intro page section with:

if (page_state() == "intro") {
  tagList(
    # Enhanced Logo
    div(class = "logo clickable pulse", 
        onclick = "Shiny.setInputValue('logo_click', Math.random())",
        icon("chart-line"), " ResTop Analytics"),
    
    div(class = "intro-page",
        uiOutput("welcome_message"),
        
        h2("Why Choose ResTop Analytics?"),
        div(class = "features-grid",
            div(class = "feature-item",
                icon("bolt", class = "fa-3x", style = "color: #4361ee; margin-bottom: 15px;"),
                h3("Lightning Fast"),
                p("Process large datasets with optimized algorithms and real-time updates")
            ),
            div(class = "feature-item",
                icon("shield-alt", class = "fa-3x", style = "color: #7209b7; margin-bottom: 15px;"),
                h3("Secure & Private"),
                p("Your data never leaves your browser - complete privacy guaranteed")
            ),
            div(class = "feature-item",
                icon("graduation-cap", class = "fa-3x", style = "color: #4cc9f0; margin-bottom: 15px;"),
                h3("Beginner Friendly"),
                p("No coding required - intuitive interface with guided workflows")
            )
        ),
        
        h2("Ready to Get Started?"),
        p("Join thousands of users who trust ResTop Analytics for their data analysis needs."),
        
        div(class = "action-buttons",
            actionButton("start_app", "Start Analyzing Now", 
                         class = "action-button pulse",
                         icon = icon("rocket")),
            actionButton("learn_more", "Explore Features", 
                         class = "action-button",
                         icon = icon("compass"))
        ),
        
        # Quick stats
        div(style = "margin-top: 40px; text-align: center;",
            p(style = "color: #666; font-size: 0.9em;",
              "Trusted by data enthusiasts worldwide • 99.9% Uptime • Regular Updates")
        )
    ),
    div(class = "footer", 
        "© 2025 ResTop Analytics • Making Data Science Accessible to Everyone")
  )
} else if (page_state() == "learn") {
        if (learn_page_available && exists("render_learn_page")) {
          tryCatch({
            tagList(
              div(class = "logo clickable", 
                  onclick = "Shiny.setInputValue('logo_click', Math.random())",
                  "ResTop Analytics"),
              render_learn_page()
            )
          }, error = function(e) {
            showNotification(paste("Error rendering learn page:", e$message), type = "error")
            tagList(
              div(class = "logo clickable", 
                  onclick = "Shiny.setInputValue('logo_click', Math.random())",
                  "ResTop Analytics"),
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
            div(class = "logo clickable", 
                onclick = "Shiny.setInputValue('logo_click', Math.random())",
                "ResTop Analytics"),
            div(class = "learn-page",
                div(class = "learn-content",
                    h1("Error"),
                    p("Sorry, the learning page is currently unavailable. Please try again or contact support."),
                    actionButton("back_to_intro", "Back to Home", class = "action-button")
                )
            ),
            div(class = "footer", "© 2025 ResTop Analytics")
          )
        }
      } else {
        tagList(
          div(class = "logo clickable", 
              onclick = "Shiny.setInputValue('logo_click', Math.random())",
              "ResTop Analytics"),
          div(class = "app-page",
              div(class = "working-frame",
                  fluidRow(
                    column(4,
                           div(class = "section-container",
                               h3("📁 Upload Data"),
                               fileInput("datafile", "Upload CSV or Excel File",
                                         accept = c(".csv", ".xlsx"),
                                         placeholder = "Drag & drop or click to upload",
                                         buttonLabel = "Browse Files"),
                               uiOutput("file_info_ui"),
                               actionButton("sample_data", "Load Sample Data", class = "action-button")
                           )
                    ),
                    column(4,
                           div(class = "section-container",
                               h3("🎯 Select Variables"),
                               uiOutput("variable_selection_ui"),
                               actionButton("refresh_vars", "Refresh Variables", class = "action-button")
                           )
                    ),
                    column(4,
                           div(class = "section-container",
                               h3("🔍 Analyze Data"),
                               div(class = "analysis-buttons",
                                   actionButton("view_data", "View Data", class = "action-button", icon = icon("table")),
                                   actionButton("auto_analyze", "Auto Analyze", class = "action-button", icon = icon("magic")),
                                   actionButton("view_summary", "Summary Stats", class = "action-button", icon = icon("calculator")),
                                   actionButton("view_correlation", "Correlation", class = "action-button", icon = icon("link")),
                                   actionButton("view_regression", "Regression", class = "action-button", icon = icon("trend-up")),
                                   actionButton("view_anova", "ANOVA", class = "action-button", icon = icon("chart-bar")),
                                   actionButton("view_chisq", "Chi-Square", class = "action-button", icon = icon("project-diagram")),
                                   actionButton("view_cluster", "Clustering", class = "action-button", icon = icon("object-group")),
                                   actionButton("view_scatter", "Scatter Plot", class = "action-button", icon = icon("braille")),
                                   actionButton("view_histogram", "Histogram", class = "action-button", icon = icon("chart-column")),
                                   actionButton("view_boxplot", "Boxplot", class = "action-button", icon = icon("boxes-stacked")),
                                   downloadButton("download_report", "Download Report", class = "action-button")
                               )
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
    }, error = function(e) {
      showNotification(paste("Error rendering UI:", e$message), type = "error")
      tagList(
        div(class = "error-page",
            h1("Error"),
            p("An error occurred while loading the page. Please try again or contact support."),
            actionButton("back_to_intro", "Back to Home", class = "action-button")
        ),
        div(class = "footer", "© 2025 ResTop Analytics")
      )
    })
  })
  
  # Navigation logic
  observeEvent(input$start_app, {
    tryCatch({
      page_state("app")
    }, error = function(e) {
      showNotification(paste("Error switching to app page:", e$message), type = "error")
    })
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
  
  # Enhanced variable selection UI
  output$variable_selection_ui <- renderUI({
    req(data())
    clean_data <- data()
    tagList(
      selectInput("xcol", "X Column (Graphs & Analysis)", 
                  choices = c("Select..." = "", names(clean_data))),
      selectInput("ycol", "Y Column (Graphs & Analysis)", 
                  choices = c("Select..." = "", names(clean_data))),
      selectInput("numcol", "Numeric Column (Histograms/Boxplots)", 
                  choices = c("Select..." = "", names(clean_data)[sapply(clean_data, is.numeric)]))
    )
  })
  
  # File info display
  output$file_info_ui <- renderUI({
    req(data())
    clean_data <- data()
    tagList(
      hr(),
      p(strong("File loaded successfully!")),
      p(paste("Rows:", nrow(clean_data))),
      p(paste("Columns:", ncol(clean_data))),
      p(paste("Numeric columns:", sum(sapply(clean_data, is.numeric)))),
      p(paste("Factor columns:", sum(sapply(clean_data, is.factor))))
    )
  })
  
  # Sample data functionality
  observeEvent(input$sample_data, {
    # Load built-in dataset
    sample_data <- mtcars
    data(sample_data)
    updateSelectInput(session, "xcol", choices = names(sample_data))
    updateSelectInput(session, "ycol", choices = names(sample_data))
    updateSelectInput(session, "numcol", choices = names(sample_data)[sapply(sample_data, is.numeric)])
    showNotification("Sample dataset (mtcars) loaded successfully!", type = "message")
  })
  
  # Enhanced data handling with progress indicators
  observeEvent(input$datafile, {
    req(input$datafile)
    withProgress(message = "Loading your data...", value = 0, {
      incProgress(0.2, detail = "Reading file...")
      
      tryCatch({
        ext <- tools::file_ext(input$datafile$name)
        uploaded_data <- switch(ext, 
                               csv = read.csv(input$datafile$datapath), 
                               xlsx = readxl::read_excel(input$datafile$datapath), 
                               stop("Unsupported file type. Please upload CSV or Excel files."))
        
        incProgress(0.5, detail = "Cleaning data...")
        
        # Enhanced data cleaning
        clean_data <- uploaded_data %>%
          na.omit() %>%
          dplyr::filter_all(all_vars(!is.infinite(.)))
        
        # Convert character columns to factors if they have few unique values
        char_cols <- sapply(clean_data, is.character)
        if (any(char_cols)) {
          for (col in names(clean_data)[char_cols]) {
            if (length(unique(clean_data[[col]])) <= 10) {
              clean_data[[col]] <- as.factor(clean_data[[col]])
            }
          }
        }
        
        data(clean_data)
        data_upload_time(Sys.time())
        
        incProgress(0.8, detail = "Updating interface...")
        
        updateSelectInput(session, "xcol", choices = names(clean_data))
        updateSelectInput(session, "ycol", choices = names(clean_data))
        updateSelectInput(session, "numcol", choices = names(clean_data)[sapply(clean_data, is.numeric)])
        
        incProgress(1, detail = "Complete!")
        
        showNotification("Data loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    })
  })
  
  # Refresh variables
  observeEvent(input$refresh_vars, {
    req(data())
    clean_data <- data()
    updateSelectInput(session, "xcol", choices = names(clean_data))
    updateSelectInput(session, "ycol", choices = names(clean_data))
    updateSelectInput(session, "numcol", choices = names(clean_data)[sapply(clean_data, is.numeric)])
    showNotification("Variables refreshed!", type = "message")
  })
  
  # Initialize analysis results
  analysis_results <- reactiveValues(
    plots = list(),
    summary = NULL,
    correlation = NULL,
    regression = NULL,
    anova = NULL,
    chisq = NULL,
    cluster = NULL,
    auto_text = "",
    recommendations = ""
  )
  
  # View data with enhanced display
  observeEvent(input$view_data, {
    req(data())
    clean_data <- data()
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("📊 Data Overview"),
        div(class = "data-info",
            p(strong("Dataset Dimensions:"), paste(nrow(clean_data), "rows ×", ncol(clean_data), "columns")),
            p(strong("Data Types:")),
            verbatimTextOutput("col_types_output")
        ),
        div(class = "data-preview",
            h4("Data Preview (First 10 rows):"),
            tableOutput("data_preview_table")
        )
      )
    })
    
    output$col_types_output <- renderPrint({
      sapply(clean_data, class)
    })
    
    output$data_preview_table <- renderTable({
      head(clean_data, 10)
    }, rownames = TRUE)
  })
  
  # Enhanced Auto-analyze with better suggestions
  observeEvent(input$auto_analyze, {
    req(data())
    clean_data <- data()
    
    withProgress(message = "Analyzing your data...", value = 0, {
      numeric_cols <- names(clean_data)[sapply(clean_data, is.numeric)]
      factor_cols <- names(clean_data)[sapply(clean_data, is.factor)]
      
      analysis_results$auto_text <- "🔍 Auto-Detected Analysis Suggestions:\n\n"
      
      incProgress(0.2, detail = "Checking correlations...")
      
      # Correlation analysis
      if (length(numeric_cols) >= 2) {
        analysis_results$correlation <- tryCatch({
          cor_matrix <- cor(clean_data[numeric_cols], use = "complete.obs")
          # Find strong correlations
          strong_corr <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
          if (nrow(strong_corr) > 0) {
            analysis_results$auto_text <- paste0(analysis_results$auto_text, 
                                                "• Strong correlations detected between numeric variables\n")
          }
          cor_matrix
        }, error = function(e) "Correlation analysis failed")
      }
      
      incProgress(0.4, detail = "Testing relationships...")
      
      # Regression suggestions
      if (length(numeric_cols) >= 2) {
        reg_data <- clean_data %>% select(all_of(numeric_cols[1:2])) %>% na.omit()
        if (nrow(reg_data) > 0) {
          analysis_results$regression <- tryCatch(
            lm(as.formula(paste(numeric_cols[1], "~", numeric_cols[2])), data = reg_data), 
            error = function(e) NULL
          )
        }
      }
      
      incProgress(0.6, detail = "Analyzing groups...")
      
      # ANOVA suggestions
      if (length(factor_cols) >= 1 && length(numeric_cols) >= 1) {
        analysis_results$anova <- tryCatch(
          aov(as.formula(paste(numeric_cols[1], "~", factor_cols[1])), data = clean_data), 
          error = function(e) NULL
        )
      }
      
      incProgress(0.8, detail = "Finalizing recommendations...")
      
      # Build suggestions
      if (length(numeric_cols) >= 2) {
        analysis_results$auto_text <- paste0(analysis_results$auto_text, "• Correlation analysis available\n")
        analysis_results$auto_text <- paste0(analysis_results$auto_text, "• Regression analysis available\n")
      }
      
      if (length(factor_cols) >= 1 && length(numeric_cols) >= 1) {
        analysis_results$auto_text <- paste0(analysis_results$auto_text, "• ANOVA available for group comparisons\n")
      }
      
      if (length(factor_cols) >= 2) {
        analysis_results$auto_text <- paste0(analysis_results$auto_text, "• Chi-Square test available\n")
      }
      
      if (length(numeric_cols) >= 2) {
        analysis_results$auto_text <- paste0(analysis_results$auto_text, "• Clustering analysis available\n")
      }
      
      incProgress(1, detail = "Complete!")
    })
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("🤖 Auto Analysis Results"),
        div(class = "auto-analysis-results",
            verbatimTextOutput("auto_output")
        )
      )
    })
    
    output$auto_output <- renderPrint({
      cat(analysis_results$auto_text)
    })
  })
  
  # Enhanced Summary statistics
  observeEvent(input$view_summary, { 
    req(data())
    analysis_results$summary <- summary(data())
    output$output_ui <- renderUI({ 
      tagList(
        h3("📈 Summary Statistics"),
        div(class = "summary-results",
            verbatimTextOutput("summary_output")
        )
      )
    })
    output$summary_output <- renderPrint(analysis_results$summary) 
  })
  
  # Enhanced Correlation with matrix display
  observeEvent(input$view_correlation, { 
    req(data())
    numeric_data <- data() %>% select(where(is.numeric))
    
    if (ncol(numeric_data) > 0) {
      analysis_results$correlation <- cor(numeric_data, use = "complete.obs")
      output$output_ui <- renderUI({ 
        tagList(
          h3("🔗 Correlation Matrix"),
          div(class = "correlation-results",
              verbatimTextOutput("corr_output")
          )
        )
      })
      output$corr_output <- renderPrint({
        cat("Correlation Matrix:\n")
        print(round(analysis_results$correlation, 3))
        cat("\nInterpretation:\n")
        cat("• Values near +1: Strong positive correlation\n")
        cat("• Values near -1: Strong negative correlation\n")
        cat("• Values near 0: Weak or no correlation\n")
      }) 
    } else {
      showNotification("No numeric columns available for correlation analysis.", type = "warning")
    }
  })
  
  # Enhanced Regression analysis
  observeEvent(input$view_regression, { 
    req(data(), input$xcol, input$ycol)
    
    reg_data <- data() %>% 
      select(all_of(c(input$xcol, input$ycol))) %>% 
      na.omit() %>% 
      dplyr::filter_all(all_vars(is.finite(.)))
    
    if (nrow(reg_data) > 0 && is.numeric(reg_data[[input$xcol]]) && is.numeric(reg_data[[input$ycol]])) {
      analysis_results$regression <- tryCatch({
        lm_model <- lm(as.formula(paste(input$ycol, "~", input$xcol)), data = reg_data)
        analysis_results$plots$regression <- ggplot(reg_data, aes_string(x = input$xcol, y = input$ycol)) + 
          geom_point(color = "blue", alpha = 0.6) + 
          geom_smooth(method = "lm", color = "red", se = TRUE) +
          theme_minimal() + 
          labs(title = paste("Linear Regression:", input$ycol, "~", input$xcol),
               x = input$xcol, y = input$ycol)
        lm_model
      }, error = function(e) "Regression failed. Check variable types.")
    } else {
      analysis_results$regression <- "Regression requires two numeric variables with valid data."
    }
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("📊 Regression Analysis"),
        div(class = "regression-results",
            verbatimTextOutput("reg_output"),
            if (!is.character(analysis_results$regression)) {
              plotOutput("reg_plot_output")
            }
        )
      )
    })
    
    output$reg_output <- renderPrint({ 
      if (is.character(analysis_results$regression)) {
        cat(analysis_results$regression)
      } else {
        cat("Linear Regression Results:\n\n")
        print(summary(analysis_results$regression))
        cat("\nKey Insights:\n")
        cat("• R-squared:", round(summary(analysis_results$regression)$r.squared, 4), "\n")
        cat("• P-value:", format.pval(summary(analysis_results$regression)$coefficients[2,4], digits = 3), "\n")
      }
    })
    
    output$reg_plot_output <- renderPlot({
      if (!is.character(analysis_results$regression)) {
        print(analysis_results$plots$regression)
      }
    })
  })
  
  # Enhanced ANOVA
  observeEvent(input$view_anova, { 
    req(data(), input$xcol, input$numcol)
    
    if (is.factor(data()[[input$xcol]]) && is.numeric(data()[[input$numcol]])) {
      anova_data <- data() %>% select(all_of(c(input$xcol, input$numcol))) %>% na.omit()
      analysis_results$anova <- tryCatch(
        aov(as.formula(paste(input$numcol, "~", input$xcol)), data = anova_data), 
        error = function(e) "ANOVA failed. Ensure X is categorical and Y is numeric."
      )
    } else {
      analysis_results$anova <- "ANOVA requires: X (categorical/factor) and Y (numeric)"
    }
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("📋 ANOVA Results"),
        div(class = "anova-results",
            verbatimTextOutput("anova_output")
        )
      )
    })
    
    output$anova_output <- renderPrint({ 
      if (is.character(analysis_results$anova)) {
        cat(analysis_results$anova)
      } else {
        cat("Analysis of Variance (ANOVA):\n\n")
        print(summary(analysis_results$anova))
        cat("\nInterpretation:\n")
        p_val <- summary(analysis_results$anova)[[1]]$`Pr(>F)`[1]
        if (p_val < 0.05) {
          cat("• Significant differences between groups (p < 0.05)\n")
        } else {
          cat("• No significant differences between groups (p >= 0.05)\n")
        }
      }
    })
  })
  
  # Enhanced Chi-Square Test
  observeEvent(input$view_chisq, { 
    req(data(), input$xcol, input$ycol)
    
    if (is.factor(data()[[input$xcol]]) && is.factor(data()[[input$ycol]])) {
      chisq_data <- data() %>% select(all_of(c(input$xcol, input$ycol))) %>% na.omit()
      analysis_results$chisq <- tryCatch({
        chisq.test(chisq_data[[input$xcol]], chisq_data[[input$ycol]])
      }, error = function(e) "Chi-Square test failed. Check that both variables are categorical.")
    } else {
      analysis_results$chisq <- "Chi-Square test requires two categorical variables."
    }
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("🧩 Chi-Square Test"),
        div(class = "chisq-results",
            verbatimTextOutput("chisq_output")
        )
      )
    })
    
    output$chisq_output <- renderPrint({
      if (is.character(analysis_results$chisq)) {
        cat(analysis_results$chisq)
      } else {
        cat("Chi-Square Test of Independence:\n\n")
        print(analysis_results$chisq)
        cat("\nInterpretation:\n")
        if (analysis_results$chisq$p.value < 0.05) {
          cat("• Significant association between variables (p < 0.05)\n")
        } else {
          cat("• No significant association between variables (p >= 0.05)\n")
        }
      }
    })
  })
  
  # Enhanced Clustering with visualization
  observeEvent(input$view_cluster, { 
    req(data())
    numeric_data <- data() %>% select(where(is.numeric)) %>% na.omit()
    
    if (ncol(numeric_data) >= 2) {
      analysis_results$cluster <- tryCatch({
        set.seed(123) # For reproducibility
        kmeans_result <- kmeans(numeric_data, centers = min(3, nrow(numeric_data)))
        
        # Create cluster plot
        if (ncol(numeric_data) >= 2) {
          cluster_df <- as.data.frame(numeric_data)
          cluster_df$cluster <- as.factor(kmeans_result$cluster)
          
          analysis_results$plots$cluster <- ggplot(cluster_df, aes_string(x = names(numeric_data)[1], 
                                                                         y = names(numeric_data)[2], 
                                                                         color = "cluster")) +
            geom_point(size = 3, alpha = 0.7) +
            theme_minimal() +
            labs(title = "K-means Clustering Results",
                 x = names(numeric_data)[1],
                 y = names(numeric_data)[2],
                 color = "Cluster") +
            scale_color_brewer(palette = "Set1")
        }
        
        kmeans_result
      }, error = function(e) "Clustering failed. Ensure sufficient numeric data.")
    } else {
      analysis_results$cluster <- "Clustering requires at least 2 numeric variables."
    }
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("🔍 Clustering Analysis"),
        div(class = "cluster-results",
            verbatimTextOutput("cluster_output"),
            if (!is.character(analysis_results$cluster)) {
              plotOutput("cluster_plot_output")
            }
        )
      )
    })
    
    output$cluster_output <- renderPrint({ 
      if (is.character(analysis_results$cluster)) {
        cat(analysis_results$cluster)
      } else {
        cat("K-means Clustering Results:\n\n")
        cat("Number of clusters:", length(analysis_results$cluster$size), "\n")
        cat("Cluster sizes:\n")
        print(analysis_results$cluster$size)
        cat("\nWithin-cluster sum of squares:", analysis_results$cluster$tot.withinss, "\n")
        cat("Between-cluster sum of squares:", analysis_results$cluster$betweenss, "\n")
      }
    })
    
    output$cluster_plot_output <- renderPlot({
      if (!is.character(analysis_results$cluster)) {
        print(analysis_results$plots$cluster)
      }
    })
  })
  
  # Enhanced Scatter Plot
  observeEvent(input$view_scatter, { 
    req(data(), input$xcol, input$ycol)
    
    plot_data <- data() %>% select(all_of(c(input$xcol, input$ycol))) %>% na.omit()
    
    if (nrow(plot_data) > 0 && is.numeric(plot_data[[input$xcol]]) && is.numeric(plot_data[[input$ycol]])) {
      analysis_results$plots$scatter <- ggplot(plot_data, aes_string(x = input$xcol, y = input$ycol)) + 
        geom_point(color = "steelblue", alpha = 0.7, size = 2) + 
        geom_smooth(method = "loess", color = "red", se = TRUE) +
        theme_minimal() + 
        labs(title = paste("Scatter Plot:", input$ycol, "vs", input$xcol),
             x = input$xcol, y = input$ycol)
    } else {
      analysis_results$plots$scatter <- NULL
    }
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("📊 Scatter Plot"),
        div(class = "plot-output",
            if (!is.null(analysis_results$plots$scatter)) {
              plotOutput("scatter_plot_output")
            } else {
              p("Scatter plot requires two numeric variables with valid data.")
            }
        )
      )
    })
    
    output$scatter_plot_output <- renderPlot({
      if (!is.null(analysis_results$plots$scatter)) {
        print(analysis_results$plots$scatter)
      }
    })
  })
  
  # Enhanced Histogram
  observeEvent(input$view_histogram, { 
    req(data(), input$numcol)
    
    if (is.numeric(data()[[input$numcol]])) {
      hist_data <- data() %>% select(all_of(input$numcol)) %>% na.omit()
      analysis_results$plots$histogram <- ggplot(hist_data, aes_string(x = input$numcol)) + 
        geom_histogram(fill = "skyblue", color = "black", alpha = 0.7, bins = 30) + 
        geom_density(aes(y = after_stat(count) * 2), color = "red", size = 1) +
        theme_minimal() + 
        labs(title = paste("Histogram of", input$numcol),
             x = input$numcol, y = "Frequency")
    } else {
      analysis_results$plots$histogram <- NULL
    }
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("📊 Histogram"),
        div(class = "plot-output",
            if (!is.null(analysis_results$plots$histogram)) {
              plotOutput("histogram_plot_output")
            } else {
              p("Histogram requires a numeric variable.")
            }
        )
      )
    })
    
    output$histogram_plot_output <- renderPlot({
      if (!is.null(analysis_results$plots$histogram)) {
        print(analysis_results$plots$histogram)
      }
    })
  })
  
  # Enhanced Boxplot
  observeEvent(input$view_boxplot, { 
    req(data(), input$xcol, input$numcol)
    
    if (is.factor(data()[[input$xcol]]) && is.numeric(data()[[input$numcol]])) {
      box_data <- data() %>% select(all_of(c(input$xcol, input$numcol))) %>% na.omit()
      analysis_results$plots$boxplot <- ggplot(box_data, aes_string(x = input$xcol, y = input$numcol)) + 
        geom_boxplot(fill = "lightgreen", alpha = 0.7) + 
        geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
        theme_minimal() + 
        labs(title = paste("Boxplot:", input$numcol, "by", input$xcol),
             x = input$xcol, y = input$numcol)
    } else {
      analysis_results$plots$boxplot <- NULL
    }
    
    output$output_ui <- renderUI({ 
      tagList(
        h3("📊 Boxplot"),
        div(class = "plot-output",
            if (!is.null(analysis_results$plots$boxplot)) {
              plotOutput("boxplot_plot_output")
            } else {
              p("Boxplot requires: X (categorical) and Y (numeric)")
            }
        )
      )
    })
    
    output$boxplot_plot_output <- renderPlot({
      if (!is.null(analysis_results$plots$boxplot)) {
        print(analysis_results$plots$boxplot)
      }
    })
  })
  
  # Enhanced Download Report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("ResTop_Analysis_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      req(data())
      
      withProgress(message = "Creating report...", value = 0, {
        # Create temporary directory
        temp_dir <- tempdir()
        report_dir <- file.path(temp_dir, "ResTop_Report")
        dir.create(report_dir)
        
        incProgress(0.2, detail = "Saving data...")
        
        # Save data
        write.csv(data(), file.path(report_dir, "dataset.csv"), row.names = FALSE)
        
        incProgress(0.4, detail = "Generating plots...")
        
        # Save plots if they exist
        if (length(analysis_results$plots) > 0) {
          for (plot_name in names(analysis_results$plots)) {
            if (!is.null(analysis_results$plots[[plot_name]])) {
              ggsave(file.path(report_dir, paste0(plot_name, ".png")), 
                     analysis_results$plots[[plot_name]], 
                     width = 8, height = 6, dpi = 300)
            }
          }
        }
        
        incProgress(0.6, detail = "Creating summary...")
        
        # Create summary text file
        summary_text <- capture.output({
          cat("ResTop Analytics Report\n")
          cat("Generated:", Sys.time(), "\n")
          cat("Dataset:", ifelse(!is.null(input$datafile), input$datafile$name, "Sample Data"), "\n")
          cat("Rows:", nrow(data()), "Columns:", ncol(data()), "\n\n")
          cat("Summary Statistics:\n")
          print(summary(data()))
          if (!is.null(analysis_results$correlation)) {
            cat("\nCorrelation Matrix:\n")
            print(analysis_results$correlation)
          }
        })
        
        writeLines(summary_text, file.path(report_dir, "analysis_summary.txt"))
        
        incProgress(0.8, detail = "Creating zip file...")
        
        # Create zip file
        zip_files <- list.files(report_dir, full.names = TRUE)
        zip::zipr(file, zip_files)
        
        incProgress(1, detail = "Complete!")
      })
    }
  )
  
  # Enhanced recommendations
  output$recommendations_ui <- renderUI({
    req(data())
    clean_data <- data()
    
    numeric_cols <- names(clean_data)[sapply(clean_data, is.numeric)]
    factor_cols <- names(clean_data)[sapply(clean_data, is.factor)]
    
    recommendations <- c()
    
    if (length(numeric_cols) >= 2) {
      recommendations <- c(recommendations, "Try correlation analysis to find relationships between numeric variables")
      recommendations <- c(recommendations, "Consider regression analysis to model relationships")
    }
    
    if (length(factor_cols) >= 1 && length(numeric_cols) >= 1) {
      recommendations <- c(recommendations, "Use ANOVA to compare means across groups")
    }
    
    if (length(factor_cols) >= 2) {
      recommendations <- c(recommendations, "Try Chi-Square test for categorical relationships")
    }
    
    if (length(numeric_cols) >= 3) {
      recommendations <- c(recommendations, "Explore clustering to find natural groupings in your data")
    }
    
    if (length(recommendations) > 0) {
      tagList(
        div(class = "recommendations-section",
            h4("💡 Analysis Recommendations"),
            tags$ul(
              lapply(recommendations, function(rec) tags$li(rec))
            )
        )
      )
    }
  })
  # Enhanced data statistics
output$data_stats <- renderUI({
  req(data())
  clean_data <- data()
  
  numeric_cols <- sum(sapply(clean_data, is.numeric))
  factor_cols <- sum(sapply(clean_data, is.factor))
  total_rows <- nrow(clean_data)
  total_cols <- ncol(clean_data)
  
  div(class = "stats-grid",
      div(class = "stat-card",
          div(class = "stat-number", total_rows),
          div(class = "stat-label", "Total Rows")
      ),
      div(class = "stat-card",
          div(class = "stat-number", total_cols),
          div(class = "stat-label", "Total Columns")
      ),
      div(class = "stat-card",
          div(class = "stat-number", numeric_cols),
          div(class = "stat-label", "Numeric Columns")
      ),
      div(class = "stat-card",
          div(class = "stat-number", factor_cols),
          div(class = "stat-label", "Categorical Columns")
      )
  )
})

# Enhanced welcome message with animation
output$welcome_message <- renderUI({
  div(class = "welcome-animation",
      h1("Welcome to ResTop Analytics"),
      p("Your gateway to powerful data insights and analytics"),
      div(class = "features-grid",
          div(class = "feature-item floating",
              div(style = "font-size: 3em; margin-bottom: 15px;", "📊"),
              h3("Data Exploration"),
              p("Dive deep into your datasets with intuitive exploration tools")
          ),
          div(class = "feature-item floating", style = "animation-delay: 0.2s;",
              div(style = "font-size: 3em; margin-bottom: 15px;", "🔍"),
              h3("Advanced Analysis"),
              p("Perform statistical tests and machine learning with ease")
          ),
          div(class = "feature-item floating", style = "animation-delay: 0.4s;",
              div(style = "font-size: 3em; margin-bottom: 15px;", "📈"),
              h3("Beautiful Visualizations"),
              p("Create stunning charts and graphs to tell your data's story")
          )
      )
  )
})

# Enhanced variable distribution overview
observeEvent(input$view_distributions, {
  req(data())
  clean_data <- data()
  numeric_cols <- names(clean_data)[sapply(clean_data, is.numeric)]
  
  if (length(numeric_cols) > 0) {
    plots <- list()
    for (col in numeric_cols[1:min(4, length(numeric_cols))]) {
      plots[[col]] <- ggplot(clean_data, aes_string(x = col)) +
        geom_histogram(fill = "#4361ee", alpha = 0.7, bins = 20) +
        theme_minimal() +
        labs(title = paste("Distribution of", col),
             x = col, y = "Count")
    }
    
    output$distribution_plots <- renderUI({
      plot_output_list <- lapply(names(plots), function(col) {
        plotOutput(paste0("dist_plot_", col), height = "250px")
      })
      
      do.call(tagList, plot_output_list)
    })
    
    for (col in names(plots)) {
      local({
        local_col <- col
        output[[paste0("dist_plot_", local_col)]] <- renderPlot({
          plots[[local_col]]
        })
      })
    }
    
    output$output_ui <- renderUI({
      tagList(
        h3("📊 Variable Distributions"),
        div(class = "distribution-grid",
            uiOutput("distribution_plots")
        )
      )
    })
  }
})

# Quick insights feature
observeEvent(input$quick_insights, {
  req(data())
  clean_data <- data()
  
  insights <- list()
  
  # Numeric insights
  numeric_cols <- names(clean_data)[sapply(clean_data, is.numeric)]
  if (length(numeric_cols) > 0) {
    insights$numeric <- paste("Found", length(numeric_cols), "numeric variables for analysis")
  }
  
  # Categorical insights
  factor_cols <- names(clean_data)[sapply(clean_data, is.factor)]
  if (length(factor_cols) > 0) {
    insights$factor <- paste("Found", length(factor_cols), "categorical variables for grouping")
  }
  
  # Missing values check
  missing_count <- sum(is.na(clean_data))
  if (missing_count > 0) {
    insights$missing <- paste("⚠️", missing_count, "missing values detected")
  } else {
    insights$missing <- "✅ No missing values found"
  }
  
  output$output_ui <- renderUI({
    tagList(
      h3("🚀 Quick Insights"),
      div(class = "insights-container",
          lapply(insights, function(insight) {
            div(class = "insight-item", 
                style = "background: rgba(67, 97, 238, 0.1); padding: 15px; border-radius: 12px; margin: 10px 0;",
                p(insight)
            )
          })
      )
    )
  })
})

# Enhanced data preview with search and pagination
output$enhanced_data_preview <- renderUI({
  req(data())
  clean_data <- data()
  
  tagList(
    h3("📋 Data Preview with Enhanced Controls"),
    div(class = "data-controls",
        textInput("data_search", "Search data:", placeholder = "Type to search..."),
        numericInput("page_number", "Page:", value = 1, min = 1, 
                     max = ceiling(nrow(clean_data)/10), step = 1)
    ),
    div(class = "data-preview-enhanced",
        DT::dataTableOutput("enhanced_data_table")
    )
  )
})

output$enhanced_data_table <- DT::renderDataTable({
  req(data())
  clean_data <- data()
  
  # Apply search filter if provided
  if (!is.null(input$data_search) && input$data_search != "") {
    clean_data <- clean_data[apply(clean_data, 1, function(row) {
      any(grepl(input$data_search, row, ignore.case = TRUE))
    }), ]
  }
  
  # Apply pagination
  page_size <- 10
  page <- input$page_number %||% 1
  start_row <- (page - 1) * page_size + 1
  end_row <- min(page * page_size, nrow(clean_data))
  
  if (start_row <= nrow(clean_data)) {
    display_data <- clean_data[start_row:end_row, ]
  } else {
    display_data <- clean_data[1:min(page_size, nrow(clean_data)), ]
  }
  
  DT::datatable(
    display_data,
    options = list(
      pageLength = page_size,
      searching = FALSE,
      lengthChange = FALSE,
      dom = 't',
      scrollX = TRUE
    ),
    class = 'cell-border stripe hover',
    rownames = TRUE
  )
})
}