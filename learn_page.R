# learn_page.R: Learning resources for data analysis

render_learn_page <- function() {
  div(class = "learn-page",
      div(class = "learn-content",
          h1("📚 Learn Data Analysis"),
          
          h2("Understanding Your Data"),
          p("Data analysis is the process of inspecting, cleaning, transforming, and modeling data to discover useful information, draw conclusions, and support decision-making."),
          
          h3("Types of Variables"),
          tags$ul(
            tags$li(strong("Numeric Variables:"), "Continuous or discrete numbers (e.g., height, price, age)"),
            tags$li(strong("Categorical Variables:"), "Groups or categories (e.g., gender, product type, region)"),
            tags$li(strong("Ordinal Variables:"), "Categories with order (e.g., small, medium, large)")
          ),
          
          h2("Common Statistical Tests"),
          
          h3("📈 Correlation Analysis"),
          p("Measures the strength and direction of relationship between two numeric variables."),
          tags$ul(
            tags$li("Range: -1 to +1"),
            tags$li("+1: Perfect positive correlation"),
            tags$li("-1: Perfect negative correlation"),
            tags$li("0: No correlation")
          ),
          
          h3("📊 Regression Analysis"),
          p("Models the relationship between a dependent variable and one or more independent variables."),
          tags$ul(
            tags$li("Used for prediction and understanding relationships"),
            tags$li("R-squared: Proportion of variance explained"),
            tags$li("P-value: Statistical significance of relationships")
          ),
          
          h3("📋 ANOVA (Analysis of Variance)"),
          p("Compares means across three or more groups to determine if there are statistically significant differences."),
          tags$ul(
            tags$li("Requires: One categorical (grouping) variable and one numeric variable"),
            tags$li("Null hypothesis: All group means are equal"),
            tags$li("P-value < 0.05: Significant differences exist")
          ),
          
          h3("🧩 Chi-Square Test"),
          p("Tests for association between two categorical variables."),
          tags$ul(
            tags$li("Requires: Two categorical variables"),
            tags$li("Tests if variables are independent or related"),
            tags$li("P-value < 0.05: Significant association exists")
          ),
          
          h3("🔍 Clustering"),
          p("Groups similar observations together without predefined categories."),
          tags$ul(
            tags$li("Unsupervised learning technique"),
            tags$li("Useful for segmentation and pattern discovery"),
            tags$li("K-means: Most common clustering algorithm")
          ),
          
          h2("Best Practices"),
          
          h3("Data Preparation"),
          tags$ul(
            tags$li("Always check for missing values"),
            tags$li("Remove outliers when appropriate"),
            tags$li("Ensure correct data types"),
            tags$li("Normalize data when comparing different scales")
          ),
          
          h3("Interpretation"),
          tags$ul(
            tags$li("Statistical significance ≠ practical significance"),
            tags$li("Correlation ≠ causation"),
            tags$li("Consider sample size and effect size"),
            tags$li("Always visualize your data")
          ),
          
          h2("Next Steps"),
          p("Ready to start analyzing? Upload your data and explore the various analysis options available in ResTop Analytics!"),
          
          div(class = "action-buttons",
              actionButton("back_to_intro", "Back to Home", class = "action-button"),
              actionButton("start_from_learn", "Start Analyzing", class = "action-button")
          )
      )
  )
}

# Navigation from learn page
observeEvent(input$start_from_learn, {
  page_state("app")
})