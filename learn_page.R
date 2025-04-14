render_learn_page <- function() {
  tagList(
    img(src = "logo.png", id = "logo", class = "logo clickable", alt = "Logo not found"),
    div(class = "learn-page",
        div(class = "learn-content",
            # Header Section
            h1("Discover Data Analysis"),
            p("New to data analysis? No worries! ResTop Analytics makes it fun and easy to uncover insights from your numbers, whether you're a beginner or just curious."),
            
            # What is Data Analysis
            h2("What’s Data Analysis All About?"),
            p("Think of data analysis as finding treasures in your numbers! It’s about spotting patterns, answering questions, and making smart choices. With ResTop Analytics, you can explore sales data, survey results, or anything else—no math degree needed!"),
            
            # Key Concepts
            h2("Key Ideas, Made Simple"),
            tags$div(
              tags$p(tags$b("Summary Stats: "), "Quick numbers that describe your data, like the average (what’s typical) or how spread out it is. It’s like a snapshot of your data’s personality."),
              tags$p(tags$b("Correlation: "), "Checks if two things move together, like hours studied and test scores. If one goes up and the other does too, that’s a positive correlation."),
              tags$p(tags$b("Regression: "), "Predicts something from another, like guessing a house’s price based on its size."),
              tags$p(tags$b("ANOVA: "), "Compares averages across groups. For example, do different schools have different test scores?"),
              tags$p(tags$b("Chi-Square: "), "Tests if categories are linked, like whether kids and adults prefer different snacks."),
              tags$p(tags$b("Clustering: "), "Groups similar things, like sorting customers by what they buy.")
            ),
            
            # How to Use ResTop Analytics
            h2("How to Get Started"),
            p("ResTop Analytics is super easy to use. Here’s how it works:"),
            tags$ul(
              tags$li(tags$b("Upload Data: "), "Drop in a CSV or Excel file. It’s like a spreadsheet with columns (e.g., 'Age', 'Salary') and rows for each entry."),
              tags$li(tags$b("Pick Columns: "), "Choose what to analyze, like 'Height' and 'Weight' to see if they’re related."),
              tags$li(tags$b("Analyze: "), "Hit buttons to explore:", tags$br(),
                      tags$ul(
                        tags$li(tags$b("View Data: "), "See your data table."),
                        tags$li(tags$b("Auto Analyze: "), "Get suggestions on what to try."),
                        tags$li(tags$b("Summary: "), "Basic stats."),
                        tags$li(tags$b("Correlation: "), "How numbers connect."),
                        tags$li(tags$b("Regression: "), "Make predictions."),
                        tags$li(tags$b("ANOVA: "), "Compare groups."),
                        tags$li(tags$b("Chi-Square: "), "Check category links."),
                        tags$li(tags$b("Clustering: "), "Find similar groups."),
                        tags$li(tags$b("Charts: "), "Make scatter plots, histograms, or boxplots.")
                      )
              ),
              tags$li(tags$b("Understand Results: "), "See clear outputs, like a plot or a number from -1 to 1 for correlation. We’ll suggest next steps too!"),
              tags$li(tags$b("Save Results: "), "Download a report with your findings and charts.")
            ),
            
            # Tips Section
            h2("Tips to Shine"),
            tags$ul(
              tags$li("Keep your data tidy—no blank cells or odd values. We clean some errors, but tidy files work best."),
              tags$li("Try different analyses to find surprises. Not sure where to start? Use 'Auto Analyze'."),
              tags$li("Use charts! They make patterns pop out way better than numbers alone.")
            ),
            
            # Why It Matters
            h2("Why It’s Awesome"),
            p("Data analysis turns boring numbers into exciting insights. Spot trends, predict outcomes, or just get curious—ResTop Analytics makes it all simple and fun. Ready to explore?"),
            
            # Navigation Button
            actionButton("back_to_intro", "Back to Home", class = "action-button")
        )
    ),
    div(class = "footer", "© 2025 ResTop Analytics")
  )
}