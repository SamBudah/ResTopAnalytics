# app.R: Main script to load libraries and run the Shiny app

# Load required libraries
required_packages <- c(
  "shiny",      # Core Shiny framework
  "shinyBS",    # Bootstrap tooltips
  "shinyjs",    # JavaScript functionality for Shiny
  "ggplot2",    # Plotting
  "dplyr",      # Data manipulation
  "tidyr",      # Data tidying
  "caret",      # Machine learning utilities
  "readxl",     # Excel file reading
  "zip",        # Zip file creation
  "cluster",    # Clustering algorithms
  "MASS"       # Statistical functions
)

# Install missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Source the app components
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)