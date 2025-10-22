# setup.R: Installation and setup script for ResTop Analytics

# Check and install required packages
required_packages <- c(
  "shiny", "shinyBS", "shinyjs", "ggplot2", "dplyr", "tidyr",
  "caret", "readxl", "zip", "cluster", "MASS"
)

# Function to install packages
install_if_missing <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  } else {
    message("All required packages are already installed.")
  }
}

# Install packages
install_if_missing(required_packages)

# Verify installation
message("Verifying package installation...")
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    message("✓ ", pkg, " loaded successfully")
  } else {
    message("✗ ", pkg, " failed to load")
  }
}

# Create necessary directories
if (!dir.exists("www")) {
  dir.create("www")
  message("Created www directory for static files")
}

message("\nSetup complete! You can now run the app using:")
message("shiny::runApp()")