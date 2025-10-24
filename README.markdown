# ResTop Analytics 📊

![Shiny](https://img.shields.io/badge/Shiny-R-blue)
![Version](https://img.shields.io/badge/version-2.0.0-green)
A modern, interactive R Shiny application for data analysis and visualization. ResTop Analytics makes data science accessible to everyone with its intuitive interface and powerful analytical capabilities.

![ResTop Analytics App](https://sambudah.shinyapps.io/restopanalytics/)

## ✨ Features

### 🎯 Core Analysis
- **Descriptive Statistics**: Summary statistics and data overview
- **Correlation Analysis**: Relationship between numeric variables
- **Regression Modeling**: Linear regression with visualization
- **ANOVA Testing**: Group mean comparisons
- **Chi-Square Tests**: Categorical data analysis
- **Clustering**: K-means clustering with plots

### 🎨 Visualization
- **Scatter Plots**: With trend lines and confidence intervals
- **Histograms**: Distribution analysis with density curves
- **Box Plots**: Group distribution comparisons
- **Interactive Plots**: Professional ggplot2 visualizations

### 🚀 User Experience
- **Modern UI**: Glass morphism design with smooth animations
- **Responsive Design**: Works on desktop, tablet, and mobile
- **Sample Data**: Built-in datasets for quick testing
- **Auto Analysis**: Smart suggestions based on data types
- **Educational Content**: Built-in learning resources

## 🛠 Installation

### Prerequisites
- R (version 4.0+ recommended)
- RStudio (optional but recommended)

### Quick Start

1. **Clone the repository**:
   ```bash
   git clone https://github.com/SamBudah/ResTopAnalytics.git
   cd ResTop-Analytics


# Run the setup script
source("setup.R")

# Method 1: Using RStudio
# Open app.R and click "Run App"

# Method 2: Using R console
shiny::runApp()

#Project Structure

ResTop-Analytics/
├── app.R                 # Main application launcher
├── ui.R                  # User interface definition
├── server.R              # Server-side logic
├── learn_page.R          # Educational content
├── setup.R               # Dependency management
├── www/
│   ├── styles.css        # Modern CSS styling
│   └── app.js           # Enhanced interactions
└── README.markdown      # Project documentation


Live App
Access the live application here: https://sambudah.shinyapps.io/restopanalytics/