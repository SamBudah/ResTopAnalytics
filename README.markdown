# ResTop Analytics 📊

![Shiny](https://img.shields.io/badge/Shiny-R-blue)
![Version](https://img.shields.io/badge/version-2.0.0-green)
![License](https://img.shields.io/badge/license-MIT-yellow)

A modern, interactive R Shiny application for data analysis and visualization. ResTop Analytics makes data science accessible to everyone with its intuitive interface and powerful analytical capabilities.

![ResTop Analytics Demo](<https://sambudah.shinyapps.io/restopanalytics/>)

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
   ```

2. **Install dependencies**:
   ```r
   # Run the setup script
   source("setup.R")
   ```

3. **Launch the app**:
   ```r
   # Method 1: Using RStudio
   # Open app.R and click "Run App"
   
   # Method 2: Using R console
   shiny::runApp()
   ```

## 📦 Dependencies

The app automatically installs these packages:

| Package | Purpose |
|---------|---------|
| `shiny` | Core web framework |
| `ggplot2` | Data visualization |
| `dplyr` | Data manipulation |
| `readxl` | Excel file support |
| `caret` | Machine learning utilities |
| `cluster` | Clustering algorithms |

## 🎮 Usage

### 1. **Upload Data**
- Support for CSV and Excel files
- Drag-and-drop interface
- Sample dataset available

### 2. **Explore Variables**
- Automatic data type detection
- Variable selection interface
- Data preview with search

### 3. **Run Analysis**
- Click analysis buttons for instant results
- Auto-analysis for smart suggestions
- Interactive visualizations

### 4. **Export Results**
- Download analysis reports
- Save plots as high-quality images
- Export data summaries

## 📁 Project Structure

```
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
```

## 🌐 Deployment

### ShinyApps.io
```r
# Install rsconnect
install.packages('rsconnect')

# Configure account
rsconnect::setAccountInfo(
  name='your-account-name',
  token='your-token',
  secret='your-secret'
)

# Deploy app
rsconnect::deployApp()
```

### RStudio Connect
- Publish directly from RStudio
- Enterprise-grade hosting

### Shiny Server
- Self-hosted deployment
- Custom server configuration

## 🤝 Contributing

We welcome contributions! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Setup
```bash
# Clone your fork
git clone https://github.com/SamBudah/ResTopAnalytics.git

# Add upstream remote
git remote add upstream https://github.com/SamBudah/ResTopAnalytics.git

# Create development branch
git checkout -b development
```

## 🐛 Troubleshooting

### Common Issues

1. **Package Installation Fails**
   ```r
   # Install from CRAN
   install.packages("package_name", dependencies = TRUE)
   
   # Or from GitHub
   remotes::install_github("username/repo")
   ```

2. **App Won't Start**
   - Check all dependencies are installed
   - Verify file paths are correct
   - Check R version compatibility

3. **File Upload Issues**
   - Ensure file is CSV or Excel format
   - Check file size limits
   - Verify column names are valid

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- RStudio for the amazing Shiny framework
- Tidyverse team for data science packages
- Contributors and testers

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/SamBudah/ResTopAnalytics/issues)
- **Email**: www.samutua17@gmail.com
- **Documentation**: [Wiki](https://github.com/SamBudah/ResTopAnalytics/wiki)

---

<div align="center">

**Made with ❤️ and R**

[⭐ Star this repo on GitHub](https://github.com/SamBudah/ResTopAnalytics)

</div>