# ResTop Analytics

ResTop Analytics is an interactive R Shiny application designed to make data analysis accessible and engaging for users of all skill levels. Whether you're exploring data for the first time or conducting advanced statistical analysis, this platform provides intuitive tools for data upload, visualization, and interpretation.

## Features

- **Data Upload**: Import CSV or Excel files for analysis.
- **Data Exploration**: View data tables, summary statistics, and column types.
- **Statistical Analysis**:
  - Summary Statistics
  - Correlation Analysis
  - Linear Regression
  - ANOVA
  - Chi-Square Tests
  - K-Means Clustering
- **Visualizations**:
  - Scatter Plots
  - Histograms
  - Boxplots
- **Auto Analysis**: Get suggested analyses based on your data.
- **Downloadable Reports**: Export results and charts as a zip file.
- **Learn Page**: A beginner-friendly guide to data analysis concepts and how to use the app.

## Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/yourusername/ResTopAnalytics.git
   ```

2. Ensure you have R and RStudio installed.

3. Install the required R packages:

   ```R
   install.packages(c("shiny", "shinyjs", "shinyBS", "ggplot2", "dplyr", "tidyr", "caret", "readxl", "zip", "cluster", "MASS"))
   ```

4. Navigate to the project directory and run the app:

   ```R
   library(shiny)
   runApp()
   ```

## Usage

1. **Launch the App**: Run `app.R` in RStudio or via the command line to start the Shiny server.
2. **Navigate**:
   - **Home Page**: Introduction to ResTop Analytics with options to start analyzing or learn more.
   - **Learn Page**: Explore data analysis basics and app instructions.
   - **Analysis Page**: Upload data, select variables, and perform analyses.
3. **Analyze Data**:
   - Upload a CSV or Excel file.
   - Choose columns for analysis or visualization.
   - Click buttons to run analyses or generate charts.
   - Download a report with your findings.
4. **Tips**:
   - Ensure data is clean (no missing or invalid values) for best results.
   - Use the "Auto Analyze" feature for quick insights.
   - Explore charts to visualize trends.

## File Structure

- `app.R`: Main Shiny app script to launch the application.
- `ui.R`: Defines the user interface, including page navigation.
- `server.R`: Handles server-side logic, data processing, and analysis.
- `learn_page.R`: Renders the educational "Learn More" page.
- `styles.css`: Custom CSS for styling (place in `www/` directory).
- `logo.png`: App logo (place in `www/` directory).

## Dependencies

- R packages: `shiny`, `shinyjs`, `shinyBS`, `ggplot2`, `dplyr`, `tidyr`, `caret`, `readxl`, `zip`, `cluster`, `MASS`
- Ensure `logo.png` and `styles.css` are in the `www/` directory relative to the app files.

## Deployment

To deploy on shinyapps.io:

1. Install the `rsconnect` package:

   ```R
   install.packages("rsconnect")
   ```

2. Configure your shinyapps.io account:

   ```R
   rsconnect::setAccountInfo(name="yourname", token="YOUR_TOKEN", secret="YOUR_SECRET")
   ```

3. Deploy the app:

   ```R
   rsconnect::deployApp()
   ```

## Notes

- Ensure `logo.png` exists in the `www/` directory to avoid rendering issues.
- The app assumes clean data; invalid values are filtered out automatically, but tidy files yield better results.
- For large datasets, performance may depend on your system's resources.

Copyright (c) 2025 ResTop Analytics

## Contact

For issues or suggestions, please open an issue on this repository or contact the maintainer at \www.samutua17@gmail.com\.