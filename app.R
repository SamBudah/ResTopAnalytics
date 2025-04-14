# Load required libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(readxl)
library(zip)
library(cluster)
library(MASS)

# Run the app
shinyApp(ui = ui, server = server)