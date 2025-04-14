# Define UI with intro page, learning page, and main app
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  uiOutput("app_ui")
)