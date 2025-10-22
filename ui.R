# ui.R: Enhanced UI with modern features

ui <- fluidPage(
  # Initialize shinyjs and other dependencies
  shinyjs::useShinyjs(),
  shiny::tags$head(
    # Include custom CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    # Include Font Awesome for icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    
    # Include Google Fonts
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap", rel = "stylesheet"),
    
    # Custom JavaScript for enhanced interactions
    tags$script(HTML("
      // Enhanced loading states
      $(document).on('shiny:busy', function() {
        $('body').addClass('loading');
        $('#loading-overlay').fadeIn();
      });
      
      $(document).on('shiny:idle', function() {
        $('body').removeClass('loading');
        $('#loading-overlay').fadeOut();
      });
      
      // Smooth scrolling for anchor links
      $('a[href^=\"#\"]').on('click', function(e) {
        e.preventDefault();
        $('html, body').animate({
          scrollTop: $($(this).attr('href')).offset().top
        }, 500);
      });
      
      // Enhanced file input styling
      $(document).on('change', '.file-input', function() {
        var fileName = $(this).val().split('\\\\').pop();
        if (fileName) {
          $(this).next('.file-input-label').html('<i class=\"fas fa-file-upload\"></i> ' + fileName);
        }
      });
    ")),
    
    # Custom styles for specific elements
    tags$style(HTML("
      .loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(255, 255, 255, 0.9);
        backdrop-filter: blur(10px);
        display: none;
        justify-content: center;
        align-items: center;
        z-index: 9999;
        flex-direction: column;
      }
      
      .loading-spinner-large {
        width: 50px;
        height: 50px;
        border: 5px solid #f3f3f3;
        border-top: 5px solid #4361ee;
        border-radius: 50%;
        animation: spin 1s linear infinite;
        margin-bottom: 20px;
      }
      
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 20px;
        margin: 25px 0;
      }
      
      .stat-card {
        background: rgba(255, 255, 255, 0.9);
        padding: 25px;
        border-radius: var(--border-radius);
        text-align: center;
        box-shadow: var(--shadow);
        transition: var(--transition);
        border-left: 4px solid #4361ee;
      }
      
      .stat-card:hover {
        transform: translateY(-5px);
        box-shadow: var(--shadow-hover);
      }
      
      .stat-number {
        font-size: 2.5em;
        font-weight: 800;
        background: var(--gradient-primary);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        margin-bottom: 10px;
      }
      
      .stat-label {
        color: #666;
        font-size: 0.9em;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 600;
      }
      
      .welcome-animation {
        animation: welcomeSlideIn 1s ease-out;
      }
      
      @keyframes welcomeSlideIn {
        from {
          opacity: 0;
          transform: translateY(50px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
    "))
  ),
  
  # Loading overlay
  div(id = "loading-overlay", class = "loading-overlay",
      div(class = "loading-spinner-large"),
      h3("Processing your request...", style = "color: #4361ee;")
  ),
  
  # Main app UI
  uiOutput("app_ui")
)