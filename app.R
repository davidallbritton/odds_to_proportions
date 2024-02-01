library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Odds Converter"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(tags$p("Specify your odds:")),
      fluidRow(
        column(5, textInput("oddsInput", label = NULL, value = "1")),
        column(2, tags$span(" to 1")),
        column(5, radioButtons("oddsType", label = NULL, 
                               choices = c("for", "against"),
                               selected = "for", inline = TRUE))
      )
    ),
    mainPanel(
      # textOutput("displayOdds"),
      uiOutput("displayOdds"),
      plotOutput("probabilityPlot", height = "80")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Custom function to format numbers
  formatNumber <- function(number) {
    if (number > 0 && number < 1e-12 || number > 1e+12 ) {
      return(sprintf("%.2e", number))
    } else {
      return(format(number, scientific = FALSE, trim = TRUE))
    }
  }
  
  formatProbability <- function(prob) {
    if (prob >= 0 && prob < 1e-15) {
      return("close to 0")
    } else if (prob >= 1 - 1e-15 && prob <= 1) {
      return("close to 1")
    } else {
      # Format with up to 15 decimal places
      formatted <- sprintf("%.15f", prob)
      # Remove trailing zeros
      return(sub("0+$", "", formatted))
    }
  }
  

  # Convert input to numeric and handle errors
  convertInput <- reactive({
    inputVal <- suppressWarnings(as.numeric(input$oddsInput))
    if (is.na(inputVal) || inputVal < 0) {
      return(NULL)
    } else {
      return(inputVal)
    }
  })
  
  # Function to calculate and return the odds and probability
  calcOdds <- reactive({
    odds <- convertInput()
    if (is.null(odds) || odds == 0) {
      return(NULL)
    }
    
    if (input$oddsType == "against") {
      odds <- 1 / odds
    }
    prob <- odds / (1 + odds)
    list(odds = odds, prob = prob)
  })
  
  # # Display the odds or error message
  # output$displayOdds <- renderText({
  #   data <- calcOdds()
  #   if (is.null(data)) {
  #     "Error: Please enter a valid numeric odds value."
  #   } else {
  #     sprintf("Odds: %s;  Log odds: %g;  Probability: %s", formatNumber(data$odds), log(data$odds), formatProbability(data$prob))
  #   }
  # })
  
  # Display the odds or error message
  output$displayOdds <- renderUI({
    data <- calcOdds()
    if (is.null(data)) {
      HTML("Error: Please enter a valid numeric odds value.")
    } else {
      HTML(paste("Odds: ", formatNumber(data$odds), "<br>",
                 "Log odds: ", sprintf("%g", log(data$odds)), "<br>",
                 "Probability: ", formatProbability(data$prob), sep = ""))
    }
  })
  
  # Render the probability plot
  output$probabilityPlot <- renderPlot({
    data <- calcOdds()
    if (is.null(data)) return()
    
    # Set margins to smaller values
    par(mar = c(2, 2, 2, 2))  # Adjust margin sizes as necessary
    
    # Create a horizontal line with a red tick mark for the probability
    plot(1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="", ylab="", axes = FALSE,
         main = sprintf("Probability = %s", formatProbability(data$prob)))
    axis(1, at = seq(0, 1, by = 0.1))
    segments(x0 = data$prob, y0 = 0, x1 = data$prob, y1 = 1, col = "red", lwd = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
