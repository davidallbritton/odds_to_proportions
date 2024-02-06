library(shiny)

# This little Shiny app is intended to help students get a feel for the relationship between odds and probabilities. It is NOT suitable for doing calculations for anything important; write some R code to do that instead.

# Define the UI
ui <- fluidPage(
  titlePanel("Specify your odds:"),
  fluidRow(
    column(2, textInput("oddsInput", label = NULL, value = "1")),
    column(1, tags$span(" to 1")),
    column(1, radioButtons("oddsType", label = NULL, 
                           choices = c("for", "against"),
                           selected = "for", inline = TRUE)),
    column(1),
    column(2, uiOutput("displayOdds")),
    column(5,
           plotOutput("probabilityPlot", height = "75")
    )
  ),
  fluidRow(
    column(12, 
           hr(),
           h3("Using Odds and Probabilities"),
           p("Odds can be converted to probabilities, and probabilities can be converted to odds. 
      By entering different odds in the box above you can get a feel for how the two are 
      related."),
           
           p("For example:"),
           tags$ul(
             tags$li("Odds of 1 (1 to 1) corresponds to a probability of 0.5"),
             tags$li("Odds of
      4 (4 to 1) means that it is 4 times as likely to be true as to be false, which corresponds to 
      a probability of 0.8 (Probability it is true = 0.8; probability it is false = 0.2; 
      odds = 0.8 / 0.2 = 4)"),
             tags$li("Odds of 0.25 (odds of 1 to 4 in favor, or 4 to 1 against) means that it is 4 times
      as likely to be false as to be true, which corresponds to a probability of 0.2 
      (Probability it is true = 0.2; probability it is false = 0.8; odds = 0.2 / 0.8 = 0.25)")
           ),
           p(strong("Either odds or probabilities can be used to quantify the amount of
             confidence you have in a belief."), "Odds of 1 (probability = 0.5) means
             you are completely uncertain whether it is true or false. Odds of 10 to 1
             in favor means you are pretty confident it is true.  Odds of 10 to 1 against
             means you are pretty confident it is false.
             "  ),
           p(strong("Using odds has some advantages:")),
           tags$ul(
             tags$li("Using odds helps keep you from", 
                     a("setting your confidence at 0% or 100%", 
                       href="https://en.wikipedia.org/wiki/Cromwell%27s_rule")),
             tags$li("Using odds makes it easy to update your beliefs in response to new evidence"),
             tags$li("The idea of a ", '"fair bet"', "can help you", 
                     a("quantify your level of confidence", 
                       href="https://jonathanweisberg.org/vip/beliefs-betting-rates.html"))
           )
           
           
           
           
           
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Custom function to format numbers
  formatNumber <- function(number) {
    if ((number > 0 && number < 1e-12) || number >= 1e+12 ) {
      return(sprintf("%.2e", number))
    } else {
      if (number >= 1 && number < 1e+12){
        return(prettyNum(number, big.mark = ",", scientific=F))
      } else{
        formatted <- format(number, scientific = FALSE, trim = TRUE)
        if (number >= .0001) {formatted <- sprintf("%.4f", number)}
        return(sub("0+$", "", formatted))
      }
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
      if(prob <= .9999 && prob >= .0001) {
        formatted <- sprintf("%.4f", prob)
      }
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
