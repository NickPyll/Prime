# source all the things

library(shiny)
library(shinythemes)

# thinking about adding fibonacci and palindrome as other "interesting facts" about input number

# f <- numeric()
# f[1] <- f[2] <- 1
# for(i in 3:100) f[i] <- f[i-2] + f[i-1]

# q <- 7117
# q <- 7332
# q <- as.character(q)
# # Function to reverse text
# strReverse <- function(x)
#   sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
# q == strReverse(q)

ui <- fluidPage(
  theme = 'custom.css',
  
  class = "page",
 
  # force vertical gap so content is beneath the navbar
  fluidRow(style = "height:50px"),
  fluidRow(
    class = "db-intro",
    align = "center",
    HTML("<H1>PRIME FINDER</H1>")
    ),
  
  fluidRow(
    class = "story",
    style = "margin-right:10px; margin-left:10px; display: flex; align-items: center; justify-content: center;",
    column(12, style = "font-size: 16px;", align = "center", 'here are some words')
  ),
  
  
  titlePanel("Is your favorite number prime?"),
  
    fluidRow(
      align = "center",
      numericInput('num', 'What is your favorite number?', 1, 3, 1000)
    ),
    
    column(12,
           verbatimTextOutput('xtext')
    )
  
)

server <- function(input, output, session) {
  
  results <- reactive({
    
    xtext <- paste(input$num, "is a prime number.")
    j <- 0
    
    #### less than 500k ----
    
    if (input$num <= 500000) {
      
      #### 1 ----
      if (input$num == 1) {
        
        xtext <- paste(input$num, "is the loneliest number, but by definition it is not prime.")
      } 
      
      #### 2 ----
      
      else if (input$num == 2) {
        xtext <- paste(input$num, "is the only even prime and the base of the binary system.")
      }  
      
      #### 42 ----
      
      else if (input$num == 42) {
        xtext <- 'yes....that is the answer to the Ultimate Question. Also...not prime.'
      } 
      
      #### even ----
      
      else {
        if ((input$num %% 2) == 0) {
          xtext <- paste(input$num, "is an even number, silly....of course it's not prime.")
      } 
      
      #### loop ----    
        
      else {
          for(i in 3:(floor(input$num / 2))) {
            k <- i + j
            if (k - input$num >= 0) {
              break
            }
            if ((input$num %% k) == 0) {
              xtext <- paste(input$num, "is not a prime number.  It is divisible by", k, ".")
              break
            } 
            j <- j + 1
          }
        }
      }
      
      #### greater than 500k ----  
      
    } else if (input$num <= 1000000000000){
      if ((input$num %% 2) == 0) {
        xtext <- paste(input$num, "is an even number, silly....of course it's not prime.")
      } 
      
      #### 8675309 -----
      
      else if (input$num == 8675309){
        xtext <- paste(input$num, "is not only Jenny's number, but it's also the hypotenuse of a primitive Pythagorean triple...  Oh, and it is also prime.")
      } 
      
      else if (input$num == 573237611){
        xtext <- paste(input$num, "Not sure how you did it, but you guessed my SSN...and yes it's prime.")
      } 
      
      else {
        for(i in 3:(floor(input$num / 100000))) {
          k <- i + j
          if (k - input$num >= 0) {
            break
          }
          if ((input$num %% k) == 0) {
            xtext <- paste(input$num, "is not a prime number.  It is divisible by", k, ".")
            break
          } 
          j <- j + 1
        }
      }
    } else {
      xtext <- paste("Sorry, but I am not able to work with numbers that large at this time. Bokay?")
    }
    results <- xtext
    results
  })
  output$xtext <- renderText(results())
}

shinyApp(ui = ui, server = server)



