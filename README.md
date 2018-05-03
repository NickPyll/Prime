# Prime
R Shiny app to determine primality

```

library(shiny)
library(shinythemes)

ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  headerPanel('Is your favorite number prime?'),
  
  sidebarPanel(
    numericInput('num', 'Enter a number.', 1, 3, 1000),
    verbatimTextOutput('xtext')
  )
)

server <- function(input, output, session) {
  
  results <- reactive({
    
    xtext <- paste(input$num, "is a prime number.")
    j <- 0
    if (input$num <= 500000) {
      
      
      if (input$num == 1) {
        xtext <- paste(input$num, "is the loneliest number, but by definition it is not prime.")
      } else if (input$num == 2) {
        xtext <- paste(input$num, "is the only even prime.")
      } else {
        if ((input$num %% 2) == 0) {
          xtext <- paste(input$num, "is an even number, silly....of course it's not prime.")
        } else {
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
    } else if (input$num <= 1000000000000){
      if ((input$num %% 2) == 0) {
        xtext <- paste(input$num, "is an even number, silly....of course it's not prime.")
      } else if (input$num == 8675309){
        xtext <- paste(input$num, "is not only Jenny's number, but it's also the hypotenuse of a primitive Pythagorean triple...  Oh, and it is also prime.")
      } else {
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

```
