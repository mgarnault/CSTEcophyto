library(shiny)

server <- function(input, output, session) {
  output$message <- renderText({"Hello world !"})
}
