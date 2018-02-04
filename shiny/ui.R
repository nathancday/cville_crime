
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
   fluidRow(
       column(width = 4,
              numericInput("num_squares", "How many squares wide",
                           15, min = 3, max = 30),
              actionButton("render", "Rebuild the grid")),
       column(width = 4,textOutput("test"))
   ),
   fluidRow(leafletOutput("map", height = 600))
))
