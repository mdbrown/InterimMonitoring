library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(""),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
   numericInput("minN", 
                "minimum sample size", 
                min = 10,
                max = 10000, 
                value = 100), 
   numericInput("N", 
                "Study sample size", 
                min =10, 
                max = 10000, 
                value = 5000)

  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))