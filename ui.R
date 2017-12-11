
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Prediction of real variable"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
     
      fileInput("file1","Upload the file for training model"),
      fileInput("file2","Upload the file for predicting"),
      uiOutput("topN"),
      uiOutput("var1"),
      actionButton("go", "Do the Optimization")
     ),

    # Show a plot of the generated distribution
    mainPanel(
        h2('Top selected variables are'),
      uiOutput("Rl"),
      h2("Predicted values"),
      uiOutput("pred1")
    )
  )
))
