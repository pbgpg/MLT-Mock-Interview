library(shiny)
library(shinyjs)
library(googlesheets)
library(ggplot2)
library(reshape2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  # Application title  
  titlePanel(title = "Interview Scores"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset","Choose an industry:", choices = c("All",as.character(industries))),
      #br(),
      checkboxGroupInput("features","Select features to display:", choices = c("Averages", "Outliers"), selected = c("Averages","My scores")),
      #br(),
      textInput("pass","Enter password to retrieve your average scores (Initials + DOB, ex: John Doe, 1/15/1990 is JD011590):", value=""),
      #br(),
      helpText("Note: Although the boxplots are updated to",
               "reflect your industry selection, the averages",
               "(black dots) displayed are still based on the",
               "full data set. Additionally, if your email is",
               "not found, your scores will not be displayed."),
      img(src="MLT.png", height = 60.69/1, width = 100/1)
   
    ),
    mainPanel(htmlOutput("myid"),
              tags$head(tags$style("
                                   #container * {
                                   display: inline;
                                   }")),
              div(id="container",h5(strong("Number of Interviews: ")),htmlOutput("intNum"))
              ,
              plotOutput("barPlot"),
              conditionalPanel(
                condition="output.intNum>=1",
                htmlOutput("industry1"),htmlOutput("comments1"),htmlOutput("invite1"),br(),
                conditionalPanel(
                  condition="output.intNum>=2",
                  htmlOutput("industry2"),htmlOutput("comments2"),htmlOutput("invite2"),br(),
                  conditionalPanel(
                    condition="output.intNum>=3",
                    htmlOutput("industry3"),htmlOutput("comments3"),htmlOutput("invite3"),br(),
                    conditionalPanel(
                      condition="output.intNum>=4",
                      htmlOutput("industry4"),htmlOutput("comments4"),htmlOutput("invite4"),br()
                )))),
              #h3("Description"),
              h6("The chart above displays mock interview scores of Fellows",
                " who attended Core Skills Seminar. The left side of the box",
                "represents the 25th percentile, the right side of the box",
                "represents the 75th, and the thick center line is the median",
                "(50th percentile).")
                 
    )
  )
))