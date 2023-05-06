library(tidyverse)
library(shiny)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ui <- fluidPage(
  
  # Application title
  titlePanel("People Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Drop down selectors
      selectInput("variableSelect",
                  "Variable of Interest?",
                  c("Monthly Income", "Attrition", "Job Satisfaction"),
                  selected = "Monthly Income"),
      selectInput("departmentSelect",
                  "Filter by Department?",
                  c("Human Resources", "Research & Development", "Sales", "All"),
                  selected="All"),
      selectInput("educationSelect",
                  "Filter by Field of Education?",
                  c("Human Resources","Life Sciences", "Marketing","Medical", "Other", "Technical Degree", "All"),
                  selected="All"),
      selectInput("genderSelect",
                  "Filter by Gender?",
                  c("Male","Female","All"),
                  selected="All"),
      selectInput("jobSelect",
                  "Filter by Job Role?",
                  c("Healthcare Representative", "Human Resources", "Laboratory Technician", "Manager", "Manufacturing Director", "Research Director", "Research Scientist", "Sales Executive", "Sales Representative","All"),
                  selected="All")
    ),
    # Show a plot and a table based on selections
    mainPanel(
      plotOutput('plot')
      #tableOutput('table')
    )
    )
  )
server <- function(input, output) {
  skinny_tbl <- readRDS("skinnydataset.rds")
  output$plot <- renderPlot({
    skinny_tbl <- skinny_tbl
    # Education Filter
    if (input$departmentSelect != "All") {
      skinny_tbl <- skinny_tbl %>%
        filter(EducationField == input$departmentSelect)
    }
    # Education Filter
    if (input$educationSelect != "All") {
      skinny_tbl <- skinny_tbl %>%
        filter(EducationField == input$educationSelect)
    }
    # Gender Filter
    if (input$genderSelect != "All") {
      skinny_tbl <- skinny_tbl %>%
        filter(Gender == input$genderSelect)
    }
    # Job Filter
    if (input$jobSelect != "All") {
      skinny_tbl <- skinny_tbl %>%
        filter(EducationField == input$jobSelect)
    }# Plots
    if (input$variableSelect == "Attrition") {
      ggplot(skinny_tbl, aes(x = Attrition)) +
        geom_bar()
    } 
    if (input$variableSelect == "Job Satisfaction") {
      ggplot(skinny_tbl, aes(x = JobSatisfaction)) +
        geom_bar()
    } 
    if (input$variableSelect == "Monthly Income") {
      ggplot(skinny_tbl, aes(x = MonthlyIncome)) +
        geom_histogram(binwidth = 1000) + 
        theme_classic()
    } 
  })
} 

# Run the application 
shinyApp(ui = ui, server = server)