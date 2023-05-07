library(tidyverse)
library(shiny)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # commented out once app was complete

ui <- fluidPage(
  
  # Application title
  titlePanel("People Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Drop down selectors
      selectInput("variableSelect",
                  "Outcome Variable of Interest?",
                  c("Monthly Pay", "Turnover Status", "Job Satisfaction"),
                  selected = "MonthlyPay"),
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
      plotOutput('plot'), 
      tableOutput('table')
    )
  )
)
server <- function(input, output) {
  skinny_tbl <- readRDS("skinnydataset.rds")
  reactive_table <- reactive({
    table_tbl <- skinny_tbl
    # Department
    if (input$departmentSelect == "All") {
      table_tbl <- table_tbl %>%
        select(-Department)
    }
    # Education Filter
    if (input$educationSelect == "All") {
      table_tbl <- table_tbl %>%
        select(-EducationField)
    }
    # Gender Filter
    if (input$genderSelect == "All") {
      table_tbl <- table_tbl %>%
        select(-Gender)
    }
    # Job Filter
    if (input$jobSelect == "All") {
      table_tbl <- table_tbl %>%
        select(-JobRole)
    }
    table_tbl
  })
  reactive_plot <- reactive({
    filtered_tbl <- skinny_tbl
    # Department
    if (input$departmentSelect != "All") {
      filtered_tbl <- filtered_tbl %>%
        filter(Department == input$departmentSelect)
    }
    # Education Filter
    if (input$educationSelect != "All") {
      filtered_tbl <- filtered_tbl %>%
        filter(EducationField == input$educationSelect)
    }
    # Gender Filter
    if (input$genderSelect != "All") {
      filtered_tbl <- filtered_tbl %>%
        filter(Gender == input$genderSelect)
    }
    # Job Filter
    if (input$jobSelect != "All") {
      filtered_tbl <- filtered_tbl %>%
        filter(JobRole == input$jobSelect)
    }
    filtered_tbl
  })
  output$plot <- renderPlot({
    #Plots
    if (input$variableSelect == "Monthly Pay") {
      reactive_plot() %>%
        ggplot(aes(x = !!sym(input$variableSelect))) +
        geom_histogram(binwidth = 1000, color = "white") +
        labs(y = "Employee Count") +
        theme_classic()
    } else {
      reactive_plot() %>%
        ggplot(aes(x = !!sym(input$variableSelect))) +
        labs(y = "Employee Count") +
        geom_bar() +
        theme_classic()
    }
  })
  output$table <- renderTable({
    # Tables
    reactive_table() %>% 
      group_by(across(where(is.factor))) %>%
      summarize(mean = mean(!!sym(input$variableSelect), na.rm = TRUE),
                sd = sd(!!sym(input$variableSelect), na.rm = TRUE))
  })
} 
#Run the application
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()