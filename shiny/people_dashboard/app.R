library(tidyverse)
library(shiny)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # commented out once app was complete

ui <- fluidPage(
  
  # Application title
  titlePanel("People Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Drop down selectors
      selectInput("variableSelect",
                  "Outcome Variable of Interest?",
                  c("Monthly_Pay", "Turnover_Status", "Job_Satisfaction"),
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
   filtered_tbl <- reactive({
    filtered_skinny_tbl <- skinny_tbl
    # Department Filter
    if (input$departmentSelect != "All") {
      filtered_skinny_tbl <- filtered_skinny_tbl %>%
        filter(Department == input$departmentSelect)
    }
    # Education Filter
    if (input$educationSelect != "All") {
      filtered_skinny_tbl <- filtered_skinny_tbl %>%
        filter(EducationField == input$educationSelect)
    }
    # Gender Filter
    if (input$genderSelect != "All") {
      filtered_skinny_tbl <- filtered_skinny_tbl %>%
        filter(Gender == input$genderSelect)
    }
    # Job Filter
    if (input$jobSelect != "All") {
      filtered_skinny_tbl <- filtered_skinny_tbl %>%
        filter(JobRole == input$jobSelect)
    }
    # Outcome Filter
    if (input$variableSelect != "None") { #included this to keep the format of the other filters. "None" will never be selected (not an option as an input for the field), so this filter will always be in play! The dynamic results of the plots and table depend on a dynamic outcome variable input -- this allowed me to make that happen at the same time! 
      filtered_skinny_tbl <- filtered_skinny_tbl %>%
        select(input$variableSelect)
    }
  })
  output$plot <- renderPlot({
    # Plots
    if (input$variableSelect == "Monthly_Pay") {
      filtered_tbl() %>%
      ggplot(aes(x = Monthly_Pay)) +
        geom_histogram(binwidth = 1000, color = "white") +
        labs(x = "Monthly Pay", y = "Employee Count") +
        theme_classic()
    }
    else if (input$variableSelect == "Turnover_Status") {
      filtered_tbl() %>%
        ggplot(aes(x = Turnover_Status)) +
        labs(x = "Turnover_Status", y = "Count of Employees") + 
        geom_bar() +
        theme_classic()
    }
    else if (input$variableSelect == "Job_Satisfaction") {
      filtered_tbl() %>%
        ggplot(aes(x = Job_Satisfaction)) +
        geom_bar() +
        labs(x = "Job Satisfaction", y = "Count of Employees") + 
        theme_classic()
    }
  })
  output$table <- renderTable({
    # Tables
    if (input$variableSelect == "Monthly_Pay") {
      summarize(filtered_tbl(), Mean = mean(Monthly_Pay),
                SD = sd(Monthly_Pay))
    }
    else if (input$variableSelect == "Turnover_Status") {
      summarize(filtered_tbl(), Mean = mean(as.numeric(Turnover_Status) - 1),
                SD = sd(as.numeric(Turnover_Status))
      )
    }
    else if (input$variableSelect == "Job_Satisfaction") {
      summarize(filtered_tbl(), Mean = mean(Job_Satisfaction),
                SD = sd(Job_Satisfaction)
      )
    }
    if (nrow(filtered_tbl()) == 0) {
      tibble("No employees meet the criteria selected" = "Please change Filters for new results")
    }
  })
} 
#Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()