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
                  c("MonthlyIncome", "Attrition", "JobSatisfaction"),
                  selected = "MonthlyIncome"),
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
    if (input$variableSelect != "None") {
      filtered_skinny_tbl <- filtered_skinny_tbl %>%
        select(input$variableSelect)
    }
  })
  output$plot <- renderPlot({
    # Plots
    if (input$variableSelect == "MonthlyIncome") {
      filtered_tbl() %>%
      ggplot(aes(x = MonthlyIncome)) +
        geom_histogram(binwidth = 1000, color = "white") +
        labs(x = "Monthly Income", y = "Employee Count") +
        theme_classic()
    }
    else if (input$variableSelect == "Attrition") {
      filtered_tbl() %>%
        ggplot(aes(x = Attrition)) +
        geom_bar() +
        theme_classic()
    }
    else if (input$variableSelect == "JobSatisfaction") {
      filtered_tbl() %>%
        ggplot(aes(x = JobSatisfaction)) +
        geom_bar() +
        labs(x = "Job Satisfaction", y = "Employee Count") + 
        theme_classic()
    }
  })
  output$table <- renderTable({
    # Tables
    if (input$variableSelect == "MonthlyIncome") {
      summarize(filtered_tbl(), Mean = mean(MonthlyIncome),
                SD = sd(MonthlyIncome))
    }
    else if (input$variableSelect == "Attrition") {
      summarize(filtered_tbl(), Mean = mean(as.numeric(Attrition) - 1),
                SD = sd(as.numeric(Attrition))
      )
    }
    else if (input$variableSelect == "JobSatisfaction") {
      summarize(filtered_tbl(), Mean = mean(JobSatisfaction),
                SD = sd(JobSatisfaction)
      )
    }
  })
} 
#Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()