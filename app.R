library(shiny)
library(tidyverse)
library(dataRetrieval)
library(bslib)


df <- readxl::read_excel("C:/Users/michaelf/PythonProjects/wq-app/data/processed/wq_data_processed.xlsx")


ui <- page_sidebar(
  title = "SRC Water Quality Portal",
  sidebar = 
    sidebar("Filter Data", 
            selectInput(
              "var",
              label = "Select Variable",
              choices = names(df %>% select(where(is.numeric))),
              multiple = FALSE
              ),
            
            checkboxGroupInput("sites",
                               label = "Select Site(s)",
                               choices = unique(df$SiteID),
                               )
            
            ),
  
  card(plotOutput("boxplot"))
  
    

  
  
  )

server <- function(input, output, session) {
  filtered_data <- reactive({
    df %>% filter(site %in% input$sites)
  })
  
  req(input$sites) # require at least one site to be selected
  
  output$boxplot <- renderPlot({
      ggplot(filtered_data(), aes(x = SiteID, y = .data[[input$var]])) +
      geom_boxplot() +
      labs(x = "Sites", y = input$var, title = paste("Boxplot of", input$var, "by Site")) 
  })
  
}

shinyApp(ui, server)


