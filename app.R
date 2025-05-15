library(shiny)
library(tidyverse)
library(dataRetrieval)
library(bslib)
library(reactable)
library(leaflet)


df <- readxl::read_excel("data/wq_data_processes.xlsx")
sites <- readxl::read_excel("data/wq_sites.xls")

#TODO: convert date from dateTime to date only
#TODO: round all numerical data to 3 decimals

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
                               selected = unique(df$SiteID)
                               ),
            
            radioButtons("depth",
                         label = "Select Depth",
                         choices = unique(df$RelativeDepth)
            ),
            
            
    ),
  
  
  mainPanel(
    
    card(
      leafletOutput("map")
    ),
    
    
    card(
      plotOutput("boxplot"),
      
      checkboxInput("outliers",
                    label = "Include Outliers",
                    value = TRUE
                    )
      ),
    
    card(reactableOutput("table"))
    
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    df %>% filter(SiteID %in% input$sites & RelativeDepth %in% input$depth)
  })
  
  #req(input$sites) # require at least one site to be selected
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-87.044831, 30.367050, zoom = 12)
  })
  
  output$boxplot <- renderPlot({
      ggplot(filtered_data(), aes(x = SiteID, y = .data[[input$var]])) +
      geom_boxplot(outliers = input$outliers) +
      labs(x = "Sites", y = input$var, title = paste("Boxplot of", input$var, "by Site")) 
  })
  
  output$table <- renderReactable({reactable(filtered_data(), defaultPageSize = 5)})
   


}

  shinyApp(ui, server)


