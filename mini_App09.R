library(tidyverse)
library(readxl)
library(shiny)
#library(ggplot2)
#library(shinyWidgets)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ny_weather <- read_csv("us/KNYC.csv")
library(vroom)

#dir.create("us")
# 
#download <- function(name) {
#url <- "https://github.com/fivethirtyeight/data/raw/master/us-weather-history/"
#download.file(paste0(url, name), paste0("us/",name), quiet = TRUE)
#}
#download("KNYC.csv")
#download("KPHX.csv")
#download("KSEA.csv")
#download("KCLT.csv")
#download("KCQT.csv")
#download("KHOU.csv")
#download("KIND.csv")

nyc <- vroom::vroom("us/KNYC.csv")
phx <- vroom::vroom("us/KPHX.csv")
sea <- vroom::vroom("us/KSEA.csv")
hou <- vroom::vroom("us/KHOU.csv")
ind <- vroom::vroom("us/KIND.csv")

#make sure 'date' is recognized as dates:
#ny_weather$date <- as.Date(ny_weather$date, format = "%Y-%m-%d")

nyc$date <- as.Date(nyc$date, format = "%Y-%m-%d")
phx$date <- as.Date(phx$date, format = "%Y-%m-%d")
sea$date <- as.Date(sea$date, format = "%Y-%m-%d")
hou$date <- as.Date(hou$date, format = "%Y-%m-%d")
ind$date <- as.Date(ind$date, format = "%Y-%m-%d")

# Define UI for application
ui <- fluidRow(
          h1("Exploration of record temperatures"),
      # Select variable for y-axis
          
      #mainPanel(
        #fluidRow(
          column(width = 8, class = "well",
           HTML("<strong>Explanation:</strong> <br>Below are shown the temperatures during a year in a selected city. <br> The outer lightgrey band shows the record temperature on a particular day since 1880. <br> The middle steelgrey band shows the average min and max temperatures since 1880. <br> The black band shows the actual min and max temperatures for that day. <br><br> <strong> Instructions: </strong> <br>Use the dropdown menu to select a city to investigate. <br>  You can then click and drag on the top graph, to zoom in on the selected area in the bottom graph. <br> <br>"),
           ),
           column(width = 1, selectInput("city", label = HTML("<strong>Select city:</strong>"), choices = c("New York","Phoenix","Seattle","Indianapolis","Houston")), selected = "nyc"),
           fluidRow(
                    plotOutput("graph1", height = "250px",
                               brush = brushOpts(
                                 id = "plot1_brush",
                                 resetOnNew = TRUE)
                    ),
                    plotOutput("graph2", height = "250px")
              )
              
              )
              #)
              #)
    
    
server <- function(input,output,session) {
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  location <- reactive({
    if (input$city == "New York"){data <- nyc}
    else if (input$city == "Phoenix"){data <- phx}
    else if (input$city == "Seattle"){data <- sea}
    else if (input$city == "Indianapolis"){data <- ind}
    else if (input$city == "Houston"){data <- hou}
    data
        })
 
  #observeEvent(location(), {})
  
  output$graph1 <- renderPlot({
    location() %>% ggplot(aes(x=date)) + 
      geom_ribbon(aes(ymin = record_min_temp, ymax = record_max_temp, fill = "Record temperatures", alpha = "Record temperatures")) +
      geom_ribbon(aes(ymin = average_min_temp, ymax = average_max_temp, fill = "Average temperatures", alpha = "Average temperatures")) +
      geom_ribbon(aes(ymin = actual_min_temp, ymax = actual_max_temp, fill = "Actual temperatures", alpha = "Actual temperatures")) +
      geom_point(data=location() %>% 
                   filter(actual_min_temp<=record_min_temp), 
                 aes(x=date,y=actual_min_temp), 
                 color='blue',
                 size=2)+
      geom_point(data=location() %>% 
                   filter(actual_max_temp>=record_max_temp), 
                 aes(x=date,y=actual_max_temp), 
                 color='red',
                 size=2)+
                  labs(x = "Date", y = "Temperature") +
      theme_bw() +
      scale_fill_manual(values=c("Record temperatures" = "black", "Average temperatures" = "black", "Actual temperatures" = "black"), guide = F) +
      scale_alpha_manual(values=c("Record temperatures" = 0.1, "Average temperatures" = 0.35, "Actual temperatures" = 1)) + 
      guides(alpha=guide_legend(title="Legend")) #+ scale_x_date(breaks = "2 month")
  
    })
    
  output$graph2 <- renderPlot({
    location() %>% ggplot(aes(x=date)) + 
      geom_ribbon(aes(ymin = record_min_temp, ymax = record_max_temp, fill = "Record temperatures", alpha = "Record temperatures")) +
      geom_ribbon(aes(ymin = average_min_temp, ymax = average_max_temp, fill = "Average temperatures", alpha = "Average temperatures")) +
      geom_ribbon(aes(ymin = actual_min_temp, ymax = actual_max_temp, fill = "Actual temperatures", alpha = "Actual temperatures")) +
      geom_point(data=location() %>% 
                   filter(actual_min_temp<=record_min_temp), 
                 aes(x=date,y=actual_min_temp), 
                 color='blue',
                 size=2)+
      geom_point(data=location() %>% 
                   filter(actual_max_temp>=record_max_temp), 
                 aes(x=date,y=actual_max_temp), 
                 color='red',
                 size=2)+
      labs(x = "Date", y = "Temperature") +
      scale_fill_manual(values=c("Record temperatures" = "black", "Average temperatures" = "black", "Actual temperatures" = "black"), guide = F) +
      scale_alpha_manual(values=c("Record temperatures" = 0.1, "Average temperatures" = 0.35, "Actual temperatures" = 1)) + 
      guides(alpha=guide_legend(title="Legend"))+
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + theme_bw() #+ scale_x_date(date_breaks = "1 day")
  })
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges2$x <- as.Date(c(brush$xmin, brush$xmax), origin="1970-01-01")
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
}
shinyApp(ui, server)
