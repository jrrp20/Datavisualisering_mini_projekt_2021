library(tidyverse)
library(readxl)
library(shiny)
#library(ggplot2)
#library(shinyWidgets)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ny_weather <- read_csv("us/KNYC.csv")
library(vroom)
# ½
#dir.create("us")
# 
#download <- function(name) {
#url <- "https://github.com/fivethirtyeight/data/raw/master/us-weather-history/"
#download.file(paste0(url, name), paste0("us/",name), quiet = TRUE)
#}
#download("KNYC.csv")
#download("KPHX.csv")
#download("KSEA.csv")

nyc <- vroom::vroom("us/KNYC.csv")
phx <- vroom::vroom("us/KPHX.csv")
sea <- vroom::vroom("us/KSEA.csv")

#make sure 'date' is recognized as dates:
#ny_weather$date <- as.Date(ny_weather$date, format = "%Y-%m-%d")

nyc$date <- as.Date(nyc$date, format = "%Y-%m-%d")
phx$date <- as.Date(phx$date, format = "%Y-%m-%d")
sea$date <- as.Date(sea$date, format = "%Y-%m-%d")

# Define UI for application that plots features of movies
ui <- fluidRow(
          column(6, selectInput("city", label = "City:", choices = c("New York","Phoenix","Seattle")), selected = "nyc"),
      # Select variable for y-axis
          
      #mainPanel(
        #fluidRow(
          column(width = 8, class = "well",
           h4("Top controls bottom"),
           fluidRow(
                    plotOutput("graph1", height = "250px",
                               brush = brushOpts(
                                 id = "plot1_brush",
                                 resetOnNew = TRUE)
                    ),
                    plotOutput("graph2", height = "250px")
              )
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
    data
        })
 
  #observeEvent(location(), {})
  
  output$graph1 <- renderPlot({
    location() %>% ggplot(aes(x=date)) + 
      geom_ribbon(aes(ymin = record_min_temp, ymax = record_max_temp, fill = "Record temperatures", alpha = "Record temperatures")) +
      geom_ribbon(aes(ymin = average_min_temp, ymax = average_max_temp, fill = "Average temperatures", alpha = "Average temperatures")) +
      geom_ribbon(aes(ymin = actual_min_temp, ymax = actual_max_temp, fill = "Actual temperatures", alpha = "Actual temperatures")) +
                  labs(x = "Date", y = "Temperature") +
      theme_bw() +
      scale_fill_manual(values=c("Record temperatures" = "black", "Average temperatures" = "black", "Actual temperatures" = "black"), guide = F) +
      scale_alpha_manual(values=c("Record temperatures" = 0.1, "Average temperatures" = 0.35, "Actual temperatures" = 1)) + 
      guides(alpha=guide_legend(title="Legend"))
    #+ scale_x_date(breaks = "2 month")
  
    })
    
  output$graph2 <- renderPlot({
    location() %>% ggplot(aes_(x=as.name("date"))) +
      geom_ribbon(aes(ymin = record_min_temp, ymax = record_max_temp),
                  fill = "grey", alpha = .4)+ labs(x = "Date", y = "Temperature") +
      geom_ribbon(aes(ymin = average_min_temp, ymax = average_max_temp),
                  fill = "grey", alpha = .8)+ labs(x = "Date", y = "Temperature") +
      geom_ribbon(aes(ymin = actual_min_temp, ymax = actual_max_temp),
                  fill = "black", alpha = 0.6)+ labs(x = "Date", y = "Temperature") +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + theme_bw()#+
      #scale_x_date(date_breaks = "1 day")
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