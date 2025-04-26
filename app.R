library(tidyverse)
library(shiny)
library(shinythemes) # Load shinythemes package

# Data ----------------------
europe <- readRDS("Data/europe.rds")
# Correct Celsius conversion
europe_cel <- mutate(europe, AvgTemperatureC = (AvgTemperatureF - 32) * 5/9)



# User interface -----------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("COURSE APP"),
  "This is the course shiny app. It is created during the course. It contains 
  Average daily temperatures (in Fahrenheit) from cities around Europe from 2000 to 2019.",
  
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      "This is the sidebar panel",
      # Input: A simple slider ----
      sliderInput(inputId = "year", label = "Year",
                  min = 2000,
                  max = 2019,
                  step = 1,
                  value = 2000,
                  sep= ''),
      selectInput(inputId = "country", label = "Country",
                  choices=sort(unique(europe_cel$Country)),
                  multiple=FALSE),
      textInput(inputId = "city", label = "Input City Name here",
                placeholder = "Please type city name in capitals"),
      radioButtons(inputId = "radio_1", label = "Radio Buttons:",
                   choices = list("By Mounth"=1, "By Year"=2),
                   selected=1)
    ),
    # Main panel
    mainPanel(
      "This is the main panel",
      textOutput(outputId = "text_output"),  # <- Display the text input
      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           verbatimTextOutput(outputId = "data_summary")),
                  tabPanel(title = "Data",
                           dataTableOutput("data_table")),
                  tabPanel(title = "Plots",
                           plotOutput("barplot"),
                           #plotOutput("lineplot"),
                           #plotOutput("boxplot")
                  ))
    )
  )
)


# Server --------------------------- 

server <- function(input, output) {
  country_df <- reactive ({
    europe_cel %>%
      filter(Year >= input$year) %>%
      filter(Country=input$country)
  })
  city_df <- reactive({
    country_df() %>%
      filter(City==input$city) %>%
      filter(Year == input$year)
    
  })
  
  output$text_output <- renderText({
    paste("Your inputs are:", input$year, input$country, input$text_input, input$radio_1)
  })
  
  # Output: Render a print output ----
  output$data_summary <- renderPrint({
    summary(europe_cel)
  })
  output$data_table <- renderDataTable({
    city_df
  })
  barplot_df <- europe_cel %>%
    filter(Country == "Switzerland") %>%
    group_by(Month) %>%
    summarise(AvgTemperatureC = mean(AvgTemperatureC)) %>%
    ungroup()
  
  # Output: Render a plot output ----
  output$barplot_df <- renderPlot({
    ggplot(city_df(), aes(x = factor(Month), y = AvgTemperatureC)) +
      geom_col(fill = "skyblue") + # You can use geom_bar(stat="identity") too
      theme_minimal() +
      scale_x_discrete(labels=month.abb) +
      labs(title = "Average Monthly Temperature in Switzerland (Celsius)",
           x = "Month",
           y = "Avg Temperature (°C)")
    
  })}

shinyApp(ui = ui, server = server)