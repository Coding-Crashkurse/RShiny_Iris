## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)
library(highcharter)


df <- data.table::fread("C:/Users/User/Desktop/ShinyIris/Iris/iris.csv") 
 
df %>%
  group_by(variety) %>%
  summarise(n = n())


ui <- dashboardPage(
    dashboardHeader(title = "Iris Dashboard", titleWidth = 400),
    dashboardSidebar(
      includeCSS("www/style.css"),
      div(class = "sidebar_header", h2("Wähle deine Variablen aus")),
      selectizeInput("var_a", "Variable A", choices = colnames(df)[1:4], selected = colnames(df)[1]),
      selectizeInput("var_b", "Variable B", choices = colnames(df)[1:4], selected = colnames(df)[2]),
      sliderInput("kMeans_slider", "Wähle die Anzahl der Cluster", min = 2, max = 8, value = 3, ticks = FALSE)
    ),
    dashboardBody(
      h1("Willkommen auf diesem Example Dashboard"),
      h3("Analysiere den Datensatz"),
      box(width = 12, title = "Ergebnis",
          highchartOutput("chart") 
      ),
      box(width = 12, "Clusterinformationen",
          DT::dataTableOutput("clusterinformation")       
      )
    )
)

server <- function(input, output, session) {
  
  
  chart_data <- reactive({
    sub_df <- df %>%
      select(c(input$var_a, input$var_b))
    
    sub_df$cluster <- kmeans(sub_df, input$kMeans_slider)$cluster
    sub_df
  })
  
  summarised_data <- reactive({
    df <- chart_data()
    print(df)
    df %>%
      group_by(cluster) %>%
      summarise(Anzahl = n())
  })
  
  output$chart <- renderHighchart({
    
    highchart() %>%
      hc_add_series(chart_data(), type = "scatter", 
                    hcaes(x = !!sym(input$var_a), y = !!sym(input$var_b), group = "cluster")) %>%
      hc_tooltip(pointFormat = "y: {point.y} <br> x: {point.x}")
  })
  
  output$clusterinformation <- DT::renderDataTable({
    summarised_data()
  })
    
}

shinyApp(ui, server)