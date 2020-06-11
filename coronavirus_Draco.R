# load library

library_names <- c(
  "jsonlite",
  "httr",
  "rvest",
  "DT",
  "dplyr",
  "shiny",
  "tidyverse",
  "plotly",
  "ggplot2"
)
lapply(library_names, require, character.only = TRUE)

# obtain data from API
library(httr)
library(jsonlite)
library(rvest)
library(DT)
library(dplyr)
library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)

country_list <-
  c("china",
    "united-states",
    "canada",
    "united-kingdom",
    "brazil",
    "italy",
    "russia")
code_country <- c("CN","US","CA","UK","BR","IT","RU")
jsons_collection <- lapply(country_list, function(x) {
  r <-
    RETRY(
      "GET",
      paste0("https://api.covid19api.com/total/country/", x),
      pause_min = 1,
      times = 50
    )
  json <- content(r, as = "text", encoding = "UTF-8")
  return(fromJSON(json))
})

mydata <- do.call("rbind", jsons_collection)
mydata$Country_Code <-
  rep(code_country, sapply(jsons_collection, nrow))
mydata$Country <- as.factor(mydata$Country)
mydata$Country_Code <- as.factor(mydata$Country_Code)
time_pattern_str = c("%Y-%m-%d", "%Y/%m/%d")
mydata$TimeStamp <-
  as.Date(
    str_extract(mydata$Date, "(\\d{4}-\\d{2}-\\d{2})"),
    tryFormats = time_pattern_str
  )
mydata <-
  mydata[, c("Country",
             "Confirmed",
             "Deaths",
             "Recovered",
             "Active",
             "Country_Code",
             "TimeStamp")]

# all countries information
r1 <-
  RETRY(
    "GET",
    "https://api.covid19api.com/summary",
    pause_min = 1,
    times = 50
  )
json <- content(r1, as = "text", encoding = "UTF-8")
all_country_data <- fromJSON(json)

ui <- fluidPage(title = "COVID-19 pandemic",
                tabsetPanel(
                  tabPanel(title = "Country Data",
                           DT::dataTableOutput("render_summary")),
                  tabPanel(
                    title = "Scatterplot (2D)",
                    plotlyOutput("plot_2D", width = "1000px", height = "1000px")
                  ),
                  tabPanel(title = "Scatterplot (3D)",
                           mainPanel(
                             plotlyOutput("plot_3D", width = "1000px", height = "1000px")
                           )),
                  tabPanel(title = "Scatterplot (3D) in Log Scale",
                           mainPanel(
                             plotlyOutput("plot_3D_log", width = "1000px", height = "1000px")
                           )),
                  tabPanel(title = "Time Series",
                           sidebarPanel(htmlOutput(
                             "features_selector"
                           )),
                           mainPanel(plotlyOutput("timeSeriesPlot")))
                ))

server <- function(input, output, session) {
  output$render_summary <- DT::renderDataTable({
    each_country_data <-
      all_country_data$Countries[, c(
        "Country",
        "NewConfirmed",
        "TotalConfirmed",
        "NewDeaths",
        "TotalDeaths",
        "NewRecovered",
        "TotalRecovered"
      )]
    names(each_country_data) <-
      c(
        "Country",
        "New Confirmed",
        "Total Confirmed",
        "New Deaths",
        "Total Deaths",
        "New Recovered",
        "Total Recovered"
      )
    each_country_data[order(each_country_data[, "New Confirmed"], decreasing = TRUE), ]
  })
  output$plot_2D <- renderPlotly({
    plot_ly(
      all_country_data$Countries,
      x = ~ TotalConfirmed,
      y = ~ TotalDeaths,
      text = ~ Country,
      type = "scatter",
      mode = "markers",
      color = ~ Country,
      marker = list(
        size = ~ TotalRecovered * 0.0001,
        opacity = 0.5
      )
    ) %>%
      layout(
        title = 'Total Deaths against Total Confirmed per Country',
        xaxis = list(showgrid = FALSE, title = "Total Confirmed Counts"),
        yaxis = list(showgrid = FALSE, title = "Total Deaths Counts")
      )
  })
  output$plot_3D <- renderPlotly({
    plot_ly(
      all_country_data$Countries,
      x = ~ TotalConfirmed,
      y = ~ TotalDeaths,
      z = ~ TotalRecovered,
      text = ~ Country,
      color = ~ Country,
      mode = "markers"
    ) %>% layout(title = "Total Confirmed, Total Deaths and Total Recovered in 3d scatter plot, marked by Country",
                 scene = list(
                   xaxis = list(title = "Total Confirmed Cases"),
                   yaxis = list(title = "Total Deaths"),
                   zaxis = list(title = "Total Recovered")
                 ))
  })
  output$plot_3D_log <- renderPlotly({
    country_dataframe = all_country_data$Countries
    country_dataframe$logTotalConfirmed = log(country_dataframe$TotalConfirmed)
    country_dataframe$logTotalDeaths = log(country_dataframe$TotalDeaths)
    country_dataframe$logTotalDeaths[country_dataframe$logTotalDeaths < 0] = 0
    country_dataframe$logTotalRecovered = log(country_dataframe$TotalRecovered)
    country_dataframe$logTotalRecovered[country_dataframe$logTotalRecovered < 0] = 0
    plot_ly(
      country_dataframe,
      x = ~ logTotalConfirmed,
      y = ~ logTotalDeaths,
      z = ~ logTotalRecovered,
      text = ~ Country,
      color = ~ Country,
      mode = "markers"
    ) %>% layout(title = "Log Total Confirmed, Log Total Deaths and Log Total Recovered in 3d scatter plot, marked by Country",
                 scene = list(
                   xaxis = list(title = "Log Total Confirmed Cases"),
                   yaxis = list(title = "Log Total Deaths"),
                   zaxis = list(title = "Log Total Recovered")
                 ))
  })
  output$features_selector = renderUI({
    selectInput(
      inputId = "feacture_picks",
      label = "feactures:",
      choices = c("Confirmed", "Deaths", "Recovered"),
      selected = "Confirmed"
    )
  })
  output$countries_selector = renderUI({
    selectInput(
      inputId = "feacture_picks",
      label = "feactures:",
      choices = c("Confirmed", "Deaths", "Recovered"),
      selected = "Confirmed"
    )
  })
  output$timeSeriesPlot <- renderPlotly({
    if (!is.null(input$feacture_picks)) {
      if (input$feacture_picks == "Confirmed") {
        plot_ly(
          mydata,
          x = ~ TimeStamp,
          y = ~ Confirmed,
          type = 'scatter',
          mode = 'lines',
          color = ~ Country
        ) %>% layout(title = "Time series plot of confirmed cases",
                     scene = list(
                       xaxis = list(title = "Total number of confirmed cases"),
                       yaxis = list(title = "Date")
                     ))
      } else if (input$feacture_picks == "Deaths") {
        plot_ly(
          mydata,
          x = ~ TimeStamp,
          y = ~ Deaths,
          type = 'scatter',
          mode = 'lines',
          color = ~ Country
        ) %>% layout(title = "Time series plot of deaths cases",
                     scene = list(
                       xaxis = list(title = "Total number of deaths cases"),
                       yaxis = list(title = "Date")
                     ))
      } else if (input$feacture_picks == "Recovered") {
        plot_ly(
          mydata,
          x = ~ TimeStamp,
          y = ~ Recovered,
          type = 'scatter',
          mode = 'lines',
          color = ~ Country
        ) %>% layout(title = "Time series plot of recovered cases",
                     scene = list(
                       xaxis = list(title = "Total number of recovered cases"),
                       yaxis = list(title = "Date")
                     ))
      }
    }
  })
}

shinyApp(server = server, ui = ui)
