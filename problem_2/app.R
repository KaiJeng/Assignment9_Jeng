library(shiny)
library(tidyverse)
library(gapminder)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1880, max = 2014, value = c(1900, 2000)),
      textInput("countryInput", "Country")),
    mainPanel(
      plotOutput("main_plot"),
      tableOutput("results")
    )
  ),
  titlePanel("Mind the gap")
)
server <- function(input, output, session) {
  reduced_df <- reactive({
    filter(gapminder, country == input$countryInput, 
           year >= input$yearInput[1] & year <= input$yearInput[2]) %>% mutate(logtenGDP = log10(gdpPercap))
  })
  output$main_plot <- renderPlot({
    ggplot(data = reduced_df(), 
           aes(logtenGDP, lifeExp)) + 
      geom_point() + ggtitle("Results") + xlab("Log10 of per capita GDP") + ylab("Life Expectency")
  })
  output$results <- renderTable({ 
    select(reduced_df(), -gdpPercap)
  },digits = 0)
}
shinyApp(ui = ui, server = server)