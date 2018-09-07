#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

iris.load<- iris %>%
  mutate(Sepal.Length = as.numeric(Sepal.Length),
         Sepal.Width = as.numeric(Sepal.Width),
         Petal.Length = as.numeric(Petal.Length),
         Petal.Width = as.numeric(Petal.Width),
         name = as.factor(Species))

pdf(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Iris NavBar", 
             theme = shinytheme("united"),
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("species_select",
                                      "Species:",
                                      choices = levels(iris$Species),
                                      multiple = TRUE,
                                      selectize = TRUE,
                                      selected = c("setosa", "versicolor","virginica")),
                          sliderInput("Petal.length_select",
                                      "Petal length:",
                                      min = min(iris.load$Petal.Length, na.rm = T),
                                      max = max(iris.load$Petal.Length, na.rm = T),
                                      value = c(min(iris.load$Petal.Length, na.rm = T), max(iris.load$Petal.Length, na.rm = T)),
                                      step = 1)
                        ),
                        # Output plot
                        mainPanel(
                          plotlyOutput("plot")
                        )
                      )
             ),
             # Data Table
             tabPanel("Table",
                      fluidPage(DT::dataTableOutput("table"))
             )
  )
)
# Define server logic
server <- function(input, output) {
  output$plot <- renderPlotly({
    dat <- subset(iris.load, Species %in% input$species_select)
    ggplot(data = dat, aes(x = Species, y = Petal.Length, fill = Species)) + geom_bar(stat = "identity")
  })
  output$table <- DT::renderDataTable({
    subset(iris, Species %in% input$species_select, select = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
