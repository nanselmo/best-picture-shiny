# This app uses data from https://cs.uwaterloo.ca/~s255khan/oscars.html

library(shiny)
library(dplyr) #library for simplifying dataframe manipulation
library(rCharts) #add interactive charts
picture_data<-read.csv('pictures.csv')

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Oscars - Best Picture"),
   
   # Sidebar with a slider input for years
   sidebarLayout(
      sidebarPanel(
         sliderInput("yearInput",
                     "Year Range:",
                     min = 1927,
                     max = 2014,
                     value = c(1990,2000)
                     )
      ),
      
      # Plot of the filtered data
      #mainPanel(
         #plotOutput("bestPicturePlot")
      #)
      mainPanel(
        showOutput("bestPicturePlot", "polycharts")
      )
   )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  # Return the filtered data
  filtered_picture_data <- reactive({
    picture_data %>%
      filter(year >= input$yearInput[1],
             year <= input$yearInput[2])
  })
  
   output$bestPicturePlot <- renderChart({
     p1 <- rPlot(rating~year, data = filtered_picture_data(),
                 type = 'point',
                 tooltip = "#!function(movie){ return 'Title: ' + movie.name}!#")
     p1$addParams(dom = 'bestPicturePlot')
     return(p1)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

