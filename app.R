# This app uses data from https://cs.uwaterloo.ca/~s255khan/oscars.html

library(shiny)
library(dplyr) #library for simplifying dataframe manipulation
library(rCharts) #add interactive charts
picture_data_raw<-read.csv('pictures.csv')
picture_data <- picture_data_raw %>% select (-synopsis, -genre2)

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
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Year Over Year Plot", showOutput("bestPicturePlot", "polycharts")),
                  tabPanel("Count By Genre Plot", showOutput("genreCountBar", "polycharts")),
                  tabPanel("Data Table", chartOutput('movieTable', 'datatables'))
                   
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
  
  get_genre_count <- reactive({
    genre_data<-filtered_picture_data() %>% group_by(genre1) %>%
      summarize(num_movies = n())
  })
  
   output$bestPicturePlot <- renderChart({
     p1 <- rPlot(rating~year, data = filtered_picture_data(),
                 color = 'genre1',
                 type = 'point',
                 tooltip = "#!function(movie){ return 'Title: ' + movie.name 
                 + '(' + movie.genre1 + ')'}!#")
     p1$addParams(dom = 'bestPicturePlot')
     return(p1)
   })
   
   output$genreCountBar <- renderChart({
     
     barPlot <- rPlot(x = list(var = "genre1", sort = "num_movies"), y = "num_movies", 
                      data = get_genre_count(), type = 'bar')
     barPlot$addParams(dom = 'genreCountBar')
     return(barPlot)
    })
   
   output$movieTable <- renderChart2({
     dTable(filtered_picture_data())
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

