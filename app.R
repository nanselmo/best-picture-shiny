# This app uses data from https://cs.uwaterloo.ca/~s255khan/oscars.html

library(shiny)
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
      mainPanel(
         plotOutput("bestPicturePlot")
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
   
   output$bestPicturePlot <- renderPlot({
     plot(filtered_picture_data()$year, filtered_picture_data()$rating, main="Best Picture Ratings by Year", 
          xlab="Year ", ylab="Rating ", pch=19)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

