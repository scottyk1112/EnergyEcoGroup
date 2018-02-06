#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Reading in the CLUM Data
CLUMData <- read.csv("/Users/scottkaplan1112/Box Sync/Graduate School/A_DS421/Spring 2018 Project/EnergyEcoGroup_FinalProject/GFN_Data_Visualization/NFA_2017_CLUM.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("CLUM Data Exploration"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- CLUMData$total
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    histinfo <- hist(log(x), n = 30, col = 'darkgray', border = 'white')
    with(histinfo,plot(counts~exp(mids),type="h",lwd=33,col=8,log="x",lend=2, xlab="Total GHA/Capita"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



