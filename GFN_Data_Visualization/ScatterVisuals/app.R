#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

# Reading in the CLUM Data
#CLUMData <- read.csv("/Users/scottkaplan1112/Box Sync/Graduate School/A_DS421/Spring 2018 Project/EnergyEcoGroup_FinalProject/GFN_Data_Visualization/NFA_2017_CLUM.csv")
CLUMData <- read.csv("../NFA_2017_CLUM.csv")

# Taking logs of all of the variables for scaling
cols <- c(names(CLUMData[,6:13]))
CLUMData[cols] <- log(CLUMData[cols])
setnames(CLUMData, old = c(names(CLUMData[,6:13])), new = c("Log Coicop Expenditure", "Log Crop-Land", "Log Grazing-Land", "Log Forest-Land", "Log Fishing-Ground", "Log BuiltUp-Land", "Log Carbon", "Log Total"))

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  headerPanel('Initial Visual Exploration of CLUM Data'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(CLUMData[,6:13])),
    selectInput('ycol', 'Y Variable', names(CLUMData[,6:13])),
    selectInput('zcol', 'Z Variable', unique(CLUMData[,5]),
                selected=names(CLUMData)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    CLUMData_subset <- subset(CLUMData, clum7_name==input$zcol)
    CLUMData_subset[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

