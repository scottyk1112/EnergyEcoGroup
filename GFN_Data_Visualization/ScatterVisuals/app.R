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
CLUMData <- read.csv("NFA_2017_CLUM.csv")
# Path for Eli in debugging outside of Shiny
#CLUMData <- read.csv("C:/Users/Eli/GitFolders/EnergyEcoGroup/GFN_Data_Visualization/ScatterVisuals/NFA_2017_CLUM.csv")


cols <- c(names(CLUMData[,6:13]))
#Log transformed data
CLUMDatalog <- CLUMData
CLUMDatalog[cols] <- log(CLUMDatalog[cols])

#Friendly names
setnames(CLUMData, old = c(names(CLUMData[,6:13])), new = c("Coicop Expenditure", "Crop-Land", 
                                                            "Grazing-Land", "Forest-Land", "Fishing-Ground",
                                                            "BuiltUp-Land", "Carbon", "Total"))

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  headerPanel('Initial Visual Exploration of CLUM Data'),
  sidebarPanel(width = 3,
    #selection of GTAP years, 2011 as default
    checkboxGroupInput("Select_years", "Years", unique(CLUMData[,1]), selected = "2011", FALSE),
    selectInput('xcol', 'X Variable', names(CLUMData[,6:13])),
    selectInput('ycol', 'Y Variable', names(CLUMData[,6:13]), selected = "Total"),
    selectInput('zcol', 'Z Variable', unique(CLUMData[,5]),
                selected=names(CLUMData)[[2]]),
    selectInput('scale', 'Scale', c("normal","log"), selected="log"),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    fluidRow(
     column(width = 6, class = "well",
            h4("Selection controls right plot"),
            
      plotOutput('plot1', hover = "plot_hover",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE)
       )
     ),
     column(width = 6, class = "well",
            h4("Zoomed from selection"),
      plotOutput("plot2", hover="plot_hover2")
                       )
             ),

    fluidRow(
     column(width = 6,
      h6("Hover IDs"),
      verbatimTextOutput("info")),
     column(width = 6,
      h6("Hover IDs Zoomed"),
       verbatimTextOutput("info2")
      )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    CLUMData_subset <- subset(
      #Choice option to use log transformed data
      if(input$scale == "normal") {CLUMData}
        else {CLUMDatalog},
      # Fliter by choices, including years
      clum7_name==input$zcol & year %in% input$Select_years)
    CLUMData_subset[,c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    abline(a=0, b=1, h=NULL, v=NULL, col="grey")
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
    
   output$plot2 <- renderPlot({
     palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
               "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
     
     par(mar = c(5.1, 4.1, 0, 1))
     plot(selectedData(), xlim=c(input$plot_brush$xmin, input$plot_brush$xmax), 
          ylim=c(input$plot_brush$ymin, input$plot_brush$ymax),
          col = clusters()$cluster,
          pch = 20, cex = 3)
          abline(a=0, b=1, h=NULL, v=NULL, col="grey")
          points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
   })
# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
  
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$info <- renderPrint({
    #It would be good to suppress the text when nothing near and suppress line number
    # Just shows year and country. Take out of expand the column index for more data listed on the hover
     nearPoints(CLUMData, input$plot_hover, xvar = input$xcol, yvar = input$ycol, addDist = FALSE)[c(1,3)]  
  })
  output$info2 <- renderPrint({
    #It would be good to suppress the text when nothing near and suppress line number
    nearPoints(CLUMData, input$plot_hover2, xvar = input$xcol, yvar = input$ycol, addDist = FALSE)[c(1,3)]  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

