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

#"C:/Users/Eli/GitFolders/EnergyEcoGroup/GFN_Data_Visualization/ScatterVisuals"
# Reading in the CLUM Data
#CLUMData <- read.csv("/Users/scottkaplan1112/Box Sync/Graduate School/A_DS421/Spring 2018 Project/EnergyEcoGroup_FinalProject/GFN_Data_Visualization/NFA_2017_CLUM.csv")
# Path for debugging outside of Shiny
# CLUMData <- read.csv("C:/Users/Eli/GitFolders/EnergyEcoGroup/GFN_Data_Visualization/ScatterVisuals/NFA_2017_CLUM.csv")
# CLUMQScore <- read.csv("C:/Users/Eli/GitFolders/EnergyEcoGroup/GFN_Data_Visualization/ScatterVisuals/CLUM_QScore.csv")
#CLUMData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_2017_CLUM.csv")

CLUMData <- read.csv("NFA_2017_CLUM.csv")
CLUMQScore <- read.csv("CLUM_QScore.csv")
CLUMData$QScore <- CLUMQScore$NFA_GTAP_Qscore[match(CLUMData$GTAP_name,CLUMQScore$GTAP.Only)]

cols <- c(names(CLUMData[,6:16]))
#Log transformed data
CLUMDatalog <- CLUMData
CLUMDatalog[cols] <- log(CLUMDatalog[cols])

#Friendly names
setnames(CLUMData, old = c(names(CLUMData[,6:16])), new = c("Coicop Expenditure", "Crop-Land", 
                                                            "Grazing-Land", "Forest-Land", "Fishing-Ground",
                                                            "BuiltUp-Land", "Carbon", "Total", "Z-Score", 
                                                            "Min-Max", "#NA"))

# Define UI for application that draws a Scatterplot
ui <- pageWithSidebar(
  headerPanel('Initial Visual Exploration of CLUM Data'),
  sidebarPanel(width = 3,
    #selection of GTAP years, 2011 as default
    checkboxGroupInput("Select_years", "Years", unique(CLUMData[,1]), selected = "2011", FALSE),
    selectInput('xcol', 'X Variable', names(CLUMData[,6:16]), selected = "Min-Max"),
    selectInput('ycol', 'Y Variable', names(CLUMData[,6:16]), selected = "Total"),
    selectInput('zcol', 'CLUM Category', unique(CLUMData[,5]),
                selected=names(CLUMData)[[2]]),
    selectInput('scale', 'Scale', c("normal","log"), selected="normal"),
    numericInput('clusters', 'Minimum Quality Score', 0,
                 min = 0, max = 6)
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
      verbatimTextOutput("info", placeholder = TRUE)),
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
      clum7_name==input$zcol & year %in% input$Select_years & GTAP_name %in% 
        #filter of countries with min QScore
        subset(CLUMQScore[1], CLUMQScore[,2] %in% seq(from=input$clusters,
                                                   to=max(CLUMQScore$NFA_GTAP_Qscore)))[,1]
      ) 
    CLUMData_subset[,c(input$xcol, input$ycol)]
  })

  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C",    "#984EA3",
              "#FF7F00", "#377EB8" , "#A65628","#E41A1C",  "#4DAF4A","#999999"))
    par(mar = c(5.1, 4.1, 0, 1))
    Qcolors <- seq(from=input$clusters,to=6) 
    plot(selectedData(),
                col = Qcolors, #c(0,1,2,3,4,5,6) ,
    pch = 20, cex = 3)
    abline(a=0, b=1, h=NULL, v=NULL, col="grey")

  })
    
   output$plot2 <- renderPlot({
     palette(c("#E41A1C",    "#984EA3",
               "#FF7F00", "#377EB8" , "#A65628","#E41A1C",  "#4DAF4A","#999999"))
     par(mar = c(5.1, 4.1, 0, 1))
     Qcolors <- seq(from=input$clusters,to=6)
     plot(selectedData(), xlim=c(input$plot_brush$xmin, input$plot_brush$xmax), 
          ylim=c(input$plot_brush$ymin, input$plot_brush$ymax),
          col = Qcolors,
          pch = 20, cex = 3)
          abline(a=0, b=1, h=NULL, v=NULL, col="grey")
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
     a <- nearPoints(if(input$scale == "normal") {CLUMData}
                else {CLUMDatalog}, 
                input$plot_hover, xvar = input$xcol, yvar = input$ycol, threshold = 3, maxpoint = 3, 
                addDist = FALSE)[c("year","GTAP_name","QScore")] 
    row.names(a) <- NULL
      if (nrow(a) > 0) {
        return(a)
      } else {
        cat("  year   GTAP_name  QScore")
      }
    })
  output$info2 <- renderPrint({
    #It would be good to suppress the text when nothing near and suppress line number
    a <- nearPoints(if(input$scale == "normal") {CLUMData}
                else {CLUMDatalog},
               input$plot_hover2, xvar = input$xcol, yvar = input$ycol, threshold = 3, maxpoint = 3, 
               addDist = FALSE)[c("year","GTAP_name","QScore")]
    if (nrow(a) > 0) {
      row.names(a) <- NULL
      return(a)
    } else {
      cat("  year   GTAP_name  QScore")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
