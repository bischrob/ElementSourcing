# This script creates a shiny app for showing biplots for obsidian sourcing.
# Created: 12.21.17
# Robert J. Bischoff

# #############################################################################################
# # Create a variable storing packages used, installs the package if missing, and loads packages.
# my.packages <- c("plotly", "shiny")
# usePackage <- function(p) 
# {
#   if (!is.element(p, installed.packages()[,1]))
#     install.packages(p, dep = TRUE)
#   require(p, character.only = TRUE)
# }
# lapply(my.packages, usePackage)

#############################################################################################
# User interface ----
ui <- fluidPage(
  titlePanel("Obsidian Sourcing Interactive Plots"), 
      sidebarLayout(
        sidebarPanel(
          radioButtons("source1", label = "Data Source",
                       choices = list("All Data" = 1,
                                      "Assigned" = 2,
                                      "Unassigned" = 3),
                       selected = 1, inline = F),
          selectInput("label1", label = h5("Legend Label"),
                      choices = list( "Original" = "Source",
                                      "Discriminant" = "Discriminant",
                                      "Mahalanobis" = "Mahalanobis"),
                      selected = 12),
          selectInput("elem1", label = h5("Element 1"), 
                      choices = list("Rb" = 7,
                                     "Sr" = 8,
                                     "Y" = 9,
                                     "Zr" = 10,
                                     "Nb" = 11), 
                      selected = 7),
               selectInput("elem2", label = h5("Element 2"), 
                           choices = list("Rb" = 7,
                                          "Sr" = 8,
                                          "Y" = 9,
                                          "Zr" = 10,
                                          "Nb" = 11), 
                           selected = 8)
      ),
        mainPanel(
          h3(textOutput("caption")),
          hr(),
          plotlyOutput("plotly1", height = "800px", width = "950px")
        )
    )
)

# Server logic ----
server <- function(input, output) {
  
  output$caption <- renderText("Biplot")
  
  output$plotly1 <- renderPlotly({
    
    # Assign correct data frame for radio button selected    
    if(input$source1 == 1) {plotDF <- AllData}
    if(input$source1 == 2) {plotDF <- rbind.data.frame(Assigned, AllData[sources,])}
    if(input$source1 == 3) {plotDF <- rbind.data.frame(Unassigned, AllData[sources,])}
    plotS <- plotDF[sources,] 
    colS <- plotS[,as.character(input$label1)]
    shapeS <- plotS$Type
    plotA <- plotDF[artifacts,] 
    colA <- plotA[,as.character(input$label1)]
    shapeA <- plotA$Type
        
    g <- ggplot() +
        geom_point(data = plotS,
                   aes(x = plotS[,as.numeric(input$elem1)],
                       y = plotS[,as.numeric(input$elem2)],
                       color = colS,
                       shape = shapeS,
                       text = plotS[,1])) +
        geom_point(data = plotA,
                   aes(x = plotA[,as.numeric(input$elem1)],
                   y = plotA[,as.numeric(input$elem2)],
                   color = colA,
                   shape = shapeA,
                   text = plotA[,1])) +
        xlab(names(plotDF)[as.numeric(input$elem1)]) +
        ylab(names(plotDF)[as.numeric(input$elem2)]) +
        theme_minimal() +
        theme(legend.title=element_blank()) +
        stat_ellipse(data = plotS,
                     aes(x = plotS[,as.numeric(input$elem1)],
                         y = plotS[,as.numeric(input$elem2)],
                         color = colS),
                     type = "norm",
                     level = .9,
                     lwd = .75) # this ellipse is based off the multivariate normal distribution
      ggplotly(g)
  })
}

# Run app ----
shinyApp(ui, server)
