#' This script creates a shiny app for showing triplots for obsidian sourcing.
#' Must run main script first
#' Created: 12.21.17
#' Robert J. Bischoff
#' Dependent pacakges: shiny, plotly, ggplot2

###############################################################################
options(warn = -1)
suppressMessages(library(shiny))
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
options(warn = 0)

# Create variables for subsetting
assigned <- which(df$Status == "assigned")
unAssigned <- which(df$Status == "unassigned")
artifacts <- which(df$Type == "Artifact")
sources <- c(which(df$Type == "Source"),which(df$Type == "Source Flake"))

#############################################################################################
# User interface ----
uitri <- fluidPage(
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
                           selected = 8),
          selectInput("elem3", label = h5("Element 3"), 
                      choices = list("Rb" = 7,
                                     "Sr" = 8,
                                     "Y" = 9,
                                     "Zr" = 10,
                                     "Nb" = 11), 
                      selected = 9)
      ),
        mainPanel(
          h3(textOutput("caption")),
          hr(),
          plotlyOutput("plotly1", height = "800px", width = "950px")
        )
    )
)

# Server logic ----
servertri <- function(input, output) {
  
  output$caption <- renderText("Triplot/Ternary")
  
  output$plotly1 <- renderPlotly({
    
    # Assign correct data frame for radio button selected    
    if(input$source1 == 1) {plotDF <- df}
    if(input$source1 == 2) {plotDF <- rbind.data.frame(df[assigned,],df[sources,])}
    if(input$source1 == 3) {plotDF <- rbind.data.frame(df[unAssigned,],df[sources,])}

    plot_ly(data = plotDF,
                 a = plotDF[,as.numeric(input$elem1)],
                 b = plotDF[,as.numeric(input$elem2)],
                 c = plotDF[,as.numeric(input$elem3)],
                 color = ~get(input$label1),
                 
                 type = 'scatterternary',
                 symbol = ~Type,
                 mode = 'markers',
                 text = ~paste("ANID: ",
                               ANID, '<br>Source:',
                               Source, '<br>Closest Source:',
                               Mahalanobis)) %>%
      layout(ternary = list(aaxis = list(title = names(plotDF)[as.numeric(input$elem1)]),
                            baxis = list(title = names(plotDF)[as.numeric(input$elem2)]),
                            caxis = list(title = names(plotDF)[as.numeric(input$elem3)]),
                            margin = list(t = 500, l = 500, r = 500, bottom = 500, pad = 500)))
  })
}

# Run app ----
# shinyApp(uitri, servertri)
