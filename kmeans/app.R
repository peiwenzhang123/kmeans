#Load libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)
library(shiny)

# Load data
obama <- read_csv("obama.csv")
obama_kmeans <- obama %>%
  select(track_name, energy, valence)

trudeau <- read_csv("trudeau.csv")
trudeau_kmeans <- trudeau %>%
  select(track_name, energy, valence)

### UI Side ####

ui <- fluidPage(
  
  titlePanel("K-Means Clustering for Obama's and Trudeau's Songs: How Many Clusters to Create?"),
  
  sidebarLayout(
    
    sidebarPanel(
      helpText("Data from Spotify"),
      
      # Allow user to choose k
      sliderInput(inputId = "kval", 
                  label = "Choose a value for k, the number of clusters: ",
                  min = 2, max = 8, value = 3)),
    
    p("This shiny app is made for the final project of Stat041 - complete code and datasets can be found", 
      shiny::a("here", href = "https://github.com/peiwenzhang123/kmeans/tree/master/kmeans"),
    
    # Output graph
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Barack Obama", plotOutput("ograph")),
        tabPanel("Justin Trudeau", plotOutput("tgraph"))
    )  
   )
  )
 )
)

### Server Side ###
server <- function(input, output){
  
  set.seed(20)
  obama_cluster <- reactive({
    as.factor(kmeans(obama_kmeans[, 2:3], input$kval, nstart = 20)$cluster)
  })
  trudeau_cluster <- reactive({
    as.factor(kmeans(trudeau_kmeans[, 2:3], input$kval, nstart = 20)$cluster)
  })
  
  # Create the graphs
  output$ograph <- renderPlot({
    ggplot(obama_kmeans, aes(energy, valence, color = obama_cluster())) +
      labs(title = "K-Means Clustering", x = "Energy", y = "Valence", col = "Cluster") +
      geom_point(size = 5) +
      theme_light()
  })
  
  output$tgraph <- renderPlot({
    ggplot(trudeau_kmeans, aes(energy, valence, color = trudeau_cluster())) +
      labs(title = "K-Means Clustering", x = "Energy", y = "Valence", col = "Cluster") +
      geom_point(size = 5) +
      theme_light()
  })
}

shinyApp(ui=ui, server=server)

