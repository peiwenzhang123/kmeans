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
                  min = 2, max = 5, value = 3),
    
      p("This shiny app is made for the final project of Stat041. Complete code and datasets can be found", 
      shiny::a("here", href = "https://github.com/peiwenzhang123/kmeans/tree/master/kmeans"), ".")
    ),
    
    # Output graph
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Barack Obama", plotOutput("ograph")),
        tabPanel("Justin Trudeau", plotOutput("tgraph"))
    )  
   )
  )
 )

### Server Side ###
server <- function(input, output){
    
  centers <- reactive({
    
    if(input$kval == 2){
      centers <- matrix(c(rep(0.5, 2), 0.4, 0.8), nrow = 2, ncol = 2) 
    }
    
    if(input$kval == 3){
      centers <- matrix(c(0.4, rep(0.7, 2), rep(0.4, 2), 0.7), nrow = 3, ncol = 2) 
    }
    
    if(input$kval == 4){
      centers <- matrix(c(0.5, rep(0.7, 3), rep(0.4, 2), 0.6, 0.9), nrow = 4, ncol = 2) 
    }
    
    if(input$kval == 5){
      centers <- matrix(c(0.4, 0.5, 0.6, rep(0.8, 2), rep(0.4, 2), rep(0.6, 2), 0.8), nrow = 5, ncol = 2) 
    }
    
    output = centers
  })
  
  set.seed(20)
  obama_cluster <- reactive({
    as.factor(kmeans(obama_kmeans[, 2:3], centers = centers(), nstart = 20)$cluster)
  })
  
  set.seed(20)
  trudeau_cluster <- reactive({
    as.factor(kmeans(trudeau_kmeans[, 2:3], centers = centers(), nstart = 20)$cluster)
  })
  
  # Create the graphs
  output$ograph <- renderPlot({
    ggplot(obama_kmeans, aes(energy, valence, color = obama_cluster())) +
      labs(title = "K-Means Clustering", x = "Energy", y = "Valence", col = "Cluster") +
      #scale_color_manual(values = pal) +
      geom_point(size = 5) +
      theme_light() +
      theme(legend.position="none")
  })
  
  output$tgraph <- renderPlot({
    ggplot(trudeau_kmeans, aes(energy, valence, color = trudeau_cluster())) +
      labs(title = "K-Means Clustering", x = "Energy", y = "Valence", col = "Cluster") +
      #scale_color_manual(values = pal) +
      geom_point(size = 5) +
      theme_light() +
      theme(legend.position="none")
  })
}

shinyApp(ui=ui, server=server)

