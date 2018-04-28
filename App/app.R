#Load libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)
library(shiny)

# Load data
obama <- read_csv("~/Desktop/Projects/Stat041_p4_grp3/App/obama.csv")
obama_kmeans <- obama %>%
  select(track_name, energy, valence)

trudeau <- read_csv("~/Desktop/Projects/Stat041_p4_grp3/App/trudeau.csv")
trudeau_kmeans <- trudeau %>%
  select(track_name, energy, valence)

### UI Side ####

# Use fluid page layout (rows and columns)
ui <- fluidPage(
  
  titlePanel("How many clusters to create?"),
  
  sidebarLayout(
    
    sidebarPanel(
      helpText("Spotify Data"),
      
      sliderInput(inputId = "kval", 
                  label = "Choose a value for k: ",
                  min = 2, max = 8, value = 5)),
    
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
  
  set.seed(20)
  obama_cluster <- reactive({
    as.factor(kmeans(obama_kmeans[, 2:3], input$kval, nstart = 20)$cluster)
  })
  trudeau_cluster <- reactive({
    as.factor(kmeans(trudeau_kmeans[, 2:3], input$kval, nstart = 20)$cluster)
  })
  
  # Create the graph
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

