# Load packages ----------------------------------------------------------

  library(dplyr)
  library(ggplot2)
  library(devtools)
  library(shiny)
  library(devtools)
  library(ggradar)
  library(lubridate)
  library(plotly)

  #devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
# Load data ----------------------------------------------------------

  load("/Users/coltongearhart/Documents/MU/STA 504/Projects/Project 2/Music data.Rdata")
  data_music<-data.frame(data_music)
  
# Define UI -----------------------------

  ## Give label to genre
  choice<-unique(data_music$genre)
  choice
  choice_artist<-unique(data_music$artist_name)
  
  ## Give label to albums
  choice_album<-unique(data_music$album_name)
  
  names(data_music)
  choice_feature<-c("danceability","energy","loudness","speechiness","acousticness","instrumentalness","tempo")
  unique(data_music$artist_name)
  ### Define UI for application
  ui <- fluidPage(
    # Application title
    titlePanel(title = "Sportify Explorer"),
    sidebarLayout(sidebarPanel(
      # Sidebar typically used to house input controls
      conditionalPanel(condition="input.tabs == 'radarplot'",
                       selectInput(inputId="genre", label="Select genre",
                                   choices=choice, multiple=TRUE,
                                   selected="pop")),
      
      conditionalPanel(condition="input.tabs == 'timeseries'",
                       selectInput(inputId="artist", label="Select Artist",
                                   choices=choice_artist, multiple=TRUE,
                                   selected="The Killers"),
                       selectInput(inputId="feature", label="Select Feature",
                                   choices=choice_feature,
                                   selected="tempo")),
      
      conditionalPanel(condition="input.tabs == 'parallel'",
                       selectInput(inputId="artist2", label="Select Artist",
                                   choices=choice_artist, multiple=TRUE,
                                   selected="The Killers"),
                       selectInput(inputId="album2", label="Select Album",
                                   choices=choice_album, multiple=TRUE,
                                   selected=c("Wonderful Wonderful", "Battle Born")),
                       selectInput(inputId="feature2", label="Select Feature",
                                   choices=choice_feature, multiple = TRUE,
                                   selected=c("tempo", "energy")))
    # Main panel typically used to display outputs
    ),
    mainPanel(
      tabsetPanel(id="tabs",
                  tabPanel("radarplot",plotlyOutput(outputId="radarplot")),
                  tabPanel("timeseries" ,plotlyOutput(outputId="timeseries")),
                  tabPanel("parallel" ,plotOutput(outputId="parallel"))
      )
    )
    
  ))
  
# Define server --------------------------
  
  ### Define server behavior for application here
  server <- function(input, output) {
    
    output$radarplot <- renderPlotly({
      ## Sample case ... the data can be replaced
      
      avg_value<-data_music %>% group_by(genre) %>% summarise(avg_dance=mean(danceability), avg_enery=mean(energy), avg_speechiness=mean(speechiness),
                                                              avg_acousticness=mean(acousticness), avg_instrumentalness=mean(instrumentalness),
                                                              avg_liveness=mean(liveness),avg_valance=mean(valence))
      data.frame(avg_value)
      rownames(avg_value)<-unique(avg_value$genre)
      avg_value<-avg_value[input$genre,]
      ggradar(avg_value[,2:8])
      
      
    })
    
    
    output$timeseries <- renderPlotly({
      
      data_artist<-data_music %>% filter(artist_name %in% input$artist)
      
      p1 <- ggplot() + 
        geom_boxplot(aes_string(x="release_date", y=input$feature, color="artist_name"),
                     data=data_artist)+coord_flip()
      p1
    })
    
    
    output$parallel <- renderPlot({
      
      data_album<-data_music %>% 
        filter(artist_name == input$artist2, album_name %in% input$album2) %>% 
        mutate(album_name = as.factor(album_name))
      
      p2 <- ggparcoord(data = data_album, columns = input$feature2, groupColumn = "album_name", scale = "uniminmax", order = "allClass")  
      p2
    })
    
  }
  
# Run app ---------------------------
  
  ### specify the ui and server objects to be combined to make App
  shinyApp(ui=ui, server=server)
  
