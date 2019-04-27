library(dplyr)
library(ggplot2)
library(devtools)
library(shiny)
library(devtools)
library(ggradar)
library(lubridate)
library(plotly)


## Load Data
load(file="C:\\Users\\Sooyeong\\Desktop\\STA504_Shiny-master\\STA504_Shiny-master\\Music data.Rdata")

## Define average values
## The data may vary based on purpose


## Give label to genre
choice<-unique(data_music$genre)
choice
choice_artist<-unique(data_music$artist_name)

names(data_music)
choice_feature<-c("danceability","energy","loudness","speechiness","acousticness","instrumentalness","tempo")
unique(data_music$artist_name)
### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Sportify Explorer"),
  sidebarLayout(
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
                                 selected="tempo"))
                     
    # Main panel typically used to display outputs
    ),
  mainPanel(
    tabsetPanel(id="tabs",
                tabPanel("radarplot",plotlyOutput(outputId="radarplot")),
                tabPanel("timeseries" ,plotlyOutput(outputId="timeseries"))
    )
  )

)

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
  
  
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

