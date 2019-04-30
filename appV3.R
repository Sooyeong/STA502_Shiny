# Load packages ----------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(devtools)
library(ggradar)
library(lubridate)
library(plotly)
library(GGally)

#devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
# Load data ----------------------------------------------------------
# 
# load("Music data.Rdata")
# data_music <- data.frame(data_music)
# head(data_music)
# music_clean <- data_music %>%
#   select(genre, artist_name, album_name, release_date, track_name,
#          danceability:tempo, -key, -mode) %>%
#   mutate(release_date = as.Date(release_date),
#          album_name = as.factor(album_name))
# save(music_clean, file="music_data_clean.Rdata")

load("music_data_clean.Rdata")
head(music_clean) 

# data preparation for radar plots
# nothing interactive so can be done outside
avg_value <- music_clean %>% 
  group_by(genre) %>% 
  summarise(Danceability=mean(danceability), 
            Energy=mean(energy), 
            Speechiness=mean(speechiness),
            Acousticness=mean(acousticness), 
            Instrumentalness=mean(instrumentalness),
            Liveness=mean(liveness),
            Valance=mean(valence))

# Define UI -----------------------------

## Give label to genre
choice<-unique(music_clean$genre)
choice
choice_artist<-sort(unique(music_clean$artist_name))

## Give label to albums
choice_album<-unique(music_clean$album_name)

names(music_clean)
choice_feature<-c("danceability","energy","loudness","speechiness","acousticness","instrumentalness","tempo")
unique(music_clean$artist_name)
### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Spotify Explorer"),
  sidebarLayout(sidebarPanel(
    # Sidebar typically used to house input controls
    conditionalPanel(condition="input.tabs == 'radarplot'",
                     selectInput(inputId="genre", label="Select genre",
                                 choices=choice, multiple=TRUE,
                                 selected="pop",
                                 selectize = TRUE)),
    
    conditionalPanel(condition="input.tabs == 'timeseries'",
                     selectInput(inputId="artist", label="Select Artist",
                                 choices=choice_artist, multiple=TRUE,
                                 selected="The Killers",
                                 selectize = TRUE),
                     selectInput(inputId="feature", label="Select Feature",
                                 choices=choice_feature,
                                 selected="tempo",
                                 selectize = TRUE)),
    
    conditionalPanel(condition="input.tabs == 'parallel'",
                     selectInput(inputId="artist2", label="Select Artist",
                                 choices=choice_artist,
                                 selected="The Killers",
                                 selectize = TRUE),
                     uiOutput("artistAlbum"),
                     uiOutput("Artist3"),
                     uiOutput("artistAlbum2"),
                     selectInput(inputId="feature2", label="Select Feature",
                                 choices=choice_feature, multiple = TRUE,
                                 selected=c("tempo", "energy")),
                     selectize = TRUE)
    # Main panel typically used to display outputs
  ),
  mainPanel(
    tabsetPanel(id="tabs",
                tabPanel("radarplot",plotOutput(outputId="radarplot")),
                tabPanel("timeseries" ,plotOutput(outputId="timeseries")),
                tabPanel("parallel" ,plotOutput(outputId="parallel"))
    )
  )
  
  ))

# Define server --------------------------

### Define server behavior for application here
server <- function(input, output) {
  
  output$artistAlbum <- renderUI({
    selectInput(inputId="album2",  label="Select Album",
                choices=sort(unique(music_clean$album_name[music_clean$artist_name==input$artist2])), 
                multiple=TRUE,
                selected=c("Wonderful Wonderful", "Battle Born"),
                selectize = TRUE)
  })
  
  output$Artist3 <- renderUI({
    selectInput(inputId="artist3",  label="Select Another Artist",
                choices=sort(unique(music_clean$artist_name[music_clean$artist_name!=input$artist2])),
                selected=c("Taylor Swift"),
                selectize = TRUE)
  })
  
  output$artistAlbum2 <- renderUI({
    selectInput(inputId="album3",  label="Select Album",
                choices=sort(unique(music_clean$album_name[music_clean$artist_name==input$artist3])), 
                multiple=TRUE,
                selected=c("Red", "1989"),
                selectize = TRUE)
  })
  
  output$radarplot <- renderPlot({
    avg_value <- avg_value[avg_value$genre %in% input$genre,]
    ggradar(avg_value)
  })
  
  output$timeseries <- renderPlot({
    
    ggplot()
    # data_artist <- music_clean %>% 
    #   filter(artist_name %in% input$artist)
    # 
    # p1 <- ggplot() + 
    #   geom_line(aes_string(x="release_date", y=input$feature, 
    #                        color="artist_name"), group=1,
    #                data=data_artist)
    # p1
  })
  
  
  output$parallel <- renderPlot({
    
    data_album <- music_clean %>% 
      filter(artist_name == input$artist2 | artist_name == input$artist3, 
             album_name %in% input$album2 | album_name %in% input$album3)
    
    data_album_avg <- data_album %>% 
      group_by(album_name, artist_name) %>% 
      summarize_at(vars(danceability:tempo), mean)
    
    p2 <- ggparcoord(data = data_album_avg, 
                     columns = input$feature2, 
                     groupColumn = "artist_name", 
                     scale = "uniminmax", 
                     order = "allClass")  
    p2
    # use plotly to add album name in mouse-over labels
  })
  
}

# Run app ---------------------------

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)