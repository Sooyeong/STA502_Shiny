# Load packages ----------------------------------------------------------
library(tidyverse)
library(shiny)
library(devtools)
library(ggradar)
library(lubridate)
library(plotly)
library(GGally)
library(ggrepel)
library(shinyjs)



#devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
# Load data ----------------------------------------------------------
# 
# load("Music data.Rdata")
# data_music <- data.frame(data_music)
# head(data_music)
# music_clean <- data_music %>%
#   select(genre, artist_name, album_name, release_date, track_name,
#          danceability:tempo, -key, -mode) %>%
#   mutate( #release_date = as.Date(release_date),
#          album_name = as.factor(album_name))
# save(music_clean, file="music_data_clean.Rdata")

load("/Users/coltongearhart/Documents/MU/STA 504/Projects/Project 2/music_data_clean.Rdata")
#load("/Users/coltongearhart/Documents/MU/STA 504/Projects/Project 2/Music data.Rdata")
#music_clean <- data.frame(data_music)

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

theme_set(theme_bw())

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
    selectInput(inputId="genre", label="Select genre",
                choices=choice, multiple=TRUE,
                selected="pop",
                selectize = TRUE),
    
    conditionalPanel(condition=("input.tabs != 'Compare Genres'" ),
                     uiOutput("Artist2")),
    
    conditionalPanel(condition="input.tabs == 'Compare Albums within Artist'",
                     uiOutput("artistAlbum"),
                     uiOutput("Artist3"),
                     uiOutput("artistAlbum2"),
                     selectInput(inputId="feature2", label="Select Feature",
                                 choices=choice_feature, multiple = TRUE,
                                 selected=c("tempo", "energy")),
                     selectize = TRUE),
    
    conditionalPanel(condition="input.tabs == 'Artists Over Time'",
                     selectInput(inputId="feature", label="Select Feature",
                                 choices=choice_feature,
                                 selected="tempo", multiple = TRUE,
                                 selectize = TRUE))
    # Main panel typically used to display outputs
  ),
  mainPanel(
    tabsetPanel(id="tabs",
                tabPanel("Compare Genres",plotOutput(outputId="radarplot")),
                tabPanel("Compare Albums within Artist" ,plotlyOutput(outputId="parallel")),
                tabPanel("Artists Over Time" ,plotOutput(outputId="timeseries"))
    )
  )
  
  ))

# Define server --------------------------

### Define server behavior for application here
server <- function(input, output) {
  
  output$Artist2 <- renderUI({
    selectInput(inputId="artist2", label="Select Artist",
                choices=sort(unique(music_clean$artist_name[music_clean$genre %in% input$genre])),
                selected=unique(music_clean$artist_name[music_clean$genre %in% input$genre])[1],
                selectize = TRUE)
  })
  
  output$artistAlbum <- renderUI({
    selectInput(inputId="album2",  label="Select Album",
                choices=sort(unique(music_clean$album_name[music_clean$artist_name==input$artist2])), 
                multiple=TRUE,
                selected=unique(music_clean$album_name[music_clean$artist_name==input$artist2])[1:2],
                selectize = TRUE)
  })
  
  output$Artist3 <- renderUI({
    selectInput(inputId="artist3",  label="Select Another Artist",
                choices=sort(unique(music_clean$artist_name[music_clean$artist_name!=input$artist2])),
                selected=unique(music_clean$artist_name[music_clean$genre %in% input$genre])[2],
                selectize = TRUE)
  })
  
  output$artistAlbum2 <- renderUI({
    selectInput(inputId="album3",  label="Select Album",
                choices=sort(unique(music_clean$album_name[music_clean$artist_name==input$artist3])), 
                multiple=TRUE,
                selected=unique(music_clean$album_name[music_clean$artist_name==input$artist3])[1:2],
                selectize = TRUE)
  })
  
  output$radarplot <- renderPlot({
    req(input$genre)
    avg_value <- avg_value[avg_value$genre %in% input$genre,]
    ggradar(avg_value)
  })
  
  # function to normalize data
  normalize <- function(x) {
    (x - min(x))/(diff(range(x)))
  }
  
  output$timeseries <- renderPlot({
    req(input$genre, input$artist2, input$feature)
    ggplot()
    data_artist <- music_clean %>%
      filter(artist_name %in% input$artist2) %>% 
      gather(key = "feature", value = "value", c("danceability", "energy",
                                                 "loudness", "speechiness",
                                                 "acousticness", "instrumentalness",
                                                 "liveness", "valence", "tempo")) %>% 
      filter(feature %in% input$feature) %>% 
      group_by(artist_name, album_name, feature) %>% 
      mutate(value = normalize(value)) %>% 
      ungroup

    p1 <- ggplot() +
      geom_boxplot(aes_string(x="release_date", y="value",
                              color="artist_name", group="release_date"),
                   data=data_artist) +
      facet_grid(. ~ feature) +
      coord_flip()
    p1
  })
  
  
  output$parallel <- renderPlotly({
    
    req(input$genre, input$artist2, input$album2, input$artist3, input$album3)
    
    data_album <- music_clean %>%
      filter(artist_name == input$artist2 | artist_name == input$artist3,
             album_name %in% input$album2 | album_name %in% input$album3)
    
    data_album_avg <- data_album %>%
      group_by(album_name, artist_name) %>%
      summarize_at(vars(danceability:tempo), mean)
    
    
    # ?plot_ly
    # p2<-plot_ly(data=data.frame(),
    #             type='parcoords', line=list(color=~input$feature2))
    
    p2 <-  ggparcoord(data=data_album_avg,
                      columns = input$feature2,
                      groupColumn = "artist_name",
                      scale = "uniminmax",
                      order = "allClass")
    # +geom_text(data=. %>% filter(variable=="tempo"),
    #                                                aes(x = variable, y = value, label = album_name),
    #                                                hjust=1.1)
    # 
    
    
    p2<-plotly_build(p2)
    mytext=paste("Artist Name: ", data_album_avg$artist_name, "\n", "Album Name: ", data_album_avg$album_name, sep="")
    
    style(p2, hoverinfo='text+y',text=mytext)
    
    # p2<-p2+add_annotations(hoverinfo='text',
    #                    text = ~paste('Album Name: ', album_name,
    #                                  '</br> Artist Name: ', artist_name))
    # 
    # p2
    # use plotly to add album name in mouse-over labels
    
  })
  
}

# Run app ---------------------------

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

