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
library(scales)



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

load("music_data_clean.Rdata")
#load("/Users/coltongearhart/Documents/MU/STA 504/Projects/Project 2/Music data.Rdata")
#music_clean <- data.frame(data_music)

# t<-music_clean %>% group_by(album_name, artist_name, release_date) %>% 
#   filter(row_number()==1) %>% 
#   mutate(temp = )
# 
# music_clean %>% 
#   mutate(ReleaseDate = as.Date(release_date)) %>% 
#   group_by(artist_name,album_name) %>% 
#   arrange(release_date) %>% 
#   summarize(album_order = nth())
# 
# View(head(t,10))

# data preparation for radar plots
# nothing interactive so can be done outside
# function to normalize data
normalize <- function(x) {
  (x - min(x))/(diff(range(x)))
}
# 
# avg_value <- music_clean %>% 
#   group_by(genre) %>% 
#   mutate_at(vars(danceability:tempo), normalize) %>% 
#   select(genre, danceability:tempo)

avg_value <- music_clean %>%
  group_by(genre) %>%
  mutate_at(vars(danceability:tempo), rescale) %>% 
  summarise_at(vars(danceability:tempo), mean)

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
  sidebarLayout(sidebarPanel(width=3,
    # Sidebar typically used to house input controls
    conditionalPanel(condition=("input.tabs == 'Compare Genres'" ),
                     h4("Welcome to the Spotify Explorer App"),
                     p("This app allows users to explore the audio features
                       (danceability, energy, loudness, speechiness, acousticness, instrumentalness, tempo)
                       of all tracks by the top 25 most popular artists in each of 23 genres."),
                     p("The first tab explores differences in audio features of popular music today by genre.
                       The second tab takes a more detailed look at these features for artists within genres
                       of interest and their respective albums.
                       Tab 3 allows the user to explore the trends in audio features for artists over time.")),
    
    selectInput(inputId="genre", label="Select genre",
                choices=choice, multiple=TRUE,
                selected="pop",
                selectize = TRUE),
    
    conditionalPanel(condition=("input.tabs != 'Compare Genres'" ),
                     uiOutput("Artist2"),
                     uiOutput("Artist3")),
    
    conditionalPanel(condition="input.tabs == 'Compare Albums within Artist'",
                     uiOutput("artistAlbum"),
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
  mainPanel(width=8,
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
                choices=sort(unique(music_clean$artist_name[music_clean$artist_name!=input$artist2 &
                                                              music_clean$genre %in% input$genre])),
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
    ggradar(avg_value, background.circle.colour = "white")
  }, height = 600, width = 800)
  
  
  output$parallel <- renderPlotly({
    
    req(input$genre, input$artist2, input$album2, input$artist3, input$album3)
    
    data_album <- music_clean %>%
      filter(artist_name == input$artist2 | artist_name == input$artist3,
             album_name %in% input$album2 | album_name %in% input$album3)
    
    data_album_avg <- data_album %>%
      group_by(album_name, artist_name) %>%
      summarize_at(vars(danceability:tempo), mean)
    
    # data_uniminmax <- data_album_avg %>% 
    #   mutate(Danceability=(danceability-min(danceability,na.rm = T))/
    #            (max(danceability,na.rm = T)-min(danceability,na.rm = T)),
    #          Energy=(energy-min(energy,na.rm = T))/
    #            (max(energy,na.rm = T)-min(energy,na.rm = T)),
    #          Loudness = (loudness-min(loudness,na.rm = T))/
    #            (max(loudness,na.rm = T)-min(loudness,na.rm = T)),
    #          Speechiness=(speechiness-min(speechiness,na.rm = T))/
    #            (max(speechiness,na.rm = T)-min(speechiness,na.rm = T)),
    #          Acousticness=(acousticness-min(acousticness,na.rm = T))/
    #            (max(acousticness,na.rm = T)-min(acousticness,na.rm = T)),
    #          Instrumentalness=(instrumentalness-min(instrumentalness,na.rm = T))/
    #            (max(instrumentalness,na.rm = T)-min(instrumentalness,na.rm = T)),
    #          Liveness=(liveness-min(liveness,na.rm = T))/
    #            (max(liveness,na.rm = T)-min(liveness,na.rm = T)),
    #          Tempo=(tempo-min(tempo,na.rm = T))/
    #            (max(tempo,na.rm = T)-min(tempo,na.rm = T)),
    #          Valence=(valence-min(valence,na.rm = T))/
    #            (max(valence,na.rm = T)-min(valence,na.rm = T))) %>% 
    #   gather(Danceability:Valence, key="feature", value="value") %>% 
    #   filter(feature %in% input$feature)
    # 
    # 
    # p2 <- ggplot(data = data_uniminmax) +
    #   geom_line(aes(x = feature, y = value,
    #                 group = artist_name, color=artist_name),
    #                 alpha=.5,size=1)+
    #   # geom_vline(aes(xintercept = which(levels(survey) %in% 'pre')))+
    #   # geom_vline(aes(xintercept = which(levels(survey) %in% 'post')))+
    #   theme_bw() 
    #   # scale_color_manual(values=c("red", "blue"))+
    #   # theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    #   #       panel.grid.minor = element_blank())
    # 
    # ggplotly(p2)
    
    # ?plot_ly
    # p2<-plot_ly(data=data.frame(),
    #             type='parcoords', line=list(color=~input$feature2))
    
    # yes
    p2 <- ggparcoord(data=data_album_avg,
                      columns = input$feature2,
                      groupColumn = "artist_name",
                      scale = "uniminmax",
                      order = "allClass")
  
    # +geom_text(data=. %>% filter(variable=="tempo"),
    #                                                aes(x = variable, y = value, label = album_name),
    #                                                hjust=1.1)
    # 
    
    # p2 <- ggplotly(p2, height = 600, width = 900)
    
    # yes
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
  
  output$timeseries <- renderPlot({
    req(input$genre, input$artist2, input$feature)
    
    data_artist <- music_clean %>%
      mutate(axisLabel = paste0(release_date, "\n", album_name),
             axisLabel = factor(axisLabel, ordered=T),
             axisLabel = factor(axisLabel, levels=rev(levels(axisLabel)))) %>% 
      filter(artist_name %in% c(input$artist2, input$artist3)) %>% 
      gather(key = "feature", value = "value", c("danceability", "energy",
                                                 "loudness", "speechiness",
                                                 "acousticness", "instrumentalness",
                                                 "liveness", "valence", "tempo")) %>% 
      filter(feature %in% input$feature) %>% 
      group_by(artist_name, axisLabel, feature) %>% 
      mutate(value = normalize(value)) %>% 
      ungroup()

    
    p1 <- ggplot(data=data_artist) +
      geom_boxplot(aes_string(x="axisLabel", y="value",
                              color="artist_name", group="axisLabel")) +
      facet_grid(artist_name ~ feature, scales="free_y") +
      coord_flip() +
      labs(x="Album Release Date") +
      theme(axis.title.x=element_blank())
    p1
  }, height = 800, width = 1000)
  
}

# Run app ---------------------------

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

