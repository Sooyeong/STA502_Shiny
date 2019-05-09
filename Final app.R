# READ ME --------------------

  # Brief escription of app
  # This app was built to explore and find trends within the features of some of the most popular mnusic
  # (first tab) A radar plot compares all features available for desired genres
  # (second tab) Parallel coordinate plots compare features for different artists across their albums
  # (third tab) Box plots for selected features compare trends within a single artist over time

  # Data description
  # Data was obtained by scraping Spotifies database through their API
  # We collected song level data for each song on every album from the top 25 artists within each genre (23 genres in total)
  # Variables included genre, artist name, artist popularity, album name, album release date,
  # -> track names and scores for audio features of each track, which are somehow computed by Spotify

# Load packages ----------------------------------------------------------
library(tidyverse)
library(shiny)
library(ggradar)
library(plotly)
library(GGally)
library(scales)

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

# function to normalize data
normalize <- function(x) {
  (x - min(x))/(diff(range(x)))
}

# data preparation for radar plots
avg_value <- music_clean %>%
  group_by(genre) %>%
  mutate_at(vars(danceability:tempo), rescale) %>% 
  summarise_at(vars(danceability:tempo), mean)

theme_set(theme_bw())

# Define UI -----------------------------

## Give label to genre
choice <- unique(music_clean$genre)

# Give label to audio features
choice_feature <- c("danceability","energy","loudness","speechiness",
                    "acousticness","instrumentalness","tempo")

### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Exploring Audio Features of Most Popular Artist on Spotify"),
  sidebarLayout(sidebarPanel(width=3,
                             # Sidebar typically used to house input controls
                             # Background information on first tab
                             conditionalPanel(condition=("input.tabs == 'Compare Genres'" ),
                                              p("This app allows users to explore the audio features
                                                (danceability, energy, loudness, speechiness, acousticness, instrumentalness, tempo)
                                                of all tracks by the top 25 most popular artists in each of 23 genres."),
                                              p("The first tab explores differences in audio features of popular music today by genre.
                                                The second tab takes a more detailed look at these features for artists within genres
                                                of interest and their respective albums.
                                                Tab 3 allows the user to explore the trends in audio features for artists over time.")),
                             
                             # drop down for genre selection
                             selectInput(inputId="genre", label="Select genre",
                                         choices=choice, multiple=TRUE,
                                         selected="pop",
                                         selectize = TRUE),
                             
                             # drop downs for artist selection
                             conditionalPanel(condition=("input.tabs != 'Compare Genres'" ),
                                              uiOutput("Artist2"),
                                              uiOutput("Artist3")),
                             
                             # drop downs to select albums and audio features
                             conditionalPanel(condition="input.tabs == 'Compare Albums within Artist'",
                                              uiOutput("artistAlbum"),
                                              uiOutput("artistAlbum2"),
                                              selectInput(inputId="feature2", label="Select Feature",
                                                          choices=choice_feature, multiple = TRUE,
                                                          selected=c("tempo", "energy")),
                                              selectize = TRUE),
                             
                             # drop downs to select audio feature(s) for boxplots
                             conditionalPanel(condition="input.tabs == 'Artists Over Time'",
                                              selectInput(inputId="feature", label="Select Feature",
                                                          choices=choice_feature,
                                                          selected="tempo", multiple = TRUE,
                                                          selectize = TRUE))
                                              ),
                # Main panel typically used to display outputs
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
  
  # reactive part for artist selection drop down
  output$Artist2 <- renderUI({
    selectInput(inputId="artist2", label="Select Artist",
                # filter to only show artists in selected genre
                choices=sort(unique(music_clean$artist_name[music_clean$genre %in% input$genre])),
                # default to first artist within selected genre
                selected=unique(music_clean$artist_name[music_clean$genre %in% input$genre])[1],
                selectize = TRUE)
  })
  
  # reactive part for album selection drop down
  output$artistAlbum <- renderUI({
    selectInput(inputId="album2",  label=paste0("Select Album(s) by ", input$artist2),
                # filter to only show albums belonging to selected artist
                choices=sort(unique(music_clean$album_name[music_clean$artist_name==input$artist2])), 
                multiple=TRUE,
                # default to first 2 albums belonging to selected artist
                selected=unique(music_clean$album_name[music_clean$artist_name==input$artist2])[1:2],
                selectize = TRUE)
  })
  
  # reactive part for second artist selection drop down
  output$Artist3 <- renderUI({
    selectInput(inputId="artist3",  label="Select Another Artist",
                # filter to only show artists in selected genre and don't show artist already selected
                choices=sort(unique(music_clean$artist_name[music_clean$artist_name!=input$artist2 &
                                                              music_clean$genre %in% input$genre])),
                # default to first artist within selected genre
                selected=unique(music_clean$artist_name[music_clean$genre %in% input$genre])[2],
                selectize = TRUE)
  })
  
  # reactive part for album selection drop down
  output$artistAlbum2 <- renderUI({
    selectInput(inputId="album3",  label=paste0("Select Album(s) by ", input$artist3),
                # filter to only show albums belonging to selected artist
                choices=sort(unique(music_clean$album_name[music_clean$artist_name==input$artist3])), 
                multiple=TRUE,
                # default to first 2 albums belonging to selected artist
                selected=unique(music_clean$album_name[music_clean$artist_name==input$artist3])[1:2],
                selectize = TRUE)
  })
  
  # construct radar plot
  output$radarplot <- renderPlot({
    req(input$genre)      # show blank if nothing selected
    # filter for selected genre
    avg_value <- avg_value[avg_value$genre %in% input$genre,]
    ggradar(avg_value, background.circle.colour = "white")
  }, height = 600, width = 800)
  
  # construct parallel coordinates plot
  output$parallel <- renderPlotly({
    # show blank if nothing selected
    req(input$genre, input$artist2, input$album2, input$artist3, input$album3)
    
    # filter for user-selected artists and albums and
    # calculate mean for each feature for each album
    data_album_avg <- music_clean %>%
      filter(artist_name == input$artist2 | artist_name == input$artist3,
             album_name %in% input$album2 | album_name %in% input$album3) %>%
      group_by(album_name, artist_name) %>%
      summarize_at(vars(danceability:tempo), mean)
    
    # construct parcoord plot but don't display it
    p2 <- ggparcoord(data=data_album_avg,
                     columns = input$feature2,
                     groupColumn = "artist_name",
                     scale = "uniminmax",
                     order = "skewness")   # order=skewness prevents errors when 
    # changing user inputs
    # extract dataset from ggparcoord object
    pardata <- p2$data
    
    # use ggparcoord data to construct new data.frame to be used with ggplot
    df <- data.frame(x=pardata$variable, y=pardata$value,
                     album=data_album_avg$album_name,
                     artist=data_album_avg$artist_name)
    
    # construct parcoord plot using ggplot and data from ggparcoord
    ggplotly(ggplot(aes(color=artist), data=df) +
               geom_line(aes(x=x, y=y, group=album), size=1.5) +
               theme_minimal()+
               theme(axis.title=element_blank()))
    
  })
  
  # construct boxplots
  output$timeseries <- renderPlot({
    req(input$genre, input$artist2, input$feature)   # show blank if nothing selected
    
    data_artist <- music_clean %>%
      # create axis label that shows release date and album name
      mutate(axisLabel = paste0(release_date, "\n", album_name), 
             axisLabel = factor(axisLabel, ordered=T),
             axisLabel = factor(axisLabel, levels=rev(levels(axisLabel)))) %>% 
      # filter for selected artists
      filter(artist_name %in% c(input$artist2, input$artist3)) %>% 
      gather(key = "feature", value = "value", c("danceability", "energy",
                                                 "loudness", "speechiness",
                                                 "acousticness", "instrumentalness",
                                                 "liveness", "valence", "tempo")) %>% 
      filter(feature %in% input$feature) %>%            # filter for selected feature(s)
      mutate(feature = str_to_title(feature)) %>%       # capitalize features to look nicer on plot
      group_by(artist_name, axisLabel, feature) %>% 
      mutate(value = normalize(value)) %>%              # normalize values for plotting
      ungroup()
    
    p1 <- ggplot(data=data_artist) +
      geom_boxplot(aes_string(x="axisLabel", y="value",
                              color="artist_name", group="axisLabel")) +
      facet_grid(artist_name ~ feature, scales="free_y") +
      coord_flip() +
      labs(x="Album Release Date & Title", color="Artist") +
      theme(axis.title.x=element_blank())
    p1
  }, height = 800, width = 1000)
  
}

# Run app ---------------------------

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)
