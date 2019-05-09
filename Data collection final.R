# Load packages ----------------------------------------------------------

  # help pages for spotifyr
  # -> https://www.rcharlie.com/spotifyr/
  # -> https://cran.r-project.org/web/packages/spotifyr/spotifyr.pdf
  library("tidyverse")
  library("magrittr")
  library("spotifyr")

# Comments -----------------------------------

  # getting data on the top 50 artists from each genre
  # -> ranked based on popularity score (scale 0 - 100, with 100 being the most popular)
  # --> calculated from the popularity of all the artist’s tracks

# Load required items -----------------------------------------------------------
  
  # set authorization info (used for spotifyr functions)
  Sys.setenv(SPOTIFY_CLIENT_ID = "13fdf02e718d478db11cd69c8b03029b")
  Sys.setenv(SPOTIFY_CLIENT_SECRET = "07943a97cf16475cbc6d2a8df6691ecc")

# Functions ---------------------------------------------------
  
  # function to get top artists within each genre
  get_artists <- function(gens) {
    
    # initialize dataframe of artists for each genre
    data_artist <- data.frame(genre = NA, artist_name = NA, popularity = NA)
    data_artist <- data_artist[0, ]
    
    # get top artists for each genre
    for (i in seq_along(gens)) {
      
      # get artists for each individual genre
      temp <- get_genre_artists(genre = as.character(gens[i]), limit = 25) 
      
      # check if data is returned
      if (nrow(data.frame(temp)) > 0) {
        
        # modify data
        temp %<>% 
          rename(id_artist = id, artist_name = name) %>% 
          select(genre, artist_name, popularity, id_artist)
        
        # combine with rest of data
        data_artist <- temp %>% 
          bind_rows(data_artist)
      }
    }
    return(data_artist)
  }
  
  # function to get albums for each artist
  get_albums <- function(data_artist) {
    
    # initialize dataframe of albums for each artist
    data_album <- data.frame(id_artist = NA, id_album = NA, album_name = NA, release_date = NA, total_tracks = NA)
    data_album <- data_album[0, ]
    
    # extract artist id's
    arts <- unique(data_artist$id_artist)
    
    # get albums for each artist
    for (i in seq_along(arts)) {
      
      # get album for indiidual artist
       temp <- get_artist_albums(id = arts[i], include_groups = "album") 
       
       # progress check
       print(i)
       
       # check if data is returned
       if (nrow(data.frame(temp)) > 0) {
         
         # modify data
          temp %<>% 
            rename(id_album = id, album_name = name) %>% 
            mutate(id_artist = arts[i]) %>% 
            select(id_artist, id_album, album_name, release_date, total_tracks)
            
            # combine with rest of data
            data_album <- temp %>% 
              bind_rows(data_album) 
       }
    }
    
    # combine with original data
    data_album %<>% right_join(data_artist, by = c("id_artist" = "id_artist"))
  }
  
  # function to get tracks for each album
  get_tracks2 <- function(data_album) {
    
    # initialize dataframe of tracks for each album
    data_track <- data.frame(id_album = NA, id_track = NA, track_name = NA, track_number = NA)
    data_track <- data_track[0, ]
    
    # extract artist id's
    albs <- unique(data_album$id_album)
    
    # get tracks for each album
    for (i in seq_along(albs)) {
      
      # get tracks for an individual album
      temp <- get_album_tracks(id = albs[i])
      
      # progress check
      if (i %% 10 == 0) { print(i) }
      
      # check if data is returned
      if (ncol(data.frame(temp)) > 1) {
        
        # modify data
        temp %<>% 
          rename(id_track = id, track_name = name) %>% 
          mutate(id_album = albs[i]) %>% 
          select(id_album, id_track, track_name, track_number)
      
        # combine with rest of data
        data_track <- temp %>% 
          bind_rows(data_track)
      }
    }
    
    # combine with original data
    data_track %<>% right_join(data_album, by = c("id_album" = "id_album"))
  }
  
  # function to get data for each track
  get_track_info <- function(data_track) {    

    # initialize dataframe of track info
    data_track_info <- data.frame(id_track = NA, danceability = NA, energy = NA, key = NA, loudness = NA, mode = NA,
                                  speechiness = NA, acousticness = NA, instrumentalness = NA, liveness = NA,
                                  valence = NA, tempo = NA, duration_ms = NA, time_signature = NA)
    data_track_info <- data_track_info[0, ]

    # extract track id's
    tracks <- data_track$id_track

    # get data for each track
    for (i in seq_along(tracks)) {
      
      # get data for an individual track
      temp <- get_track_audio_features(id = tracks[i])
      
      # progess check
      if (i %% 10 == 0) { print(i) }
      
      # check if data is returned
      if (ncol(data.frame(temp)) > 1) {
        
        # modify data
        temp %<>%
          rename(id_track = id) %>%
          select(-c(type, uri, track_href, analysis_url))
      
        # combine with rest of data
        data_track_info <- temp %>% 
          bind_rows(data_track_info)
      } 
    }
    
    # combine with original data
    data_track_info %<>% right_join(data_track, by = c("id_track" = "id_track"))
  }
  
# Get data ---------------------------------------------------

  # initialize genres
  genres = c("blues", "edm", "rap", "jazz", "hip hop", "folk", "pop", "country", "soul",
             "reggae", "punk", "funk", "disco", "techno", "alternative rock", "classic rock",
             "pop rock", "world", "latin", "acoustic", "grunge", "bluegrass", "indie rock")
  
  # collect data in stages
  data_artist <- genres %>% 
    get_artists
  
  data_album <- data_artists[complete.cases(data_artist), ]  %>% 
    get_albums

  data_track <- data_album[complete.cases(data_album), ] %>% 
    get_tracks2
  
  data_track_info <- data_track[complete.cases(data_track), ] %>% 
    get_track_info
  
  # create final dataset
  data_music <- data_track_info[complete.cases(data_track_info), ]

  # save dataset
  save(data_music, file = "Music data.RData")
