library(dplyr)
library(ggplot2)
library(devtools)
install_github("ricardo-bion/ggradar")
library(ggradar)



View(data_music)

avg_value<-data_music %>% group_by(genre) %>% summarise(avg_dance=mean(danceability), avg_enery=mean(energy), avg_speechiness=mean(speechiness),
                                             avg_acousticness=mean(acousticness), avg_instrumentalness=mean(instrumentalness),
                                             avg_liveness=mean(liveness),avg_valance=mean(valence))


View(data_music %>% 
        group_by(genre))

View(avg_value)



ggradar(avg_value)


