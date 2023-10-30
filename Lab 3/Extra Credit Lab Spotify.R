#https://www.rcharlie.com/spotifyr/
## Set up dev account
## https://developer.spotify.com/dashboard/
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("knitre")
#install.packages("devtools)
#devtools::install_github('charlie86/spotifyr')



library(spotifyr)
library(lubridate)
library(tidyverse)
library(knitr)

##############
## My API Key Above this
##############
Sys.setenv(SPOTIFY_CLIENT_ID = '')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '')
#access_token <- get_spotify_access_token()
#access_token <- get_spotify_authorization_code(scope = scopes()[c(7,8,9,10,14,15)])
access_token <- get_spotify_authorization_code(scope = "user-read-recently-played")











################
## Code Examples
################
get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()


get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()

ed <- get_artist_audio_features('ed sheeran')

ed %>% 5
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  distinct() %>%
  head(5) %>% 
  kable()

library(ggjoy)

ggplot(ed, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Joyplot of Ed's joy distributions", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")

