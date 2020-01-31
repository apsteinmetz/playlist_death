#library(xts)
library(tidyverse)
library(tidymodels)
library(lubridate)
#feature engineering

# pivot deaths so all bands are in one column though this will make names not unique.
load("data/playlists.Rdata")
playlists <- playlists %>% filter(ArtistToken != "")

load("data/deaths.rdata")
deaths <- deaths %>%
  pivot_longer(cols = starts_with("band"),
               values_to = "band",
               values_drop_na = T) %>%
  select(-name) %>%
  mutate(band = str_trim(band)) %>%
  {.}


# artist spins by date
# create interval of two weeks using lubridate

# feature play count
play_count<-playlists %>%
  ungroup() %>%
  filter(ArtistToken != "") %>%
#  mutate(date_window = lubridate::interval(start = AirDate-14,end = AirDate))  %>%
  mutate(date_window = quarter(AirDate,with_year = TRUE))  %>%
  group_by(date_window,ArtistToken) %>%
  summarise(spins = n()) %>%
  {.}

# feature dj count
dj_counts<-playlists %>%
  ungroup() %>%
  mutate(date_window = quarter(AirDate,with_year = TRUE))  %>%
  group_by(ArtistToken,AirDate,DJ) %>%
  summarise(Spins=n()) %>%
  summarise(dj_count=n()) %>%
  group_by(ArtistToken) %>%
  {.}

#feature: artist concentration within shows
temp <-playlists %>%
  ungroup() %>%
  mutate(date_window = quarter(AirDate,with_year = TRUE))  %>%
  group_by(DJ,AirDate) %>%
  summarise(total_spins = n()) %>%
#  summarise(dj_count=n()) %>%
#  group_by(ArtistToken) %>%
  {.}
dj_concentration <-playlists %>%
  ungroup() %>%
  mutate(date_window = quarter(AirDate,with_year = TRUE))  %>%
  group_by(DJ,AirDate,ArtistToken) %>%
  summarise(spins = n()) %>%
  {.}
