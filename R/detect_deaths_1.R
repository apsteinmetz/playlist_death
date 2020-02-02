library(xts)
library(tidyverse)
library(tidymodels)
library(lubridate)

#problems to solve
# 1. creating quarterly buckets is coarse and still risks an event right before the quarter boundary, splitting the
# DJs who react across two quarters.  A rolling window scheme would avoid that but would increase the noise as
# many windows might have signal

# 2. need to create a "match any" scheme that still has one unique instance of any variants of artist name so we
# don't create false missed events.
# 3. Are artists that are in the death data but not in playlists not played at all or am I using a bad token
# Say "E Street Band" counting as a miss even though "Bruce Springsteen" would be played.
# In the former case I should set NAs to zero plays, in the latter I should delete rows with NAs.  I chose the latter.


#feature engineering

# pivot deaths so all bands are in one column though this will make names not unique.
load("data/playlists.Rdata")
playlists <- playlists %>%
  filter(ArtistToken != "") %>%
  #  mutate(date_window = lubridate::interval(start = AirDate-14,end = AirDate))  %>%
  mutate(date_window = as.yearqtr(AirDate))  %>%
  {.}

load("data/deaths.rdata")
deaths <- deaths %>%
  pivot_longer(cols = starts_with("band"),
               values_to = "band",
               values_drop_na = T) %>%
  select(-name) %>%
  mutate(band = str_trim(band)) %>%
  mutate(date_window = as.yearqtr(Date)) %>%
  {.}

#ha, ha
# add name rows and band rows sequentially so we'll pick up both
deaths_row <- deaths %>%
  select(date_window,Name,band) %>%
  pivot_longer(cols = c("Name","band"),names_to= "discard",values_to = "ArtistToken") %>%
  mutate(event = factor("death",levels = c("death","not death"))) %>%
  select(-discard) %>%
  {.}



# artist spins by date
# create interval of two weeks using lubridate


# feature dj count
dj_counts<-playlists %>%
  ungroup() %>%
  group_by(ArtistToken,date_window,DJ) %>%
  summarise(Spins=n()) %>%
  summarise(dj_count=n()) %>%
  group_by(ArtistToken) %>%
  {.}

#features: play absolute count and percentage
all_plays <-playlists %>%
  ungroup() %>%
  group_by(date_window) %>%
  summarise(total_spins = n()) %>%
  {.}

play_percentage <-playlists %>%
  ungroup() %>%
  group_by(date_window,ArtistToken) %>%
  summarise(spins = n()) %>%
  left_join(all_plays,by = c("date_window")) %>%
  group_by(date_window,ArtistToken) %>%
  summarise(spins = sum(spins),total_spins = sum(total_spins)) %>%
  mutate(concentration = spins/total_spins) %>%
  select(-total_spins) %>%
  {.}

artist_data <- left_join(play_percentage,dj_counts)

artist_data_full <- full_join(artist_data,deaths_row) %>%
  mutate(event = fct_explicit_na(event,na_level = "not death")) %>%
  drop_na() %>%
  {.}



# ---------- run models ----------------------------
lm_spec <- logistic_reg(mode = "classification", penalty = NULL, mixture = NULL) %>%
  set_engine("glm")



