suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(lubridate))

date <- seq.Date(from = as.Date("2000-01-01"),length.out = 360, by = 1)
df <- date %>% enframe(value = "date") %>%
  mutate(interval = lubridate::interval(start = date, end = date + 7))

# what I expect. dates are unique so group count equals row count
df %>% group_by(date) %>% count() %>% head(5)
# NOT what I expect. group_by treats all intervals as one group
df %>% group_by(interval) %>% count()

At #rstudioconf @JennyBryan mentioned trying the same thing over and over as a bad debugging method. Just spent an hour trying:

suppressPackageStartupMessages(library(dplyr))
df <- dplyr::tibble(ndx = c(1,2,3),fct = as.factor(c("yay","ugh",NA)))

#trying the same thing over
df %>% dplyr::mutate(fct = ifelse(is.na(fct),"ugh",fct)) #this HAS to work WTF??

#Other debugging technique, Google
df %>% dplyr::mutate(fct = forcats::fct_explicit_na(fct,"ugh"))
