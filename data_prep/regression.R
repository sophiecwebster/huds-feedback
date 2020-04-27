library(tidyverse)
library(gganimate)
library(gifski)
library(rvest)
library(tm)
library(wordcloud)
library(wordcloud2)
library(lubridate)
library(sf)
library(ggmap)
library(readxl)
library(sentimentr)
library(tidyr)
library(broom)

tntell <- read.csv("./textntell.csv", stringsAsFactors = FALSE) %>% select(Start, Tracker, Location, Mobile.Number, Comment)

tntell$arranged <- tntell$Location %>% fct_relevel("Annenberg", "Lowell", "Dunster", "Cabot", "Quincy", "Mather", "Pforzheimer", "Winthrop", "Currier", "Leverett", "Eliot", "Adams", "Hillel", "Kirkland", "FlyBy", "Dudley")



tntell$area <- sub(".", "", tntell$Mobile.Number)
tntell$area <- substr(tntell$area,1,3) %>% as.double(tntell$area)

# mapping phone numbers

url <- paste0("https://www.areacodelocations.info/areacodelist.html")
h <- read_html(url)
html_text(h)
tab <- h %>% html_nodes("table")
tab <- tab %>% html_table() %>% as.data.frame()
tab <- tab[,c(1,2)]
tab$Area.code <- as.numeric(tab$Area.code)

tntell$House <- tntell$Location
located <- left_join(tntell[, c(1,2,4,5,6,7)], tab, by = c("area" = "Area.code"))
#mass = 236, ny = 236, ca = 236, tx = 177
# can either group these by region or just look at peeps in mass, ny, ca, tx
# also prob MA encompasses international students

# Lubridating -- ymd_hms 

tntell$time <- ymd_hms(tntell$Start)

tntell <- tntell %>% mutate(year = year(time),
                            month = month(time, label=TRUE),
                            day = day(time),
                            hour = hour(time),
                            weekday = wday(time))

# Classifying the messages as breakfast, lunch, and dinner

tntell <- tntell %>%
  mutate(meal = case_when(
    hour(time) %in% c(7:10) ~ "breakfast",
    hour(time) %in% c(11:15) ~ "lunch",
    hour(time) %in% c(16:21) ~ "dinner",
    hour(time) %in% c(1:6, 21:24) ~ "brain break"
  ))

# Using sentimentr to give each comment a sentiment 'score'
# Positive values denote... positivity! Negative values denote negativity.

phrases <- get_sentences(tntell$Comment)
sentiments <- sentiment_by(phrases)

full <- cbind(tntell, sentiments)

hour_model <- lm(ave_sentiment ~ hour, data = full) %>%
  tidy(conf.int=T)
month_model <- lm(ave_sentiment ~ month, data = full) %>%
  tidy(conf.int=T)
weekday_model <- lm(ave_sentiment ~ weekday, data = full) %>%
  tidy(conf.int=T)

saveRDS(full, '~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/full.RDS')

ggplot(aes(month, y = ave_sentiment)) + geom_point() +
  geom_smooth(se = F, method = "lm")

new <- full %>% select(c("month", "ave_sentiment"))

