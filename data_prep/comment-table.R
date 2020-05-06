library(DT)
library(tidyverse)
library(gganimate)
library(gifski)
library(rvest)
library(tm)
library(wordcloud)
library(wordcloud2)
library(lubridate)
library(sentimentr)
library(tools)

tntell <- read.csv('./textntell.csv', stringsAsFactors = FALSE) %>% select(Start, Tracker, Location, Mobile.Number, Comment)


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


tntell$time <- ymd_hms(tntell$Start)

tntell <- tntell %>% mutate(year = year(time),
                            month = month(time, label=TRUE),
                            day = day(time),
                            hour = hour(time))

# Classifying the messages as breakfast, lunch, and dinner

tntell <- tntell %>%
  mutate(meal = case_when(
    hour(time) %in% c(7:10) ~ "breakfast",
    hour(time) %in% c(11:15) ~ "lunch",
    hour(time) %in% c(16:21) ~ "dinner",
    hour(time) %in% c(1:6, 21:24) ~ "brain break"
  ))

phrases <- get_sentences(tntell$Comment)
sentiments <- sentiment_by(phrases)

full <- cbind(tntell, sentiments)

# Just for consistency's sake with case!

full$Meal <- toTitleCase(full$meal)
full$Sentiment <- full$ave_sentiment %>% round(digits = 4)

full <- full %>% slice(1:200)

tntel1 <- full[, c('Comment', 'House', 'Meal', 'Sentiment')]

saveRDS(tntel1, '~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/table.RDS')

