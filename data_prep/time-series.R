library(tidyverse)
library(gganimate)
library(gifski)
library(rvest)
library(tm)
library(wordcloud)
library(wordcloud2)
library(lubridate)

tntell <- read.csv("./textntell.csv", stringsAsFactors = FALSE) %>% select(Start, Tracker, Location, Mobile.Number, Comment)

tntell$arranged <- tntell$Location %>% fct_relevel("Annenberg", "Lowell", "Dunster", "Cabot", "Quincy", "Mather", "Pforzheimer", "Winthrop", "Currier", "Leverett", "Eliot", "Adams", "Hillel", "Kirkland", "FlyBy", "Dudley")

# Lubridating -- ymd_hms 

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

# Making heat plot; this will be an interactive plot by house

heat <- tntell %>%
  group_by(month, day, hour, year, arranged) %>%
  count()

ggplot(heat, aes(day,hour, fill=n))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name = "# of Messages Sent", option = "C") +
  facet_grid(year ~ month) +
  scale_y_continuous(trans = "reverse", breaks = c(0:23)) +
  scale_x_continuous(breaks=c(1,10,20,31)) +
  theme_classic(base_size = 8) +
  theme(legend.position = "bottom") +
  labs(
    x = "Day",
    y = "Hour"
  )

# For UI inputs

house_names <- c(unique(tntell$Location))

saveRDS(object = heat, file = "./shiny_app/rds_files/times.RDS")
saveRDS(object = house_names, file = "./shiny_app/rds_files/house_names.RDS")
  
tntell %>%
  ggplot(aes(time, fill = arranged, alpha = 0.1)) + geom_density()

