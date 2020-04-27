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
                            hour = hour(time))

# Classifying the messages as breakfast, lunch, and dinner

tntell <- tntell %>%
  mutate(meal = case_when(
    hour(time) %in% c(7:10) ~ "breakfast",
    hour(time) %in% c(11:15) ~ "lunch",
    hour(time) %in% c(16:21) ~ "dinner",
    hour(time) %in% c(1:6, 21:24) ~ "brain break"
  ))


US <- map_data(map = "world", region = "US")
saveRDS(US, '~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/US.RDS')

#US <- map_data("state")
# library(albersusa)
# US <- usa_composite()

#world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

state_ids <- read_excel('./data_prep/state-ids.xlsx')

state_ids$Location <- state_ids$name

locations <- left_join(located, state_ids, by = "Location")

revised_loc <- locations %>%
  group_by(Location) %>%
  count()

to_plot <- left_join(revised_loc, state_ids, by = "Location")

to_plot <- rename(to_plot, "Messages" = n)

to_plot <- to_plot[1:4]

to_plot <- to_plot %>% filter(!is.na(latitude))

saveRDS(to_plot, "~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/map.RDS")

ggplot(data = US, aes(x = long, y = lat)) + geom_polygon(fill="grey", aes(group = group)) +
  coord_map() + geom_point(data = to_plot, color="#f15b29", aes(x = longitude, y = latitude, size = Messages)) +
  xlim(-180, -50) + theme_classic() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                            axis.text.y=element_blank(),axis.ticks=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                            panel.grid.minor=element_blank(),plot.background=element_blank())

#ggsave('~/Desktop/Gov 1005/huds-feedback/shiny_app/images/map.jpg', last_plot())










