library(tidyverse)
library(gganimate)
library(gifski)
library(rvest)
library(tm)
library(wordcloud)
library(wordcloud2)

tntell <- read.csv('./textntell.csv', stringsAsFactors = FALSE) %>% select(Start, Tracker, Location, Mobile.Number, Comment)
 

tntell$arranged <- tntell$Location %>% fct_relevel("Annenberg", "Lowell", "Dunster", "Cabot", "Quincy", "Mather", "Pforzheimer", "Winthrop", "Currier", "Leverett", "Eliot", "Adams", "Hillel", "Kirkland", "FlyBy", "Dudley")

# for loop thru all locations? maybe make new frame for number of and folks per house
# tntell %>% filter(Location == "Dunster") %>% count()
# 
# for(i in unique(tntell$Location[1:16])){
#   filter(Location = "i") %>% count()
# }

per_cap <- data.frame(House = unique(tntell$Location), Student.Pop = c(371, 414, 476, 430, 408, 466, 381, 361, 1715, 388, NA, 403, 515, NA, 390, NA), Messages = c(143, 361, 255, 61, 143, 41, 163, 275, 429, 276, NA, 189, 73, NA, 19, NA)) %>% filter(!is.na(Student.Pop))

ggplot(tntell, aes(x = forcats::fct_rev(arranged))) + geom_bar(fill = "#f15b29") + coord_flip() + ggtitle("Who Sent HUDS the Most Messages in 2019?") + labs(y = "", x = "") + theme_light()

ggplot(per_cap, aes(x = reorder(House, Messages/Student.Pop), y = (Messages/Student.Pop))) + geom_col(fill = "#f15b29") + coord_flip() + ggtitle("Messages Per Capita By Harvard House") + labs(y = "", x = "") + theme_light() #+ labs(y = "Messages Per Capita")

# do comments per person
# crankiest students

# what about a "today's menu" section that scrapes site and says how people in the past have
# reacted to that entree
# wordcloud

# best rated/ranked quotes
# unique phone numbers versus folks in dhall
# positivity -- train language classifier!
# double check on publishing individual messages
# is this a representative sample of opinions?
# nicest area code? nicest state?
 

# pulling out area codes 

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

 

 #{r making geographic plot}
#"#f3b204", "#F59187"
located %>%
  ggplot(aes(Location)) + 
  geom_bar(fill = "#f15b29") + 
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(lineheight = 10, size=6)) +
  scale_x_discrete(limits = rev(unique(sort(located$Location)))) +
  labs(y = "", x = "", title = "Number of Messages Sent by Student Home State", caption = "*International students likely included in MA counts")
 

#{r top four states, per capita}

top_four <- located %>%
  filter(Location %in% c("Massachusetts", "Texas", "California", "New York")) %>%
  group_by(Location) %>%
  count()

saveRDS(top_four, "~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/top-four.RDS")

# these numbers are estimates from data I hunted down online (I could only find
# these top state's statistics, which (unsurprisingly) ended up being the
# biggest contributors to the HUDS messages)

top_four$n <- top_four$n / c(944, 708, 944, 944)
top_four %>%
  ggplot(aes(Location, n)) + 
  geom_col(fill = "#2bb673") + 
  labs(x = "Home State", y = "HUDS Messages Per Capita", title = "Messages Per Capita By Student Home State", subtitle = "For Top Four States")

# Massachusetts is almost off the charts! This is likely because we haven't
# factored in international students, many of whom get a Massachusetts phone
# number upon arriving. When we add these roughly 1180 undergraduates, we get a
# proportion that is far more within range.

internat <- located %>%
  filter(Location %in% c("Massachusetts", "Texas", "California", "New York")) %>%
  group_by(Location) %>%
  count()
internat$n <- internat$n / c(944, 2124, 944, 708)
internat %>%
  ggplot(aes(Location, n)) + geom_col(fill = "#f3b204") +
  labs(x = "Home State", y = "HUDS Messages Per Capita", title = "Messages Per Capita By Student Home State", subtitle = "Including International Students in Massachusetts' Count") +
  theme_minimal()
 
saveRDS(internat, "~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/ma_adjusted.RDS")

comment <- as.character(tntell$Comment)
doc <- Corpus(VectorSource(comment))
doc_clean <- doc %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

doc2 <- tm_map(doc_clean, content_transformer(tolower))
final_doc <- tm_map(doc2, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(final_doc)
matrix <- as.matrix(dtm)
wordd <- sort(rowSums(matrix), decreasing = T)
df <- data.frame(word = names(wordd), freq=wordd)
 


# generate the word cloud

wordcloud(df$word, freq = df$freq, min.freq = 1, max.words = 200, random.order = F, rot.per=0.35, colors=brewer.pal(8,"Dark2"))

# use wordcloud2 to make more enhanced visualization; found HUDS' color palette 

wordcloud2(df, size = 1.3, color = rep_len(c("#f15b29", "#2bb673", "#BDD2FF", "#f3b204", "#F59187"), nrow(demoFreq)))

# for my attempt at a CSS scrolling marquee

saveRDS(tntell$Comment, "~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/comments.RDS")


ggdotchart(per_cap, x = "House", y = "Messages", color = "#f15b29", add = "segments", add.params = list(color = "#e95420", size = 0.7), dot.size = 6, label = round(per_cap$Messages), font.label = list(color = "white", size = 8, 
                                                                                                                                                       vjust = 0.5, ggtheme = theme_pubr())) + coord_flip() +
  labs(x = "", y="Message Count")

per_cap <- per_cap %>%
  mutate(cap = Messages/Student.Pop)

saveRDS(per_cap, "~/Desktop/Gov 1005/huds-feedback/shiny_app/rds_files/per_cap.RDS")
ggdotchart(per_cap, x = "House", y = "cap", color = "#00AFBB", add = "segments", add.params = list(color = "#00AFBB", size = 0.7), dot.size = 6, label = round(per_cap$cap, digits = 2), font.label = list(color = "white", size = 7, 
                                                                                                                                                                                                         vjust = 0.5, ggtheme = theme_pubr())) + coord_flip() +
  labs(x = "", y="Messages Per Capita")




