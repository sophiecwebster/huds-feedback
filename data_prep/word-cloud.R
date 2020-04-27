library(tidyverse)
library(gganimate)
library(gifski)
library(rvest)
library(tm)
library(wordcloud)
library(wordcloud2)
library(shinythemes)

tntell <- read.csv("./data_prep/textntell.csv", stringsAsFactors = FALSE) %>% select(Start, Tracker, Location, Mobile.Number, Comment)
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

saveRDS(object = df, "./shiny_app/rds_files/word-cloud.RDS")
