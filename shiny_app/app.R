# Prepare environment

library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(rvest)
library(tm)
library(wordcloud)
library(wordcloud2)
library(shinythemes)

# Load in RDS files

tntell <- read.csv("./textntell.csv", stringsAsFactors = FALSE) %>% select(Start, Tracker, Location, Mobile.Number, Comment)
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

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("united"),
                 "HUDS Feedback",
                 fluidPage(
                     
                     # Application title
                     titlePanel("Text-and-Tell Word Cloud"),
                     
                   
                     mainPanel(
                         wordcloud2Output("wordPlot")
                     )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$wordPlot <- renderWordcloud2({
        
        wordcloud2(df, size = 1.3, color = rep_len(c("#f15b29", "#2bb673", "#BDD2FF", "#f3b204", "#F59187"), nrow(demoFreq)))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
