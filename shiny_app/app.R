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

tntell <- read.csv("../data_prep/textntell.csv", stringsAsFactors = FALSE) %>% select(Start, Tracker, Location, Mobile.Number, Comment)
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


ui <- navbarPage(theme = shinytheme("united"),
                 "HUDS Feedback",
                 
                 ## About ##
                 
                 tabPanel("About",
                          column(7,

                          h1("Background"),
                          p("Howdy!"),
                          p("Hi again"),
                          h1("Data"),
                          h1("About Me"),
                          p("I'm Sophie Webster, and I'm currently a junior at Harvard College studying 
                            Integrative Biology with a secondary in Earth & Planetary Sciences. When I'm not click-clacking away in RStudio, you 
                            can find me singing a cappella with the ", a("Radcliffe Pitches", href = "https://pitches.org"), ", writing 
                          comedy for ", a("Satire V", href = "https://satirev.org"), ", jogging slowly around the Charles River, or inhabiting Harvard's maker space."),
                                  
                          p("Thanks for stopping by! You can reach me at ",
                            a("sophiewebster@college.harvard.edu",
                              href = "mailto: sophiewebster@college.harvard.edu"),
                            "or on ",
                            a("LinkedIn",
                              href = "https://www.linkedin.com/in/sophie-webster-651b03171/"),".")
                          ),
                          column(2,
                              imageOutput("huds", height = "50%", width = "50%"),
                              imageOutput("food", height = "50%", width = "50%"))
                              ),
                          
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
    
    # Images for the About tab
    output$huds <- renderImage({
        list(
            src = './images/huds.png',
            contentType='image/png'
        )}, deleteFile = F)
    
    output$food <- renderImage({
        list(
            src = './images/food.jpg',
            contentType='image/jpg'
        )}, deleteFile = F)
    
    output$wordPlot <- renderWordcloud2({
        
        wordcloud2(df, size = 1.3, color = rep_len(c("#f15b29", "#2bb673", "#BDD2FF", "#f3b204", "#F59187"), nrow(demoFreq)))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
