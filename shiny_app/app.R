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

word_cloud <- readRDS("./rds_files/word-cloud.RDS")


ui <- navbarPage(theme = shinytheme("united"),
                 "HUDS Feedback",
                 
                 ## About ##
                 
                 tabPanel("About",
                          column(7,

                          h1("Background"),
                          p("Harvard University Dining Services (colloquially known as HUDS) manages twelve dining halls in the undergraduate student houses, the first-year dining hall (Annenberg), one kosher dining hall (Harvard Hillel), and two graduate dining halls for graduate students. "),
                          p("They serve breakfast, lunch, and dinner to hungry students every day, feeding a population of over 7000 daily. The rotating menu offers a variety of seasonal selections, healthy options, and comforting favorites — just ask any undergraduate, and they'll tell you their HUDS pick!"),
                          p("In 2017, HUDS rolled out a program initially coined ", em("Text-and-Tell,"), "in which students can send a text message to the manager of their house's dining hall. In an ", a("article", href="https://www.thecrimson.com/article/2019/3/7/huds-texting/"), "in ", em("The Crimson"), "'s", em("Fifteen Minutes Magazine,"), "HUDS Managing Director David P. Davidson
                            estimated that nearly 98% of messages are positive, while the other 2% are requests for specific dishes.
                            Students are known to send humorous feedback, and HUDS managers are known for their even wittier replies. As a huge HUDS fan, I wanted to better understand how this feedback tool is used by students through visualization and statistical analysis."),
                          h1("Data"),
                          p("This project analyzes the almost 2500 text messages sent by students during the fall and spring semesters of 2019. This data was kindly given to me by Crista Martin, Director for Strategic Initiatives & Communications at HUDS. While,
                            as a courtesy to her and HUDS, I will not publicize the data on this site, you can contact Crista if you have 
                            any questions or curiosities."),
                          h1("About Me"),
                          p("My name is Sophie Webster, and I'm an avid texter of HUDS. I'm also a current junior at Harvard College studying 
                            Integrative Biology with a secondary in Earth & Planetary Sciences. This is my final project for ", a("Gov 1005,", href="https://www.davidkane.info/files/gov_1005_spring_2020.html"), "a data science course taught in R. When I'm not click-clacking away in RStudio, you 
                            can find me singing jazz a cappella with the ", a("Radcliffe Pitches,", href = "https://pitches.org"), "writing 
                          comedy for ", a("Satire V,", href = "https://satirev.org"), "jogging slowly around the Charles River, or inhabiting Harvard's maker spaces."),
                                  
                          p("Thanks for stopping by! Feel free to say hi at ",
                            a("sophiewebster@college.harvard.edu",
                              href = "mailto: sophiewebster@college.harvard.edu"),
                            "or on ",
                            a("LinkedIn.",
                              href = "https://www.linkedin.com/in/sophie-webster-651b03171/"))
                          ),
                          column(1,
                              br(),
                              br(),
                              imageOutput("huds", height = "100%", width = "100%"),
                              br(),
                              br(),
                              imageOutput("food", height = "100%", width = "100%"))
                              ),
                 tabPanel("By Location"),
                          
                 tabPanel("Sentiment Analysis"),
                 tabPanel("Browse",
                          fluidPage(
                              
                              
                              titlePanel("Text-and-Tell Word Cloud"),
                              
                              
                              mainPanel(wordcloud2Output("wordPlot"), class = 'rightAlign'
                              )))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Adding two images for the About tab
    
    output$huds <- renderImage({
        list(
            src = './images/huds.png',
            contentType='image/png',
            width = 400,
            height = 262
        )}, deleteFile = F)
    
    output$food <- renderImage({
        list(
            src = './images/food.jpg',
            contentType='image/jpg',
            width = 400,
            height = 262
        )}, deleteFile = F)
    
    output$wordPlot <- renderWordcloud2({
        
        wordcloud2(word_cloud, size = 1.3, color = rep_len(c("#f15b29", "#2bb673", "#BDD2FF", "#f3b204", "#F59187"), nrow(demoFreq)))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
