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
library(viridis)
library(sf)
library(ggmap)
library(readxl)
library(shinycustomloader)
library(ggpubr)

# Load in RDS files

word_cloud <- readRDS("./rds_files/word-cloud.RDS")
heat <- readRDS("./rds_files/times.RDS")
house_names <- readRDS("./rds_files/house_names.RDS")
US <- readRDS("./rds_files/US.RDS")
map_1 <- readRDS("./rds_files/map.RDS")
comments <- readRDS("./rds_files/comments.RDS")
top_four <- readRDS("./rds_files/top-four.RDS")
full <- readRDS("./rds_files/full.RDS")

marquee_list <- list(marquee("comments"))


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
                          comedy for ", a("Satire V, ", href = "https://satirev.org"), "  jogging slowly around the Charles River, or inhabiting Harvard's maker spaces."),
                                  
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
                 tabPanel("By House",
                          fluidPage(
                              
                                     titlePanel("   Message Density Across 2019"),
                                        h4("   Explore By Dining Hall:"),
                                        br(),
                                        sidebarLayout(
                                            column(7,
                                            sidebarPanel(
                                                selectInput(
                                                    inputId = "house",
                                                    label = "House",
                                                    c("All", sort(house_names)))
                                                )
                                            ),
                                            column(12, align="center",
                                        mainPanel(width = 12, plotOutput("times")))
                                        )
                              
                          )),
                 tabPanel("By State",
                          fluidPage(
                              titlePanel("Messages By Student Home State"),
                              h4("Determined From Area Code"),
                              column(12,
                                     mainPanel(width = 12, imageOutput("map", width="auto", height="auto")))),
                              
                          fluidPage(
                            column(7, align="left",
                            p("While it sure looks like there are a much higher density of die-hard HUDS texters hailing from the Northeast and California, it's
                               important to remember that these numbers do not into take account relative abundance of these states' residents. Below is a plot
                               for the top four represented states, adjusted per capita."),
                            p("Massachusetts nonetheless looks awfully high; however, most international students acquire a 617 area code upon arriving at school, so they too are lumped in with 
                               MA residents."),
                            br()
                          )),
                          fluidPage(
                              column(7,
                                     plotOutput("top_four")))
                          
                          ),       
                 
                 tabPanel("Sentiment Analysis",
                          fluidPage(
                            titlePanel("Sentiment Analysis & Regression"),
                            p("Perhaps the most critical piece of information we can know before we make any judgments surrounding the volume of messages is the nature of their contents. 
                             Using the `rsentiment` package, I assigned each message a sentiment score, with positive values mapping onto more positive content, whereas negative values suggest criticism."),
                            p("Hour of day shows a weak, positive relationship with sentiment, suggesting that the attitudes of texters improve through the day or that dinner may simply be better than lunch. A similar progression 
                             is visible from Monday to Sunday, while month shows a slightly negative relationship with message sentiment."),
                            br(),
                                       fluidRow(
                                         column(3,
                                                selectInput(
                                                  inputId = "duration",
                                                  label = "Time Period",
                                                  c("Hour", "Weekday", "Month")),
                                                p("Toggle to view the relationship between sentiment and differing submission periods."),
                                                br(),
                                  
                                                selectInput(
                                                  inputId = "model",
                                                  label = "Regression Type",
                                                  c("Linear", "LOESS")
                                                )
                                         ),
                                         column(9, align="left",
                                                mainPanel(width = 40, plotOutput("regs")))
                                         
                                       ))),
                 tabPanel("Browse Messages",
                          fluidPage(
                              # Trying to get a marquee going; I'll figure this out later.
                              #mainPanel(renderCSS(type="text", loader=marquee_list)),
                              #marquee("<marquee>hello there! Hover over a word to see how many times it occurs across all messages </marquee>"),
                              h2("Text-and-Tell Word Cloud", align="left"),
                              h4("Hover over a word to see how many times it occurs across all messages!", align="left"),
                              br(),
                              column(12, align="center",
                              wordcloud2Output("wordPlot"), class = 'rightAlign'
                              ))))


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
    
    output$times <- renderPlot({
        
        ifelse(input$house == "All",
        houses <- heat,
        houses <- heat %>% filter(arranged == input$house)
            )
            
        ggplot(houses, aes(day,hour, fill=n))+
            geom_tile(color= "white",size=0.1) + 
            scale_fill_viridis(name = "# of Messages Sent", option = "C") +
            facet_grid(year ~ month) +
            scale_y_continuous(trans = "reverse", breaks = c(0:23)) +
            scale_x_continuous(breaks=c(1,10,20,31)) +
            theme_classic(base_size = 8) +
            theme(legend.position = "top") +
            labs(
                x = "Day",
                y = "Hour"
            )
    })
    
    output$map <- renderImage({
      list(
        src = "./images/map.jpg",
        contentType='image/jpg',
        width = 700,
        height = 457
      )}, deleteFile = F)
    
    output$top_four <- renderPlot({
      top_four$n <- top_four$n / c(944, 708, 944, 944)
      top_four %>%
        ggplot(aes(Location, n)) + 
        geom_col(fill = "#2bb673") + 
        labs(x = "Home State", y = "HUDS Messages Per Capita", title = "Messages Per Capita By Student Home State", subtitle = "For Top Four States") +
        theme_minimal()
      
    })
    
    output$regs <- renderPlot({
      
      if (input$duration == "Month" && input$model == "Linear") {
        print(full %>% ggplot(aes(x = month, y = ave_sentiment)) + geom_point(alpha = 0.3) +
                geom_jitter(alpha = 0.5) +
                geom_smooth(inherit.aes = F, aes(x = as.numeric(full$month), y = full$ave_sentiment), se = F, method = "lm", color = "#e95420") +
                labs(
                  x = "Month",
                  y = "Sentiment Score",
                  title = "Message Sentiment Score vs. Month Sent"
                ) + stat_cor(inherit.aes = F, aes(x = as.numeric(full$month), y = full$ave_sentiment), label.x = 7, label.y = 1.2) + stat_regline_equation(inherit.aes = F, aes(x = as.numeric(full$month), y = full$ave_sentiment), label.x = 7, label.y = 1.35))
      } else if (input$duration == "Month" & input$model == "LOESS") {
        print(full %>% ggplot(aes(x = month, y = ave_sentiment)) + geom_point(alpha = 0.3) +
                geom_jitter(alpha = 0.5) +
                geom_smooth(inherit.aes = F, aes(x = as.numeric(full$month), y = full$ave_sentiment), se = F, method = "loess", color = "#e95420", width = 2) +
                labs(
                  x = "Month",
                  y = "Sentiment Score",
                  title = "Message Sentiment Score vs. Month Sent"
                ) +
                theme_minimal()) 
      } else if (input$duration == "Weekday" & input$model == "Linear") {
        print(full %>% ggplot(aes(x = weekday, y = ave_sentiment)) + geom_point(alpha = 0.3) +
                geom_jitter(alpha = 0.3) +
                geom_smooth(inherit.aes = F, aes(x = as.numeric(full$weekday), y = full$ave_sentiment), se = F, method = "lm", color = "#e95420") +
                labs(
                  x = "Day of Week (1 = Monday, 7 = Sunday)",
                  y = "Sentiment Score",
                  title = "Message Sentiment Score vs. Day of Week Sent"
                ) + stat_cor(label.y = 1.30) + stat_regline_equation(label.y = 1.44))
      } else if (input$duration == "Weekday" & input$model == "LOESS") {
        print(full %>% ggplot(aes(x = weekday, y = ave_sentiment)) + geom_point(alpha = 0.3) +
                geom_jitter(alpha = 0.3) +
                geom_smooth(inherit.aes = F, aes(x = as.numeric(full$weekday), y = full$ave_sentiment), se = F, method = "loess", color = "#e95420") +
                labs(
                  x = "Day of Week (1 = Monday, 7 = Sunday)",
                  y = "Sentiment Score",
                  title = "Message Sentiment Score vs. Day of Week Sent"
                ))
      } else if (input$duration == "Hour" & input$model == "Linear") {
        print(full %>% ggplot(aes(x = hour, y = ave_sentiment)) + geom_point(alpha=0.3) +
                geom_jitter(alpha = 0.3) +
                geom_smooth(inherit.aes = F, aes(x = as.numeric(full$hour), y = full$ave_sentiment), se = F, method = "lm", color = "#e95420") +
                labs(
                  x = "Hour of Day",
                  y = "Sentiment Score",
                  title = "Message Sentiment Score vs. Hour Sent"
                ) + stat_cor(label.y = 1.30) + stat_regline_equation(label.y = 1.45))
      } else if (input$duration == "Hour" & input$model == "LOESS") {
        print(full %>% ggplot(aes(x = hour, y = ave_sentiment)) + geom_point(alpha = 0.3) +
                geom_jitter(alpha = 0.3) +
                geom_smooth(inherit.aes = F, aes(x = as.numeric(full$hour), y = full$ave_sentiment), se = F, method = "loess", color = "#e95420") +
                labs(
                  x = "Hour of Day",
                  y = "Sentiment Score",
                  title = "Message Sentiment Score vs. Hour Sent"
                ))
      }
     
    })

    
    # For some reason, this plot would not render properly/threw an error every time :'(
    # I will be sure to fix when I turn it in!
    
    
    #output$map <- renderPlot({
    #     ggplot(data = US, aes(x = long, y = lat)) + geom_polygon(fill="grey", aes(group = group)) +
    #         coord_map() + geom_point(data = map_1, color="#f15b29", aes(x = longitude, y = latitude, size = Messages)) +
    #         xlim(-180, -50) + theme_classic() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
    #                                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
    #                                                   axis.title.x=element_blank(),
    #                                                   axis.title.y=element_blank(),
    #                                                   panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
    #                                                   panel.grid.minor=element_blank(),plot.background=element_blank())
    # })
    
     output$scroll <- renderCSS({
       marquee(marquee_list, behavior = "scroll", direction = "left",
              scrollamount = 6, width = "100%")
     })
    
     output$wordPlot <- renderWordcloud2({
        
        wordcloud2(word_cloud, size = 1.3, color = rep_len(c("#f15b29", "#2bb673", "#BDD2FF", "#f3b204", "#F59187"), nrow(demoFreq)))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
