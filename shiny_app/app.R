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
library(readxl)
library(shinycustomloader)
library(ggpubr)
library(plotly)
library(DT)

# Load in RDS files

word_cloud <- readRDS("./rds_files/word-cloud.RDS")
heat <- readRDS("./rds_files/times.RDS")
house_names <- readRDS("./rds_files/house_names.RDS")
US <- readRDS("./rds_files/US.RDS")
to_plot <- readRDS("./rds_files/map.RDS")
comments <- readRDS("./rds_files/comments.RDS")
top_four <- readRDS("./rds_files/top-four.RDS")
ma_adjusted <- readRDS("./rds_files/ma_adjusted.RDS")
full <- readRDS("./rds_files/full.RDS")
per_cap <- readRDS("./rds_files/per_cap.RDS")
table <- readRDS("./rds_files/table.RDS")

#marquee_list <- list(marquee("comments"))


ui <- navbarPage(theme = shinytheme("united"),
                 "HUDS Feedback",
                 
                 ## About ##
                 
                 tabPanel("About",
                          fluidPage(
                          fluidRow(
                          column(7,

                          h1("Background"),
                          p("Harvard University Dining Services (colloquially known as HUDS) manages twelve dining halls in the undergraduate student houses, the first-year dining hall (Annenberg), one kosher dining hall (Harvard Hillel), and two graduate dining halls for graduate students. "),
                          p("They serve breakfast, lunch, and dinner to hungry students every day, feeding a population of over 7000 daily. The rotating menu offers a variety of seasonal selections, healthy options, and comforting favorites — just ask any undergraduate, and they'll tell you their HUDS pick!"),
                          p("In 2017, HUDS rolled out a program initially coined ", em("Text-and-Tell,"), "in which students can send a text message to the manager of their house's dining hall. In an ", a("article", href="https://www.thecrimson.com/article/2019/3/7/huds-texting/"), "in ", em("The Crimson"), "'s", em("Fifteen Minutes Magazine,"), "HUDS Managing Director David P. Davidson
                            estimated that nearly 98% of messages are positive, while the other 2% are requests for specific dishes.
                            Students are known to send humorous feedback, and HUDS managers are known for their even wittier replies. As a huge HUDS fan, I wanted to better understand how this feedback tool is used by students through visualization and statistical analysis."),
                          h1("Data"),
                          p("This project analyzes the almost 2500 text messages sent to HUDS by students during the fall and spring semesters of 2019. Each message reports the time it was sent, its Harvard house of origin, the sender's phone number, and a unique ID string. This data was kindly given to me by Crista Martin, Director for Strategic Initiatives & Communications at HUDS. While,
                            as a courtesy to her, HUDS, and student senders, I will not publicize the data on this site, you can contact Crista if you have 
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
                              href = "https://www.linkedin.com/in/sophie-webster-651b03171/"), br(), "Code for this project can be found ", a("here.", href="https://github.com/sophiecwebster/huds-feedback"))
                          ),
                          
                          column(5, align="center",
                              br(),
                              br(),
                              imageOutput("huds", height = "100%", width = "100%"),
                              br(),
                              br(),
                              imageOutput("food", height = "100%", width = "100%"))
                              ))),
                 
                 ## By House Analysis ## 
                 
                 tabPanel("By House",
                          fluidPage(
                              
                                     titlePanel("   Exploring Messages By Dining Hall"),
                                     br(),
                                      
                                    fluidRow(
                                      column(6, 
                                             withLoader(imageOutput("percap"), type="html", loader="loader2"),
                                             ),
                                      column(5,
                                             br(),
                                             p("Annenberg (the dining hall for all 1700 first-year students) looks like it takes the cake for messages sent in 2019, but when we correct for student population, 
                                               Lowell House takes the lead with 0.87 texts sent per capita. Interestingly, we see that houses run the gamut from this upper value to Kirkland's meager 0.05 messages per capita,
                                               with relatively consistent intervals between each house all the way down the scale. This may be attributable to several different factors, as it's unlikely that Lowellians are simply more loquacious over text by nature, given that housing assignments are random.
                                               Perhaps Lowell, whose dining hall just opened this year, is still sorting out some kinks and garnering a lot of messages. Meanwhile, Kirkland and Leverett, which have much lower values,
                                               do not have the table tents that clearly advertise the Text-n-Tell program in other dining halls. Likely, students who aren't reminded that they can send feedback to HUDS are sending
                                               fewer messages."))
                                    ),
                                        br(),
                                        sidebarLayout(
                                            column(7,
                                            h3("Message Density Across 2019"),
                                            br(),
                                            sidebarPanel(
                                                selectInput(
                                                    inputId = "house",
                                                    label = "House",
                                                    c("All", sort(house_names)))
                                                    
                                                ),
                                            column(6,
                                            p("As for temporal trends, there are two strong bands for lunch and dinner, but somewhat of an aching void over the summertime (where, apparently, summer school students either opt not to send messages or don't have this program in place). Toggle between houses to check out their message distribution throughout the year."))
                                            ),
                                            column(12, align="center",
                                        mainPanel(width = 12, withLoader(plotOutput("times"), type="html", loader="loader2"), br()))),
                                        )
                              
                          ),
                 
                 ## Geographic Analysis ## 
                 
                 tabPanel("By State",
                          fluidPage(
                              titlePanel("Messages By Student Home State"),
                              h4("Determined From Area Code"),
                              fluidRow(
                              column(11, align="center",
                                     mainPanel(width = 12, withLoader(plotlyOutput("map_2", width="78%", height = "auto"), type="html", loader="loader2"))))),#imageOutput("map", width="auto", height="auto")))),
                                      
                          fluidPage(
                            column(11, align="left",
                            p("Sentiment score on this map is an average of the scores of each message sent by a state's residents. A higher sentiment score indicates a state's residents sent more positive, complimentary messages, and a larger dot corresponds to a higher message count. It bears mentioning that the more extreme sentiment-scored states tend to have fewer total messages sent (especially 
                              West Virginia!), whereas states sending more messages tend to settle into an average sentiment score of approximately 0.25."),
                            p("Moreover, while it sure looks like there is a much higher density of die-hard HUDS texters hailing from the Northeast and California, it's
                               important to remember that these numbers do not take into account relative abundance of each state's student population. Below is a plot
                               for the top four represented states, adjusted per capita."),
                            p("Massachusetts nonetheless looks awfully high, over double the per capita sending of California, with the next highest value. However, it bears mentioning that most international students acquire a '617' area code upon arriving at school, so they too are lumped in with 
                               Massachusetts residents. The number of international students is included in the adjusted plot at the right, which tells a much more believable story."),
                            br()
                          )),
                          fluidPage(
                              column(6,
                                     plotOutput("top_four")),
                              column(6,
                                     plotOutput("ma_adjusted"))),
                          br()
                          
                          ),    
                 
                 ## Sentiment Analysis ##
                 
                 tabPanel("Sentiment Analysis",
                          fluidPage(
                            titlePanel("Sentiment Analysis & Regression"),
                            p("Perhaps the most critical piece of information we can know before we make any judgments surrounding the volume of messages is the nature of their contents. 
                             Using the `rsentiment` package, I assigned each message a sentiment score, with positive values mapping onto more positive content, whereas negative values suggest criticism. 
                              This package works by assigning connotation scores to words in the text message and calculating the text's overall positivity or negativity. "),
                            p("This regression explores how sentiment changes with time across different intervals — 24 hours, 7 days, or 12 months. The hour of day shows a weak, positive relationship with sentiment, suggesting that the attitudes of texters improve throughout the day or that dinner may simply be better than lunch. A similar progression 
                             is visible from Monday to Sunday, perhaps implying a morale boost across the week or a preference for mid-to-late week meals. Meanwhile, month shows a slightly negative relationship with message sentiment. For both semesters documented here, engagement with HUDS over text seems to peak in the first month or two and gradually decline over the latter half of the semester."),
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
                              # Tried to get a marquee going; I'll opted for a searchable table instead.
                              #mainPanel(renderCSS(type="text", loader=marquee_list)),
                              #marquee("<marquee>hello there! Hover over a word to see how many times it occurs across all messages </marquee>"),
                              h2("Text-and-Tell Word Cloud", align="left"),
                              h4("Hover over a word to see how many times it occurs across all messages!", align="left"),
                              br(),
                              column(11.5, align="center",
                              wordcloud2Output("wordPlot"), 
                              br(), br()),
                              column(12,
                                     p("This certainly gives us the impression that the messages are overwhelmingly positive. Following up on David Davidson's assertion of 98% positivity in ", em("Fifteen Minutes,"), "this data yields a positivity rate of 78% across all messages. However, there are far more (literal) false positives 
                                       than false negatives with the rsentiment method, so this is likely actually a bit lower. Regardless, students do seem to be mostly complimentary in their messages. And for what it's worth, I, as a vegetarian, can't help but smile that the tofu:sausage mention ratio is over 3.")
                                     ),
                              column(12, align="left",
                                     h2("Peruse & Search a Sample of Messages"),
                                     h4("Trust me, there are some gems in here."),
                                     br(),
                                     dataTableOutput("table"), br())
                              )))


server <- function(input, output) {
    
    # Adding two images for the About tab
    
    output$huds <- renderImage({
        list(
            src = './images/huds.png',
            contentType='image/png',
            width = 425,
            height = 278
        )}, deleteFile = F)
    
    output$food <- renderImage({
        list(
            src = './images/food.jpg',
            contentType='image/jpg',
            width = 425,
            height = 278
        )}, deleteFile = F)
    
    # GIF for house-by-house analysis
    
    output$percap <- renderImage({
      list(
        src = './images/house.gif',
        contentType = 'image/gif',
        width = 500,
        height = 400
      )
    }, deleteFile = F)
    
    # Timeline heat map
    
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
    
    # rendering interactive map with plotly 
    
    output$map_2 <- renderPlotly({
      
      map_2 <- ggplot(data = US, aes(x = long, y = lat)) + geom_polygon(fill="grey", aes(group = group)) +
        geom_point(data = to_plot, aes(x = longitude, y = latitude, name = Location, size = Messages, color=Sentiment, text = paste("State:", to_plot$Location, "<br>", "Sentiment:", to_plot$Sentiment %>% round(digits = 4), "<br>", "Message Count:", to_plot$Messages))) +
        xlim(-180, -50) + theme_classic() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                  axis.title.x=element_blank(),
                                                  axis.title.y=element_blank(),
                                                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                                  panel.grid.minor=element_blank(),plot.background=element_blank()) +
        scale_color_viridis(option = "B")
      
      ggplotly(map_2, tooltip = "text")
    })
    
    # Next two plots are exploring top four states' messaging counts
    
    output$top_four <- renderPlot({
      top_four$n <- top_four$n / c(944, 708, 944, 944)
      top_four %>%
        ggplot(aes(Location, n)) + 
        geom_col(fill = "#2bb673") + 
        labs(x = "Home State", y = "HUDS Messages Per Capita", title = "Messages Per Capita By Student Home State", subtitle = "For Top Four States") +
        theme_minimal()
      
    })
    
    output$ma_adjusted <- renderPlot({
      ma_adjusted %>%
        ggplot(aes(Location, n)) + geom_col(fill = "#f3b204") +
        labs(x = "Home State", y = "HUDS Messages Per Capita", title = "Messages Per Capita By Student Home State", subtitle = "Adjusting for International Students") +
        theme_minimal()
    })
    
    # Regressions
    
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
                )) 
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
    
    # Another woe-begotten attempt at using a CSS element
    
     output$scroll <- renderCSS({
       marquee(marquee_list, behavior = "scroll", direction = "left",
              scrollamount = 6, width = "100%")
     })
     
     # Renderin the word cloud
    
     output$wordPlot <- renderWordcloud2({
        
        wordcloud2(word_cloud, size = 1.3, color = rep_len(c("#f15b29", "#2bb673", "#BDD2FF", "#f3b204", "#F59187"), nrow(demoFreq)))
        
    })
     
     # Interactive table
     
    output$table <- renderDataTable(table, server=F)
}

# Run the application 
shinyApp(ui = ui, server = server)
