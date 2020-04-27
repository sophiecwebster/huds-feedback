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

# Load in RDS files

word_cloud <- readRDS("./rds_files/word-cloud.RDS")
heat <- readRDS("./rds_files/times.RDS")
house_names <- readRDS("./rds_files/house_names.RDS")
US <- readRDS("./rds_files/US.RDS")
map_1 <- readRDS("./rds_files/map.RDS")
comments <- readRDS("./rds_files/comments.RDS")
top_four <- readRDS("./rds_files/top-four.RDS")

#marquee_list <- list(marquee(comments))


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
                                     mainPanel(width = 12, imageOutput("map")))),
                              
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
                 tabPanel("Sentiment Analysis"),
                 tabPanel("Browse Messages",
                          fluidPage(
                              # Trying to get a marquee going; I'll figure this out later.
                              # mainPanel(renderCSS(type="text", loader=marquee_list)),
                              h2("Text-and-Tell Word Cloud", align="left"),
                              h4("Hover over a word to see how many times it occurs across all messages!", align="left"),
                              br(),
                              column(12, align="center",
                              wordcloud2Output("wordPlot"), class = 'rightAlign'
                              ))))


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
        height = 420
      )}, deleteFile = F)
    
    output$top_four <- renderPlot({
      top_four$n <- top_four$n / c(944, 708, 944, 944)
      top_four %>%
        ggplot(aes(Location, n)) + 
        geom_col(fill = "#2bb673") + 
        labs(x = "Home State", y = "HUDS Messages Per Capita", title = "Messages Per Capita By Student Home State", subtitle = "For Top Four States") +
        theme_minimal()
      
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
    
    # output$scroll <- renderCSS({
    #   marquee(comments, behavior = "scroll", direction = "left",
    #           scrollamount = 6, width = "100%")
    # })
    
     output$wordPlot <- renderWordcloud2({
        
        wordcloud2(word_cloud, size = 1.3, color = rep_len(c("#f15b29", "#2bb673", "#BDD2FF", "#f3b204", "#F59187"), nrow(demoFreq)))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
