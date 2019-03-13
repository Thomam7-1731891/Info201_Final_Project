###
### bar chart?
### checkbox for the bar chart
## assign each emotionals words in different bins
## choose of obama and trump in drop down list 

## How often do the presidents interact with the public over Twitter? Do they retweet any messages from their followers?
## Do they directly reply to questions? We can interpret how engaged the presidents are by finding the percentages of tweets that are retweets,
## responses, in comparison to the amount of all tweets, and then compare the two presidents with each other.
library("httr")
library("jsonlite")
library("dplyr")
library("stringr")
library(shiny)
library(ggplot2)
library(ggmap)
options(scipen = 999)
Trump_data <- read.csv(file = "Trump_Obama_Tweets/trump_tweets.csv", stringsAsFactors = FALSE)
Obama_data <- read.csv(file = "Trump_Obama_Tweets/obama_tweets.csv", stringsAsFactors = FALSE)
View(Obama_data)
Obama_data <- tail(Obama_data, -7)
View(Obama_retweet)

trump_retweet <- Trump_data %>% 
  filter(is_retweet == "TRUE")



Obama_retweet <- Obama_data %>% 
  filter(grepl(" @", Text))

Obama_retweet <- Obama_retweet %>% 
  mutate(
    Date = substr(Date, 0, 4)
  ) %>% 
  rename(
    Obama_text = "Text",
    amount_Retweets = "Retweets",
    amount_Favorites =  "Favorites"
  ) %>% 
  select(
    c("Obama_text", "Date", "amount_Favorites", "amount_Retweets")
  )

trump_retweet <- trump_retweet %>% 
  mutate(
    Date = substr(Date, 0, 4)
  ) %>% 
  rename(
    Trump_text = "text",
    amount_Retweets = "retweet_count",
    amount_Favorites =  "favorite_count"
  ) %>% 
  select(
    c("Trump_text", "Date", "amount_Favorites", "amount_Retweets")
  )

View(trump_retweet)

##Trump_Obama_retweets <- full_join(Obama_retweet, trump_retweet, by = "x")
##unique(Trump_Obama_retweets)
##View(Trump_Obama_retweets)


select_values <- c("2012","2013","2014","2015","2016","2017", "2018")

ui <- fluidPage(
  titlePanel(title = h4("Hillsborough County Population by Census", align="center")),
  
  sidebarLayout(
    sidebarPanel(
   # checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                      # choices = list("amount_Retweets" = 1, "amount_Favorites" = 2),
                      # selected = 1),
    hr(),
    selectInput(inputId = "years", label = "years of choice", choices = select_values, selected = "2017"),
    fluidRow(column(2, verbatimTextOutput("value")))
    ),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Trump", plotOutput("plot",height = 400), tableOutput("table"),textOutput("text")),
                tabPanel("Obama", plotOutput("plot2",height = 500),tableOutput("table2"), textOutput("text2"))
                )
  ) 
)
)

server <- function(input,output){
  
  output$plot <- renderPlot({
    filtered_trump_retweet <- trump_retweet %>% 
      filter(Date == input$years)
    ##filtered_trump_retweet$Date <- as.numeric(filtered_trump_retweet$years)
    
    Trump_plot <- ggplot(data = filtered_trump_retweet) +
      geom_point(mapping = aes_string(x = "amount_Favorites", y = "amount_Retweets", color=shQuote("one"))) +
      scale_color_manual(name="color", values= (one ="#105B63"))+
      labs(x = "amount_Favorites", y = "amount_Retweets", title = "Amount of favorite/ Amount of retweets by the followers") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 10, size = 8))
    Trump_plot
  })
  
  output$plot2 <- renderPlot({
    filtered_Obama_retweet <- Obama_retweet %>% 
      filter(Date == input$years)
    ##filtered_trump_retweet$Date <- as.numeric(filtered_trump_retweet$years)
    
    Obama_plot <- ggplot(data = filtered_Obama_retweet) +
      geom_point(mapping = aes_string(x = "amount_Favorites", y = "amount_Retweets", color=shQuote("one"))) +
      scale_color_manual(name="color", values= (one ="#105B63"))+
      labs(x = "amount_Favorites", y = "amount_Retweets", title = "Amount of favorite/ Amount of retweets by the followers") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 5, size = 8))
    Obama_plot
  })
  
  
  output$table <- renderTable({
    table <- trump_retweet%>% 
      select(Date, Trump_text,input$checkgroup) %>% 
      filter(Date == input$years) %>% 
      top_n(20)
    table
  })
  
  output$table2 <- renderTable({
    table2 <- Obama_retweet%>% 
      select(Date, Obama_text, input$checkgroup) %>% 
      filter(Date == input$years) %>% 
      top_n(20)
    table2
  })
} 
shinyApp(ui,server)
