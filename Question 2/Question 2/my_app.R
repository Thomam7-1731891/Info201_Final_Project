# Question 2 for Final Project

library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")
library("tidyr")

Trump_data <- read.csv(file = "Trump_Obama_Tweets/trump_tweets.csv", stringsAsFactors = FALSE)
Obama_data <- read.csv(file = "Trump_Obama_Tweets/obama_tweets.csv", stringsAsFactors = FALSE)

ui <- fluidPage( 
  
  titlePanel("Domestic Policies"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Trump_policy",
        label = "Select Policy For Trump",
        choices = c("Immigration" = "immigration",
                    "Taxes" = "tax",
                    "Healthcare" = "healthcare",
                    "Gun control" = "gun")),
      br(),
      
      selectInput(
        inputId = "Obama_policy",
        label = "Select Policy For Obama",
        choices = c("Immigration" = "immigration",
                    "Taxes" = "tax",
                    "Healthcare" = "Obamacare",
                    "Gun control" = "gun"))

    ),
    mainPanel(
      h2("Choose either a data visualization or table of the data"),
      p("Q: How often do the presidents address important domestic policies on Twitter? 
        By looking for the number of times they address certain issues such as Taxes, Healthcare, and Immigration, 
        we can compare those numbers to the total number of their tweets to see how often they talk about these issues."),
      p("Summary: Both presidents do not address each individual policy that often in relative comparison to their total tweets.
        However, Obama seems to address each policy far more than Trump does. Yet it is important to also keep in mind that Trump has about a
        third of the total number of tweets Obama has due to a shorter current tenure in office."),
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", fluidRow(
                    column(width = 10, plotlyOutput(outputId = "Trump_plot")),
                    column(width = 10, plotlyOutput(outputId = "Obama_plot"))
                  )),
                  tabPanel("Trump_Table", tableOutput("table")),
                  tabPanel("Obama_Table", tableOutput("Obama_table"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$Trump_plot <- renderPlotly({
  
    Trump_text_filtered <- Trump_data %>% 
      filter(grepl(input$Trump_policy, text))
    
    policy_tweets <- nrow(Trump_text_filtered)
    other_tweets <- nrow(Trump_data) - policy_tweets
    
    format_data <- mutate(
      Trump_data,
      policy_tweets,
      other_tweets
    ) %>% 
      select(policy_tweets, other_tweets) %>%
      gather(
        key = category,
        value = value
      ) 
    
    unique_data <- unique(format_data)
    
    
    ggplot(data = unique_data) +
      geom_col(mapping = aes(x = category, y = value, fill = category))  +
      ggtitle(paste("Number of Tweets related to", input$Trump_policy, "Vs Total Number of Tweets")) +
      labs(y = "Number of Tweets") + 
      scale_y_log10()
    
  })
  
  output$Obama_plot <- renderPlotly({
    
    Obama_text_filtered <- Obama_data %>% 
      filter(grepl(input$Obama_policy, Text))
    
    policy_tweets <- nrow(Obama_text_filtered)
    other_tweets <- nrow(Obama_data) - policy_tweets
    
    format_data <- mutate(
      Obama_data,
      policy_tweets,
      other_tweets
    ) %>% 
      select(policy_tweets, other_tweets) %>%
      gather(
        key = category,
        value = value
      ) 
    
    unique_data <- unique(format_data)
    
    
    ggplot(data = unique_data) +
      geom_col(mapping = aes(x = category, y = value, fill = category))  +
      ggtitle(paste("Number of Tweets related to", input$Obama_policy, "Vs Total Number of Tweets")) +
      labs(y = "Number of Tweets") + 
      scale_y_log10()
    
  })
  
  output$table <- renderTable({
  
    Trump_text_filter <- Trump_data %>%
      filter(grepl(input$Trump_policy, text)) %>%
      select(text, Date, favorite_count, retweet_count) %>%
      rename("Text" = text, "Favorites" = favorite_count, "Retweets" = retweet_count)
    
  })
  
  output$Obama_table <- renderTable({
    
    Obama_text_filter <- Obama_data %>%
      filter(grepl(input$Obama_policy, Text)) %>%
      select(Text, Date, Favorites, Retweets)
    
  })
  
}

shinyApp(ui = ui, server = server)





