library("shiny")
library("dplyr")
library("plotly")
library("tidyr")

#create page
shinyUI(fluidPage(
  #application title
  titlePanel("Twitter follower reception on presidents addressing policies"),
    sidebarLayout(
      sidebarPanel(
        selectInput("policy",
                    "Select a policy:",
                    c("Immigration", "Tax", "Healthcare", "Gun Control")
        ),
        tags$b(textOutput(outputId = "president_message")),
  
        radioButtons("president",
                     "",
                     c("Donald Trump", "Barack Obama")
        )
      ),
      #create mainPanel with data table and caption
      mainPanel(
        tabsetPanel(
        tabPanel("Plot",
                 p("What does the general reception to the tweets, with a baseline of average likes and favorites/retweets between them, say about the president making them and the people liking them? Given that Obama and Trump come from quite literally the opposite sides of the political spectrum, 
                   this should say a lot about what the reception of the US to each president and the general beliefs of the people in the country that we live in."),
                 p("Summary: When the presidents address policies in their tweets, Trump's audience appears to be more responsive to them in both
                   favorites and retweets in comparison to Obama's audience as shown in the bar graphs below."),
                 fluidRow(
          column(width = 10, plotlyOutput(outputId = "plot_trump")),
          column(width = 10, plotlyOutput(outputId = "plot_obama"))
          )),
        tabPanel("Table", h3(textOutput(outputId = "table_message")),
        dataTableOutput(outputId = "table"))
        )
      )
    )
  )
)



