#Final Project analysis.

library("httr")
library("jsonlite")
library("dplyr")
library("stringr")
Trump_data <- read.csv(file = "Trump_Obama_Tweets/trump_tweets.csv", stringsAsFactors = FALSE)
Obama_data <- read.csv(file = "Trump_Obama_Tweets/obama_tweets.csv", stringsAsFactors = FALSE)

##3-1 
## Obama stats
Obama_data <- tail(Obama_data, -7)
Obama_tweet_number <- nrow(Obama_data)
Obama_max_retweet <- Obama_data %>% 
  filter(Retweets == max(Retweets)) %>% 
  select(Text, Date, Retweets)
Obama_min_retweet <- Obama_data %>% 
  filter(Retweets == min(Retweets)) %>% 
  select(Text, Date, Retweets)

Obama_max_favorite <- Obama_data %>% 
  filter(Favorites == max(Favorites)) %>% 
  select(Text, Date, Favorites)

Obama_min_favorite <- Obama_data %>% 
  filter(Favorites == min(Favorites)) %>% 
  select(Text, Date, Favorites)

Obama_average_retweets <- as.list(Obama_data %>% 
                                    summarise(
                                      mean(Retweets)
                                    ))

Obama_average_favorite <- as.list(Obama_data %>% 
                                    summarise(
                                      mean(Favorites)
                                    ))
## trump stats
Trump_tweet_number <- nrow(Trump_data)

Trump_max_retweet <- Trump_data %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  select(text, Date, retweet_count)

Trump_min_retweet <- Trump_data %>% 
  filter(retweet_count == min(retweet_count)) %>% 
  select(text, Date, retweet_count)

Trump_max_favorite <- Trump_data %>% 
  filter(favorite_count == max(favorite_count)) %>% 
  select(text, Date, favorite_count)

Trump_min_favorite <- Trump_data %>% 
  filter(favorite_count == min(favorite_count)) %>% 
  select(text, Date, favorite_count)

Trump_average_retweets <- as.list(Trump_data %>% 
                                    summarise(
                                      mean(retweet_count)
                                    ))

Trump_average_favorite <- as.list(Trump_data %>% 
                                    summarise(
                                      mean(favorite_count)
                                    ))

##3-2
##1. What does the general reception to the tweets, with a baseline of average likes and favorites/retweets between them, say about the president making them and the people liking them? Given that        Obama and Trump come from quite literally the opposite sides of the political spectrum, this should say a lot about what the reception of the US to each president and the general
##beliefs of the people in the country that we live in.

##2. How seriously is the president taking his job? That is, what percentage of the tweets are actually about issues/policy in the United States? What percentage are insults? etc.
##This is an obviously important question to ask - he's literally the leader of the US, it's important that a president focuses more on policy rather than the media.

##3. What are the different approaches to various topics have the Presidents in question taken? How does one approach Gun Control or Healthcare or Immigration over the other? It's incredibly
##important to know about this in order to understand the interests of the President and to get a look at the future of the country's laws.

##4. How often do the presidents interact with their followers? Do they retweet any messages from their followers? Do they directly reply to questions or stay ambivalent?


#3-3
#obama and donald average retweet and favorite

Obama_Trump_Average <- data.frame("Name" = c("Obama","Trump"), "Average_Retweets:" = c(Obama_average_retweets$`mean(Retweets)`,Trump_average_retweets$`mean(retweet_count)`) ,"Average_Favorite:" = c(Obama_average_favorite$`mean(Favorites)`, Trump_average_favorite$`mean(favorite_count)`), stringsAsFactors = FALSE)



# 3-4
## rate of I, My, I'm , appearing rate. 
## however can't combined data, and would be unecessary to combine them 
Trump_text_my <- Trump_data %>% 
  filter(grepl("My ", text))
Trump_text_I <- Trump_data %>% 
  filter(grepl("I ", text))

Trump_text_Im <- Trump_data %>% 
  filter(grepl("I'm ", text))

Trump_text_emotional <- Trump_data %>% 
  filter(grepl(" badly | weak | dumb | crazy ", text))

Trump_text_great <- Trump_data %>% 
  filter(grepl(" great ", text))



Obama_text_my <- Obama_data %>% 
  filter(grepl("My ", Text))

Obama_text_emotional <- Obama_data %>% 
  filter(grepl(" badly | weak | dumb | crazy ", Text))

Obama_text_I <- Obama_data %>% 
  filter(grepl("I ", Text))

Obama_text_Im <- Obama_data %>% 
  filter(grepl("I'm ", Text))

Obama_rate <- ((nrow(Obama_text_I) + nrow(Obama_text_my) + nrow(Obama_text_Im))/nrow(Obama_data))*100

Trump_rate <- ((nrow(Trump_text_I) + nrow(Trump_text_my) + nrow(Trump_text_Im))/nrow(Trump_data))*100

