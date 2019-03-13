library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("plotly")
source("wordingAnalysis.R")
#wrangle data, average number of likes and favorites made by
Trump_data <- read.csv(file = "Trump_Obama_Tweets/trump_tweets.csv", stringsAsFactors = FALSE)

Trump_text_immigration <- Trump_data %>% 
  filter(grepl("immigration", text)) %>% 
  select(text, favorite_count, retweet_count)
Trump_mean_immigration <- Trump_text_immigration %>% 
  summarise(mean_favorites = mean(favorite_count), mean_retweets = mean(retweet_count)) %>% 
  mutate(total_mean_favorites = Trump_average_favorite, total_mean_retweets = Trump_average_retweets) %>% 
  gather(key = category, value = value)

Trump_text_tax <- Trump_data %>%
  filter(grepl("tax", text)) %>% 
  select(text, favorite_count, retweet_count)
Trump_mean_tax <- Trump_text_tax %>%
  summarise(mean_favorites = mean(favorite_count), mean_retweets = mean(retweet_count)) %>%
  mutate(total_mean_favorites = Trump_average_favorite, total_mean_retweets = Trump_average_retweets) %>%
  gather(key = category, value = value)

Trump_text_healthcare <- Trump_data %>%
  filter(grepl("healthcare", text)) %>% 
  select(text, favorite_count, retweet_count)
Trump_mean_healthcare <- Trump_text_healthcare %>%
  summarise(mean_favorites = mean(favorite_count), mean_retweets = mean(retweet_count)) %>%
  mutate(total_mean_favorites = Trump_average_favorite, total_mean_retweets = Trump_average_retweets) %>%
  gather(key = category, value = value)

Trump_text_gun_control <- Trump_data %>%
  filter(grepl("gun", text)) %>% 
  select(text, favorite_count, retweet_count)
Trump_mean_gun_control <- Trump_text_gun_control %>%
  summarise(mean_favorites = mean(favorite_count), mean_retweets = mean(retweet_count)) %>%
  mutate(total_mean_favorites = Trump_average_favorite, total_mean_retweets = Trump_average_retweets) %>%
  gather(key = category, value = value)

Trump_text <- Trump_text_tax
Trump_text <- full_join(Trump_text, Trump_text_healthcare)
Trump_text <- full_join(Trump_text, Trump_text_healthcare) %>% 
  select(text, favorite_count, retweet_count)

Obama_data <- read.csv(file = "Trump_Obama_Tweets/obama_tweets.csv", stringsAsFactors = FALSE)

Obama_text_immigration <- Obama_data %>%
  filter(grepl("Immigration", Text)) %>% 
  select(Text, Favorites, Retweets)

Obama_mean_immigration <- Obama_text_immigration %>%
  summarise(mean_favorites = mean(Favorites), mean_retweets = mean(Retweets)) %>%
  mutate(total_mean_favorites = Obama_average_favorite, total_mean_retweets = Obama_average_retweets) %>%
  gather(key = category, value = value) 

Obama_text_taxes <- Obama_data %>%
  filter(grepl("tax", Text)) %>% 
  select(Text, Favorites, Retweets)

Obama_mean_taxes <- Obama_text_taxes %>%
  summarise(mean_favorites = mean(Favorites), mean_retweets = mean(Retweets)) %>%
  mutate(total_mean_favorites = Obama_average_favorite, total_mean_retweets = Obama_average_retweets) %>%
  gather(key = category, value = value)

Obama_text_healthcare <- Obama_data %>%
  filter(grepl("Obamacare", Text)) %>% 
  select(Text, Favorites, Retweets)

Obama_mean_healthcare <- Obama_text_healthcare %>%
  summarise(mean_favorites = mean(Favorites), mean_retweets = mean(Retweets)) %>%
  mutate(total_mean_favorites = Obama_average_favorite, total_mean_retweets = Obama_average_retweets) %>%
  gather(key = category, value = value)

Obama_text_gun_control <- Obama_data %>%
  filter(grepl("gun", Text)) %>% 
  select(Text, Favorites, Retweets)

Obama_mean_gun_control <- Obama_text_gun_control %>%
  summarise(mean_favorites = mean(Favorites), mean_retweets = mean(Retweets)) %>%
  mutate(total_mean_favorites = Obama_average_favorite, total_mean_retweets = Obama_average_retweets) %>%
  gather(key = category, value = value)

Obama_text <- full_join(Obama_text_immigration, Obama_text_taxes)
Obama_text <- full_join(Obama_text, Obama_text_healthcare)
Obama_text <- full_join(Obama_text, Obama_text_gun_control)