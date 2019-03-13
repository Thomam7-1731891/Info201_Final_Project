library("tidytext")
library("dplyr")
library("ggplot2")
library("plotly")

Trump_data <- read.csv(file = "Trump_Obama_Tweets/trump_tweets.csv", stringsAsFactors = FALSE)
Obama_data <- read.csv(file = "Trump_Obama_Tweets/obama_tweets.csv", stringsAsFactors = FALSE)

Trump_data <- mutate(Trump_data, Year = substr(Date, 1, 4)) # Add Year column
Obama_data <- mutate(Obama_data, Year = substr(Date, 1, 4))

colnames(Obama_data)[1] <- "text" # Allows for methods to be generalized

# Emotion index based on NRC that's been pre-prepared for reasons listed below.
trump_emotion <- read.csv(file = "Trump_Obama_Tweets/trump_emotions.csv", stringsAsFactors = FALSE)
obama_emotion <- read.csv(file = "Trump_Obama_Tweets/obama_emotions.csv", stringsAsFactors = FALSE)


# NRC Lexicon of Words and their Emotional meaning from the tidytext package
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

# Trump data by year
trumpDataByYear <- function(year) {
  data <- filter(Trump_data, Year == year)
}

# Obama data by year
obamaDataByYear <- function(year) {
  data <- filter(Obama_data, Year == year)
}

# Returns a count of emotional words in the 'text' column of a data frame based on the NRC Lexicon
emotionCount <- function(df) {
  text <- df$text
  customNRC <- nrc
  wordCount <- sapply(nrc$word, function(word) { length(grep(word, text)) })
  customNRC$occurence <- wordCount
  customNRC <- select(customNRC, sentiment, occurence)
  customNRC <- group_by(customNRC, sentiment) %>%
    summarise(sum = sum(occurence))
  customNRC
}

# Returns a subsection of the emotion index based on year
emotionTrumpDataByYear <- function(year) {
  data <- filter(trump_emotion, Year == year)
}

# Returns a subsection of the emotion index based on year
emotionObamaDataByYear <- function(year) {
  data <- filter(obama_emotion, Year == year)
}


# This is code used to generate the data frame used to display emotion levels in Trump's tweets.
# They have been pre-prepared and saved in a .csv file due to the sheer size of the data set causing
# the actual time needed to generate each emotion dataframe to bloat to about 40 seconds. This is incredibly undesirable,
# as it would mean the page would take a solid minute to load its graphic.
# trumpEmotion2017 <- trumpDataByYear(2017) %>%
#   emotionCount()
# trumpEmotion2017$Year <- 2017
# 
# trumpEmotion2018 <- trumpDataByYear(2018) %>%
#   emotionCount()
# trumpEmotion2018$Year <- 2018
# 
# trump_emotion <- rbind(trumpEmotion2017, trumpEmotion2018)
# write.csv(trump_emotion, "trump_emotions.csv")


# This is code used to generate the data frame used to display emotion levels in Obama's tweets.
# They have been pre-prepared and saved in a .csv file due to the sheer size of the data set causing
# the actual time needed to generate each emotion dataframe to bloat to about 40 seconds. This is incredibly undesirable,
# as it would mean the page would take a solid minute to load its graphic.
# obamaEmotion2012 <- obamaDataByYear(2012) %>%
#   emotionCount()
# obamaEmotion2012$Year <- 2012
# 
# obamaEmotion2013 <- obamaDataByYear(2013) %>%
#   emotionCount()
# obamaEmotion2013$Year <- 2013
# 
# obamaEmotion2014 <- obamaDataByYear(2014) %>%
#   emotionCount()
# obamaEmotion2014$Year <- 2014
# 
# obamaEmotion2015 <- obamaDataByYear(2015) %>%
#   emotionCount()
# obamaEmotion2015$Year <- 2015
# 
# obamaEmotion2016 <- obamaDataByYear(2016) %>%
#   emotionCount()
# obamaEmotion2016$Year <- 2016
# 
# obamaEmotion2017 <- obamaDataByYear(2017) %>%
#   emotionCount()
# obamaEmotion2017$Year <- 2017
# 
# obama_emotion <- rbind(obamaEmotion2012, obamaEmotion2013) %>%
#   rbind(obamaEmotion2014) %>%
#   rbind(obamaEmotion2015) %>%
#   rbind(obamaEmotion2016) %>%
#   rbind(obamaEmotion2017)
# 
# write.csv(obama_emotion, "obama_emotions.csv")
