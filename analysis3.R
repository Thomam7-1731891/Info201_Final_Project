library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("plotly")
library("scales")

Trump_data <- read.csv(file = "Trump_Obama_Tweets/trump_tweets.csv", stringsAsFactors = FALSE)
Obama_data <- read.csv(file = "Trump_Obama_Tweets/obama_tweets.csv", stringsAsFactors = FALSE)

Year_Trump_data <- mutate(Trump_data, Year = substr(Date, 1, 4)) # Add Year column
Year_Obama_data <- mutate(Obama_data, Year = substr(Date, 1, 4))

colnames(Year_Obama_data)[1] <- "text" # Allows for methods to be generalized


# Trump data by year
trumpDataByYear <- function(year) {
  data <- filter(Year_Trump_data, Year == year)
}

# Obama data by year
obamaDataByYear <- function(year) {
  data <- filter(Year_Obama_data, Year == year)
}

percentOfRetweetsInYearObama <- function(year) {
  data <- obamaDataByYear(year)
  count <- length(grep("@", data$text))
  percent <- (count / nrow(data))
}

percentOfRetweetsInYearTrump <- function(year) {
  data <- trumpDataByYear(year)
  count <- length(grep("@", data$text))
  percent <- (count / nrow(data))
}

# Creates a data frame for a pie chart based on retweets from Trump
pieChartMakerTrump <- function(year) {
  retweets <- round(percentOfRetweetsInYearTrump(year) * 100)
  normalTweets <- 100 - retweets
  df <- data.frame(
    names = c("Retweets", "Normal Tweets"),
    values = c(retweets, normalTweets)
  )
}

# Creates a data frame for a pie chart based on retweets from Obama
pieChartMakerObama <- function(year) {
  retweets <- round(percentOfRetweetsInYearObama(year) * 100)
  normalTweets <- 100 - retweets
  df <- data.frame(
    names = c("Retweets", "Normal Tweets"),
    values = c(retweets, normalTweets)
  )
}

# Creates a Pie Chart with Labels based on retweets and year for Trump
pieChartPlotterTrump <- function(year) {
  chart <- ggplot(pieChartMakerTrump(year), aes(x = "", y = values, fill = names)) +
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + guides(fill = guide_legend(title = "Type of Tweet")) +
    xlab("") + ylab("Percentage of Retweets") + scale_fill_manual(values = c("#00acee", "#ff5311")) + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank()) + 
    geom_text(aes(y = values/3 + c(0, cumsum(values)[-length(values)]), label = percent(values/100)), size=5)
  chart
}

# Creates a Pie Chart with Labels based on retweets and year for Obama
pieChartPlotterObama <- function(year) {
  chart <- ggplot(pieChartMakerObama(year), aes(x = "", y = values, fill = names)) +
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + guides(fill = guide_legend(title = "Type of Tweet")) +
    xlab("") + ylab("Percentage of Retweets") + scale_fill_manual(values = c("#00acee", "#ff5311")) + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank()) + 
    geom_text(aes(y = values/3 + c(0, cumsum(values)[-length(values)]), label = percent(values/100)), size=5)
  chart
}
