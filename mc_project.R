# Clear the workspace
rm(list = ls())

# Load libraries
library(rtweet)
library(tidytext)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(wordcloud)
library(magrittr)  # Package needed for %>% to work correctly.
library(stringi)   # Package needed for stri_detect_regex function.
library(stringr)   # Package needed for functions str_detect and str_replace_all.  This package is built on top of package stringi.

# Read the saved tweet csv file.
mcdf <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)

# Load stop_words dataset, typically extremely common words such as 
# "the", "of", "to", and so forth
data(stop_words)

# Break the text into individual tokens, remove unwanted text and remove stop words 
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
mcdf$text <- as.character(mcdf$text)
tidy_tweets <- mcdf %>%
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
    filter(!word %in% stop_words$word, 
                  str_detect(word, "[a-z]"))

# ===============================
# Perform sentiment analysis
# ===============================
# Count the most common positive words in the tweets
nrcpositive <- get_sentiments("nrc") %>% 
    filter(sentiment == "positive")

positive_words <- tidy_tweets %>%
    inner_join(nrcpositive) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common positive words in the tweets
mcp <- positive_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    labs(y="Count",
         x="Positive Words",
         title="The 30 Most Common Positive Words in Tweets")

# Save the plot of the 30 most common positive words in tweets
ggsave(filename = "common_pos_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common joy words in the tweets
nrcjoy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

joy_words <- tidy_tweets %>%
    inner_join(nrcjoy) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common joy words in the tweets
mcp <- joy_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Joy Words",
         title="The 30 Most Common Joy Words in Tweets")

# Save the plot of the 30 most common joy words in tweets
ggsave(filename = "common_joy_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common trust words in the tweets
nrctrust <- get_sentiments("nrc") %>% 
    filter(sentiment == "trust")

trust_words <- tidy_tweets %>%
    inner_join(nrctrust) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common trust words in the tweets
mcp <- trust_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Trust Words",
         title="The 30 Most Common Trust Words in Tweets")

# Save the plot of the 30 most common trust words in tweets
ggsave(filename = "common_trust_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common negative words in the tweets
nrcnegative <- get_sentiments("nrc") %>%
    filter(sentiment == "negative")

negative_words <- tidy_tweets %>%
    inner_join(nrcnegative) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common negative words in the tweets
mcp <- negative_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    labs(y="Count",
         x="Negative Words",
         title="The 30 Most Common Negative Words in Tweets")

# Save the plot of the 30 most common negative words in tweets
ggsave(filename = "common_neg_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common disgust words in the tweets
nrcdisgust <- get_sentiments("nrc") %>%
    filter(sentiment == "disgust")

disgust_words <- tidy_tweets %>%
    inner_join(nrcdisgust) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common disgust words in the tweets
mcp <- disgust_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Disgust Words",
         title="The 30 Most Common Disgust Words in Tweets")

# Save the plot of the 30 most common disgust words in tweets
ggsave(filename = "common_disgust_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Count the most common anger words in the tweets
nrcanger <- get_sentiments("nrc") %>%
    filter(sentiment == "anger")

anger_words <- tidy_tweets %>%
    inner_join(nrcanger) %>%
    count(word, sort = TRUE)

# Plot a bar graph of the 30 most common anger words in the tweets
mcp <- anger_words %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(y="Count",
         x="Anger Words",
         title="The 30 Most Common Anger Words in Tweets")

# Save the plot of the 30 most common anger words in tweets
ggsave(filename = "common_anger_words.png", plot = mcp, width = 6, height = 4,
       dpi = 600)
# =========================
# End of sentiment analysis
# =========================

# Get min created_at date and max created_at date of the data
mindate <- min(mcdf$created_at)
mindate <- substr(mindate, 1, 10)
maxdate <- max(mcdf$created_at)
maxdate <- substr(maxdate, 1, 10)

# Count source of tweets
source_count <- as.data.frame(table(mcdf$source))

# Rename source_count columns
names(source_count)[names(source_count) == "Var1"] <- "tweet_source"
names(source_count)[names(source_count) == "Freq"] <- "n"

# Plot a bar graph of the top 30 sources of deere tweets
mcp <- source_count %>%
    top_n(30) %>%
    mutate(tweet_source = reorder(tweet_source, n)) %>%
    ggplot(aes(x = tweet_source, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    theme(axis.title = element_text(size = 8)) +                  
    theme(axis.text = element_text(size = 6)) +                   
    labs(y="Count",
         x="Source of Tweets",
         title="Top 30 Sources of Tweets")

# Save the plot of the top 30 sources of deere tweets
ggsave(filename = "tweet_source.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Coerce created_at from character to POSIXct class
mcdf$created_at <- as.POSIXct(mcdf$created_at)

# Break records into 4 hour time blocks based on created_at values
# Add time_block column to store the time block of the tweets
mcdf$time_block <- NA

i <- 0
for(i in 1:nrow(mcdf)){
    if(substr(mcdf$created_at[i], 12,19) >= "00:00:00" & substr(mcdf$created_at[i], 12,19) < "04:00:00"){
        mcdf$time_block[i] <- 1
    } else{
        if(substr(mcdf$created_at[i], 12,19) >= "04:00:00" & substr(mcdf$created_at[i], 12,19) < "08:00:00"){
            mcdf$time_block[i] <- 2
        }else{
            if(substr(mcdf$created_at[i], 12,19) >= "08:00:00" & substr(mcdf$created_at[i], 12,19) < "12:00:00"){
                mcdf$time_block[i] <- 3
            }else{
                if(substr(mcdf$created_at[i], 12,19) >= "12:00:00" & substr(mcdf$created_at[i], 12,19) < "16:00:00"){
                    mcdf$time_block[i] <- 4
                }else{
                    if(substr(mcdf$created_at[i], 12,19) >= "16:00:00" & substr(mcdf$created_at[i], 12,19) < "20:00:00"){
                        mcdf$time_block[i] <- 5
                    }else{
                        mcdf$time_block[i] <- 6
                    }
                }
            }
        }
    }
    i <- i + 1
}

# Coerce time_block to factor
mcdf$time_block <- as.factor(mcdf$time_block)

# Change levels of time_block
levels(mcdf$time_block) <- c("12:00 AM - 3:59 AM", "4:00 AM - 7:59 AM", "8:00 AM - 11:59 AM",
                             "12:00 PM - 3:59 PM", "4:00 PM - 7:59 PM", "8:00 PM - 11:59 PM")

# Plot the time blocks of the tweets
mcp <- qplot(time_block, data = mcdf, geom = "bar", fill = time_block)
mcp <- mcp + scale_fill_brewer(palette = "Set1")
plot_title <- paste("Time of Day Tweets Were Created", "(", mindate, "to", maxdate, ")", sep = " ")
mcp <- mcp + ggtitle(plot_title)                                          # Set plot title
mcp <- mcp + theme(plot.title = element_text(size = 10, face = "bold"))   # Change plot title theme 
mcp <- mcp + theme(axis.title = element_text(size = 8))                   # Change both axis' title font size
mcp <- mcp + theme(axis.text = element_text(size = 6))                    # Change both axis' text font size
mcp <- mcp + theme(axis.title.x = element_blank())                        # Remove x axis label
mcp <- mcp + ylab("Tweet Count")                                          # Set y axis label
mcp <- mcp + theme(legend.position = "none")                              # Remove legend

# Save plot of the time block of the tweets
ggsave(filename = "tweet_time_block.png", plot = mcp, width = 6, height = 4,
       dpi = 600)

# Add day_tweeted column to indicate the day of the week of the tweet
mcdf$day_tweeted <- as.factor(weekdays(mcdf$created_at))

# Reorder levels of day_tweeted
mcdf$day_tweeted <- factor(mcdf$day_tweeted, levels = c("Sunday", "Monday",
                                                "Tuesday", "Wednesday",
                                                "Thursday", "Friday",
                                                "Saturday"))

# Function to take a date range supplied by the user and plot a bar graph of the count of tweets 
# for the days of the week in that date range
tweets.per.day <- function(start_date = mindate, end_date = maxdate){
    # Subset data to date range specified by the user
    daterange_tweets <- subset(mcdf, date(mcdf$created_at) >= start_date & date(mcdf$created_at) <= end_date)
    
    # Plot tweets per day of the week.
    mcp <- qplot(day_tweeted, data = daterange_tweets, geom = "bar", fill = day_tweeted)
    mcp <- mcp + scale_fill_brewer(palette = "Set2")
    plot_title <- paste("Tweets per Day of the Week", "(", start_date, "to", end_date, ")", sep = " ")
    mcp <- mcp + ggtitle(plot_title)                                          # Set plot title
    mcp <- mcp + theme(plot.title = element_text(size = 10, face = "bold"))   # Change plot title theme 
    mcp <- mcp + theme(axis.title = element_text(size = 8))                   # Change both axis' title font size
    mcp <- mcp + theme(axis.text = element_text(size = 6))                    # Change both axis' text font size
    mcp <- mcp + theme(axis.title.x = element_blank())                        # Remove x axis label
    mcp <- mcp + ylab("Tweet Count")                                          # Set y axis label
    mcp <- mcp + theme(legend.position = "none")                              # Remove legend
    
    # Save tweets per day of the week plot
    ggsave(filename = "tweets_per_day.png", plot = mcp, width = 6, height = 4,
           dpi = 600)   

    # Return the plot object
    return = mcp
}



