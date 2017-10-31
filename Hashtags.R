library(dplyr)
library(tidytext)
library(ggplot2)
library(ggraph)
library(igraph)
library(scales)
library(reshape2)

#################################################################################################
#################################################################################################

rm(list = ls())   #Clear Environment

#################################################################################################
# This code reads the older dataset                                                             #
#################################################################################################
tweet <- read.csv("deerestats.csv", stringsAsFactors = FALSE)


tweet <- subset(tweet,tweet$hashtags != "NA")

tweet <- subset(tweet,select = "hashtags")

tweet <- strsplit(as.character(tolower(tweet$hashtags)),' ')

tweet <- data.frame(unlist(tweet),stringsAsFactors=FALSE)

tweet %>%
      count(unlist.tweet., sort=TRUE) %>%
      top_n(30) %>%
      mutate(unlist.tweet. = reorder(unlist.tweet., n)) %>%
      ggplot(aes(x = unlist.tweet., y = n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip() +
      labs(x="Unique Hashtags",y="Count",title="Count of Hashtags Found in Tweets")

ggsave(filename = "UniqueHashtagsOld.png", plot = last_plot(), width = 6, height = 4, dpi = 600)

#################################################################################################
# This code reads the newer dataset                                                             #
#################################################################################################

tweet1 <- read.csv("deerestatsNew.csv", stringsAsFactors = FALSE)


tweet1 <- subset(tweet1,tweet1$hashtags != "NA")

tweet1 <- subset(tweet1,select = "hashtags")

tweet1 <- strsplit(as.character(tolower(tweet1$hashtags)),' ')

tweet1 <- data.frame(unlist(tweet1),stringsAsFactors=FALSE)

tweet1 %>%
  count(unlist.tweet1., sort=TRUE) %>%
  top_n(30) %>%
  mutate(unlist.tweet1. = reorder(unlist.tweet1., n)) %>%
  ggplot(aes(x = unlist.tweet1., y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x="Unique Hashtags",y="Count",title="Count of Hashtags Found in Tweets")

ggsave(filename = "UniqueHashtagsNew.png", plot = last_plot(), width = 6, height = 4, dpi = 600)

#################################################################################################
# This code reads both dataset together                                                         #
#################################################################################################
tweet2 <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)


tweet2 <- subset(tweet2,tweet2$hashtags != "NA")

tweet2 <- subset(tweet2,select = "hashtags")

tweet2 <- strsplit(as.character(tolower(tweet2$hashtags)),' ')

tweet2 <- data.frame(unlist(tweet2),stringsAsFactors=FALSE)

tweet2 %>%
  count(unlist.tweet2., sort=TRUE) %>%
  top_n(30) %>%
  mutate(unlist.tweet2. = reorder(unlist.tweet2., n)) %>%
  ggplot(aes(x = unlist.tweet2., y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x="Unique Hashtags",y="Count",title="Count of Hashtags Found in Tweets")

ggsave(filename = "UniqueHashtagsCombined.png", plot = last_plot(), width = 6, height = 4, dpi = 600)

#################################################################################################
#################################################################################################
