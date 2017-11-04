rm(list = ls())

#libraries note we have not used a lot of these in class so you'll have to install them
library(ggraph)
library(igraph)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rtweet)
library(openssl)
library(httr)
library(widyr)

#Read in Data Set
deerestatsNew <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)
#deerestatsNew <- search_tweets("Deere", n = 10000, include_rts = FALSE)


#hyperlinks aren't works so lets get rid of them
deerestatsNew$stripped_text <- gsub("http.*","",  deerestatsNew$text)
deerestatsNew$stripped_text <- gsub("https.*","", deerestatsNew$stripped_text)

#strip and clean
deerestats_cleanNew <- deerestatsNew %>%
         dplyr::select(stripped_text) %>%
         unnest_tokens(word,stripped_text)



#john and deere are going to appear a lot...duh... get rid of them
#deerestats_cleanNew <- subset(deerestats_cleanNew, word != "deere")
#deerestats_cleanNew <- subset(deerestats_cleanNew, word != "john")

#make stop_words
data("stop_words")
cleaned_tweet_wordsNew <- deerestats_cleanNew %>%
    anti_join(stop_words)

#remove punctuations and convert to lowercase, also add id for tweets
deere_tweets_paired_words <- deerestatsNew %>% 
    dplyr::select(stripped_text) %>% unnest_tokens(paired_words, stripped_text, token = "ngrams", n=2)

#count the number of paired words
deere_tweets_paired_words %>% count(paired_words, sort = TRUE)

#pair the words together
deere_tweets_separated_words <- deere_tweets_paired_words %>%
    separate(paired_words, c("word1", "word2"), sep = " ")

#John and Deere are going to be in sequence alot, obviously. This will change all occurences of the word 'John' to 'Deere'
deere_tweets_separated_words$word1 <- gsub(pattern = "john", "deere", deere_tweets_separated_words$word1)
deere_tweets_separated_words$word2 <- gsub(pattern = "john", "deere", deere_tweets_separated_words$word2)


#filter by the words
deere_tweets_filtered <- deere_tweets_separated_words %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

#new bigrams
deere_words_counts <- deere_tweets_filtered %>% count(word1, word2, sort = TRUE)

#plot graph
deere_words_counts %>%
    filter(n >= 50) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = "green", size = 2) +
    geom_node_text(aes(label = name), vjust = 1.8, size=2) +
    labs(title= "Word Network: Tweets having the word deere",
         subtitle="Text mining twitter data ", x="", y="")

ggsave(filename = "Deere Word Network_03Nov_After.png", width = 6, height = 4)


