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

#lets go get those tweets
deerestats <- search_tweets("Deere", n = 10000, include_rts = FALSE)

#lets view them
View(deerestats)
#hyperlinks aren't works so lets get rid of them
deerestatsNew$stripped_text <- gsub("http.*","",  deerestatsNew$text)
deerestatsNew$stripped_text <- gsub("https.*","", deerestatsNew$stripped_text)
#strip and clean
deerestats_cleanNew <- deerestatsNew %>%
         dplyr::select(stripped_text) %>%
         unnest_tokens(word,stripped_text)

View(deerestats_cleanNew)

# plot the top words
deerestats_cleanNew %>%
    count(word, sort=TRUE) %>%
    top_n(60) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x="Count",
         y="Unique words",
         title="Count of unique words found in tweets")

#look at the data

#john and deere are going to appear a lot...duh... get rid of them
deerestats_cleanNew <- subset(deerestats_cleanNew, word != "deere")
deerestats_cleanNew <- subset(deerestats_cleanNew, word != "john")

#make stop_words
data("stop_words")
cleaned_tweet_wordsNew <- deerestats_cleanNew %>%
    anti_join(stop_words)

#lets look again.
cleaned_tweet_wordsNew %>%
    count(word, sort=TRUE) %>%
    top_n(60) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(x="Count",
         y="Unique words",
         title="Count of unique words found in tweets")

#lets try wayyyy more tweets and retweets
deerestats50 <- search_tweets("Deere", n = 50000, include_rts = TRUE, retryonratelimit = TRUE)

#lets make a word network!
#this will let us see the relations words have to each other in pairs

#widyr library is needed for this
library(widyr)

#make deerestats2
deerestats2 <- deerestats


#remove punctuations and convert to lowercase, also add id for tweets
deere_tweets_paired_words <- deerestats2 %>% 
    dplyr::select(stripped_text) %>% unnest_tokens(paired_words, stripped_text, token = "ngrams", n=2)



#count the number of paired words
deere_tweets_paired_words %>% count(paired_words, sort = TRUE)

#pair the words together
deere_tweets_separated_words <- deere_tweets_paired_words %>%
    separate(paired_words, c("word1", "word2"), sep = " ")
#****can't get this part working, something to do with it being atomic I believe
#replace John with Deere as they will probably almost always be pairs
#levels(deere_tweets_separated_words$word1)[levels(deere_tweets_separated_words$word1) == "john"] <- "deere"
#wh <- levels(deere_tweets_separated_words$word1) == "john"
#levels(deere_tweets_separated_words$word1)[wh] <- "deere"
#*******

#filter by the words
deere_tweets_filtered <- deere_tweets_separated_words %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

#new bigrams
deere_words_counts <- deere_tweets_filtered %>% count(word1, word2, sort = TRUE)

#plot graph
deere_words_counts %>%
    filter(n >= 24) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = "green", size = 2) +
    geom_node_text(aes(label = name), vjust = 1.8, size=2) +
    labs(title= "Word Network: Tweets having the word deere",
         subtitle="Text mining twitter data ",
         x="", y="")

#the following users are known "bots"
# bots <- c("Deere_w13","RodriguezDaGod","Deere_k","John_F_Deere") 

