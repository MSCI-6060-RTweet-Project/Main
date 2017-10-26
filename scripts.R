#start of TweetDeere scripts - aka common functions
#version - 0.0.1
# modified by lg70171 on 2017-10-25


#botremove function
#meant to remove "bots" or users who have chosen Deere in screen_name
#description - removes screen_name and replies to that screen_name

botremove <- function(bot, dataframe){
    dataframe <- subset(deerestatM, deerestatM$screen_name != bot)
    dataframe <- subset(deerestatM, deerestatM$in_reply_to_status_screen_name != bot)
}

botremove(bot = "Deere_w13", dataframe = "deerestatsM1")
