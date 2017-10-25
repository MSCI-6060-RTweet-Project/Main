## whatever name you assigned to your created app
appname <- "rtweet_deere"

## api key (example below is not a real key)
key <- "PYZkll7T1peMIxh0TbPRDLHrf"

## api secret (example below is not a real key)
secret <- "ugKrLGVcsmxBAxaQKwOBtvIueOF166sZpRrJnkSf1v2WdxdWy5"

## create token named "twitter_token"
twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret,
    cache = TRUE)


#consumerKey <- "PYZkll7T1peMIxh0TbPRDLHrf"
#consumerSecret <- "ugKrLGVcsmxBAxaQKwOBtvIueOF166sZpRrJnkSf1v2WdxdWy5"
#accessToken <- "254240036-p2qPxUOp0J1DqiNvhjvviHkm78cqmHBzOwevXIgd"
#accessTokenSecret <- "tu8WB3tFZWW332d8aW42FWYKDebpO09Qap5YXx5hLEm9n"

#setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)