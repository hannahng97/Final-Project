library(devtools)
library(twitteR)

api_key <- "Rb0wieEB6WQDzX5AbV6EMoU5m"
api_secret <- "SL6Eaz2dy7Ovw7lDCCyS8KvOwdCRfC9uv5WyroBJOP8xZigvCj"

access_token <- "989677117033140224-WTLpwrvVnlLyGVUhHFStSfKHjA7dH8S"
access_token_secret <- "yiwPhiqQBEVe7vAu5GqPi43Th4l9uwUmZBuJszgjVFHXG"


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#The maximum number of tweets to return. Default number is 25
none <- searchTwitter('celtics',n = 500, resultType = "recent")

#Search for hashtag
hashtag <- searchTwitter('#celtics', n = 500, resultType = "recent")

tweets.df <- twListToDF(none)
write.csv(tweets.df, file = "final1.csv")

tweets.df2 <- twListToDF(hashtag)
write.csv(tweets.df, file = "final2.csv")
