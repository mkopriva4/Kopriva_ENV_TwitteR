library(twitteR)
library(tidyverse)
setup_twitter_oauth("6sjseFWNctpdBeT5JVjFLcorp",
                    "kFz5JPcTxSkYxp77beOendNW2BVlsdmwYcodbMg2XBbppZ8kxY",
                    "789870228-KQrMaR4KPxXW8aLBcy40ygFQaMyQ5vHY7RKbplEE",
                    "MdzxGgOEiwiIqm7v3qqsNACz2YJdKHIjEfmshPxYEpDrA")

trump_tweets2 <- userTimeline("realDonaldTrump", n = 3200)
trump_tweets_2df <- tbl_df(map_df(trump_tweets2, as.data.frame))

trump_friends <- friendships(user_ids = character("mlraposa"))

tweets <- trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

tweets2 <- trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*)<") %>%
  filter(source %in% c("iPhone", "Android"))

identical(tweets, tweets2)

library(lubridate)
tweets %>%
  count(source, hour = hour(with_tz(created, "EST")))

maga <- searchTwitter("#MAGA", n = 200)
maga_df <- tbl_df(map_df(maga, as.data.frame))

kanye <- searchTwitter("@kanyewest", n=200)
kanye_d <- twListToDF(kanye)
kanye_df <- tbl_df(map_df(kanye, as.data.frame))
kanye <- getUser('kanyewest')
kanye_pic <- kanye$getProfileImageUrl()
kanye_pic


retweets("991690248399794176", n=50)

trump <- getUser('realDonaldTrump')
trump$getFollowersCount()
trump$getFriends(n=50)

Montreal_trends <- getTrends(4118)
Chile_trends <- closestTrendLocations(-42.8, -71.1)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"'))%>%
  mutate(text = str_replace_all(text,
                                "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))
