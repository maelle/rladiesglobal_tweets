library("rtweet")
library("readr")
library("dplyr")
library("monkeylearn")

rladies <- get_timeline(user = "RLadiesGlobal",
                        n = 1000)
write_csv(rladies, path = "data/timeline.csv")
# filter only tweets, no direct tweets or retweets
rladies <- select(rladies, - coordinates)
rladies <- filter(rladies, !is_retweet, is.na(in_reply_to_status_id))

# English
rladies <- filter(rladies, lang == "en")






# sentimientos algo 1 "English Tweets Sentiment Analysis"
sentimientos <- monkeylearn_classify(rladies$text,
                                     classifier_id = "cl_qkjxv9Ly")
sentimientos <- bind_cols(select(rladies, text),
                          sentimientos)
write_csv(sentimientos, path = "output/algo1_output.csv")

# sentimientos algo 2 "English Tweets Sentiment&Emotion Analysis"
sentimientos <- monkeylearn_classify(rladies$text,
                                     classifier_id = "cl_mEcCuEcG")
sentimientos <- group_by(sentimientos, text_md5)
sentimientos <- summarize(sentimientos, label = toString(label))
sentimientos <- bind_cols(select(rladies, text),
                          sentimientos)
write_csv(sentimientos, path = "output/algo2_output.csv")