library("rtweet")
library("readr")
library("dplyr")
library("monkeylearn")

############################################################
#                                                          #
#                      RLadies global                      #
#                                                          #
############################################################


rladies <- get_timeline(user = "RLadiesGlobal",
                        n = 1000)
rladies <- select(rladies, - coordinates)
library("lubridate")
rladies <- mutate(rladies, hour = hour(created_at))

library("ggplot2") 
ggplot(rladies) +
  geom_bar(aes(hour))

retweets <- filter(rladies, is_retweet)
retweets <- mutate(retweets, person = gsub(":.*", "", text))
retweets <- mutate(retweets, person = gsub("RT @", "", person))
retweets <- group_by(retweets, person)%>%
  mutate(retweeted_person = ifelse(n() <= 3, "others", person)) %>%
  ungroup() %>%
  group_by(retweeted_person) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
retweets <- mutate(retweets,
                   retweeted_person = factor(retweeted_person,
                                        ordered = TRUE,
                                        levels = unique(retweeted_person)))

ggplot(retweets) +
  geom_bar(aes(retweeted_person, n, fill = retweeted_person), stat = "identity") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

# mentioned
screennames <- group_by(rladies, status_id)
screennames <- mutate(screennames, mentions_screen_name = strsplit(mentions_screen_name, ","))
screennames <- tidyr::unnest(screennames, mentions_screen_name)
screennames <- filter(screennames, !is.na(mentions_screen_name))
screennames <- group_by(screennames, mentions_screen_name)%>%
  mutate(mentioned_person = ifelse(n() <= 5, "others", mentions_screen_name)) %>%
  ungroup() %>%
  group_by(mentioned_person) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
screennames <- mutate(screennames,
                      mentioned_person = factor(mentioned_person,
                                             ordered = TRUE,
                                             levels = unique(mentioned_person)))

ggplot(screennames) +
  geom_bar(aes(mentioned_person, n, fill = mentioned_person), stat = "identity") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))