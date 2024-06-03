library(tidyverse)
library(ggthemr)
library(quanteda.sentiment)
library(ggpubr)
ggthemr('light')
POS_DATA <- readRDS("~/R Projects/BACHELOR_ARBEIT/Data/POS_DATA.rds")
data_full <- readRDS("~/R Projects/BACHELOR_ARBEIT/Data/data_full.rds")
data_tokens_nested <- readRDS("~/R Projects/BACHELOR_ARBEIT/Data/data_tokens_nested.RDS")


dict_sentiment_pos <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$positive
                             , pos = 1)
dict_sentiment_neg <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$negative
                             , neg = 1)

pos_neg_data_adj <- POS_DATA%>%
  filter(upos == "ADJ")%>%
  left_join(dict_sentiment_neg, join_by(lemma == word))%>%
  left_join(dict_sentiment_pos, join_by(lemma == word))%>%
  left_join(data_full, join_by(doc_id == urls))%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(Topic, doc_id, outlet, upload_date, lemma, neg, pos)%>%
  filter(!is.na(pos) | !is.na(neg))%>%
  group_by(Topic, outlet,  upload_date)%>%
  summarise(articles = n_distinct(doc_id) 
            , positives = sum(pos, na.rm = T)
            , negatives = sum(neg, na.rm = T))



library(dplyr)
library(ggplot2)

pos_neg_data_adj %>%
 filter(upload_date >= "2014-12-31 23:00:00" & upload_date <= 
 "2016-04-01 00:00:00") %>%
 filter(positives <= 1000)%>%
 ggplot() +
 aes(x = upload_date, y = positives/articles, colour = outlet) +
 geom_line(size = 0.5) +
 geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
 stat_regline_equation() +
 scale_color_hue(direction = 1) +
 labs(x = "Datum", y = "Postive Adjektive", title = "Positive Adjektive ", 
 subtitle = "Nennung von Positiven Adjektiven normalisiert pro Artikel", caption = "Daten von Jul 2015 - Mär 2016", 
 color = "Zeitung") +
 facet_wrap(vars(Topic), scales = "free_y")


pos_neg_data_adj %>%
  filter(upload_date >= "2014-12-31 23:00:00" & upload_date <= 
           "2016-04-01 00:00:00") %>%
  filter(negatives <= 1000)%>%
  ggplot() +
  aes(x = upload_date, y = negatives/articles, colour = outlet) +
  geom_line(size = 0.5) +
  stat_regline_equation() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_hue(direction = 1) +
  labs(x = "Datum"
       , y = "Negative Adjektive"
       , title = "Negative Adjektive "
       , subtitle = "Nennung von Negativen Adjektiven normalisiert pro Artikel"
       , caption = "Daten von Jul 2015 - Mär 2016"
       , color = "Zeitung") +
  facet_wrap(vars(Topic), scales = "free_y")


pos_neg_data_verb <- POS_DATA%>%
  filter(upos == "VERB")%>%
  left_join(dict_sentiment_neg, join_by(lemma == word))%>%
  left_join(dict_sentiment_pos, join_by(lemma == word))%>%
  left_join(data_full, join_by(doc_id == urls))%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(Topic, doc_id, outlet, upload_date, lemma, neg, pos)%>%
  filter(!is.na(pos) | !is.na(neg))%>%
  group_by(Topic, outlet,  upload_date)%>%
  summarise(articles = n_distinct(doc_id) 
            , positives = sum(pos, na.rm = T)
            , negatives = sum(neg, na.rm = T))


pos_neg_data_verb %>%
  filter(upload_date >= "2014-12-31 23:00:00" & upload_date <= 
           "2016-04-01 00:00:00") %>%
  filter(positives <= 1000)%>%
  ggplot() +
  aes(x = upload_date, y = positives/articles, colour = outlet) +
  geom_line(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  stat_regline_equation() +
  scale_color_hue(direction = 1) +
  labs(x = "Datum", y = "Postive Verben", title = "Positive Verben ", 
       subtitle = "Nennung von Positiven Verben normalisiert pro Artikel", caption = "Daten von Jul 2015 - Mär 2016", 
       color = "Zeitung") +
  facet_wrap(vars(Topic), scales = "free_y")


pos_neg_data_verb %>%
  filter(upload_date >= "2014-12-31 23:00:00" & upload_date <= 
           "2016-04-01 00:00:00") %>%
  filter(negatives <= 1000)%>%
  ggplot() +
  aes(x = upload_date, y = negatives/articles, colour = outlet) +
  geom_line(size = 0.5) +
  stat_regline_equation() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_hue(direction = 1) +
  labs(x = "Datum"
       , y = "Negative Verben"
       , title = "Negative Verben "
       , subtitle = "Nennung von Negativen Verben normalisiert pro Artikel"
       , caption = "Daten von Jul 2015 - Mär 2016"
       , color = "Zeitung") +
  facet_wrap(vars(Topic), scales = "free_y")
