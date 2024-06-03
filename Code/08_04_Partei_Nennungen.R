library(tidyverse)
library(quanteda.sentiment)


POS_DATA <- readRDS("Data/parsedtxt_all.rds")%>%
  mutate(id = cumsum(ifelse(sentence_id == 1 & (lag(sentence_id) > sentence_id | is.na(lag(sentence_id))), 1, 0))
         , doc_id = paste0("text", id))

dict_sentiment_pos <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$positive
                             , pos = 1)
dict_sentiment_neg <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$negative
                             , neg = 1)

partei_negative_mentions_context <- POS_DATA%>%
  mutate(CDU = ifelse(grepl("cdu", tolower(lemma)),1,0)
         , CDU = ifelse(grepl("cdu", tolower(token)),1,CDU)
         , CDU = ifelse(grepl("csu", tolower(lemma)),1,CDU)
         , CDU = ifelse(grepl("csu", tolower(token)),1,CDU)
         , CDU = ifelse(grepl("union", tolower(lemma)),1,CDU)
         , CDU = ifelse(grepl("union", tolower(token)),1,CDU)
         , SPD = ifelse(grepl("spd", tolower(lemma)),1,0)
         , SPD = ifelse(grepl("spd", tolower(token)),1,SPD)
         , GRN = ifelse(grepl("grüne", tolower(lemma)),1,0)
         , GRN = ifelse(grepl("grüne", tolower(token)),1,GRN)
         , LINKE = ifelse(grepl("linke", tolower(lemma)),1,0)
         , LINKE = ifelse(grepl("linke", tolower(token)),1,LINKE)
         , LINKE = ifelse(grepl("pds", tolower(lemma)),1,LINKE)
         , LINKE = ifelse(grepl("pds", tolower(token)),1,LINKE)
         , FDP = ifelse(grepl("fdp", tolower(lemma)),1,0)
         , FDP = ifelse(grepl("fdp", tolower(token)),1,FDP)
         , AFD = ifelse(grepl("afd", tolower(lemma)),1,0)
         , AFD = ifelse(grepl("afd", tolower(token)),1,AFD)
         , AFD = ifelse(grepl("alternative für deutschland", tolower(lemma)),1,AFD)
         , AFD = ifelse(grepl("alternative für deutschland", tolower(token)),1,AFD)
  )%>%
  left_join(dict_sentiment_neg, join_by(token == word))
  
partei_negative_mentions_context <- partei_negative_mentions_context%>%  
  gather(key = "partei", value = "mention"
         , -doc_id, -sentence_id, -token_id, -token, -lemma, -head_token_id, - dep_rel, -neg)%>%
  select(doc_id, sentence_id, partei, mention, neg)%>%
  group_by(doc_id, sentence_id, partei)%>%
  summarise(mention = max(mention)
            , negatives = sum(neg, na.rm = T))%>%
  ungroup()%>%
  group_by(doc_id, partei)%>%
  arrange(doc_id, partei, sentence_id)%>%
  mutate(neg_lead_1 = lead(negatives, n = 1L)
         , neg_lead_2 = lead(negatives, n = 2L)
         , neg_lag_1 = lag(negatives, n = 1L)
         , neg_tots = negatives + neg_lead_1 + neg_lead_2 + neg_lag_1
         )%>%
  filter(mention == 1)%>%
  ungroup()


contextual_negative_mentions <- partei_negative_mentions_context%>%
  left_join(data_full, join_by(doc_id == urls))%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(partei, neg_tots, upload_date, outlet, sentiment_ai, Topic, doc_id, sentence_id)%>%
  group_by(partei, upload_date, outlet, Topic, doc_id)%>%
  summarise(mentions = n_distinct(sentence_id)
            , negative_mentions = sum(neg_tots, na.rm = T))%>%
  ungroup()%>%
  group_by(partei, upload_date, outlet, Topic)%>%
  summarise(mentions_tots = sum(mentions)
            , mentions_mean = mean(mentions)
            , negative_tots = sum(negative_mentions)
            , negative_mean = mean(negative_mentions)
            )


library(dplyr)
library(ggplot2)

contextual_negative_mentions %>%
 filter(upload_date >= "2014-12-31 23:00:00" & upload_date <= "2016-11-30 22:12:00") %>%
 filter(Topic %in% "Europäische Flüchtlingskrise") %>%
 ggplot() +
 aes(x = upload_date, y = negative_mean, colour = outlet) +
 geom_line(size = 0.5) +
 scale_color_discrete()+
 labs(x = "Datum", y = "Durchschn. Negative Nennungen pro Artikel", 
 title = "Durchschnittliche Negative Erwähnungen von Parteien pro Woche pro Artikel", subtitle = "Kontextuelle analyse von Negativen Wörtern in Verbindung mit Parteinennungen", 
 caption = "Daten von Jul 2015 - Apr 2016", color = "Partei") +
 facet_wrap(vars(partei))

