library(tidyverse)
library(DescTools)
#
max_by <- function(data, var, by) { 
  data %>%
    group_by({{ by }}) %>%
    summarise(maximum = max({{ var }}, na.rm = TRUE)) }

data_full<- readRDS("data/data_full.rds")%>%
  mutate(doc_id = paste0("text",row_number())
  )%>%
  mutate(CDU = ifelse(grepl("cdu", tolower(content)),1,0)
         , CDU = ifelse(grepl("cdu", tolower(title)),1,CDU)
         , CDU = ifelse(grepl("csu", tolower(content)),1,CDU)
         , CDU = ifelse(grepl("csu", tolower(title)),1,CDU)
         , CDU = ifelse(grepl("union", tolower(content)),1,CDU)
         , CDU = ifelse(grepl("union", tolower(title)),1,CDU)
         , SPD = ifelse(grepl("spd", tolower(content)),1,0)
         , SPD = ifelse(grepl("spd", tolower(title)),1,SPD)
         , GRN = ifelse(grepl("grüne", tolower(content)),1,0)
         , GRN = ifelse(grepl("grüne", tolower(title)),1,GRN)
         , LINKE = ifelse(grepl("linke", tolower(content)),1,0)
         , LINKE = ifelse(grepl("linke", tolower(title)),1,LINKE)
         , LINKE = ifelse(grepl("pds", tolower(content)),1,LINKE)
         , LINKE = ifelse(grepl("pds", tolower(title)),1,LINKE)
         , FDP = ifelse(grepl("fdp", tolower(content)),1,0)
         , FDP = ifelse(grepl("fdp", tolower(title)),1,FDP)
         , AFD = ifelse(grepl("afd", tolower(content)),1,0)
         , AFD = ifelse(grepl("afd", tolower(title)),1,AFD)
         , AFD = ifelse(grepl("alternative für deutschland", tolower(content)),1,AFD)
         , AFD = ifelse(grepl("alternative für deutschland", tolower(title)),1,AFD)
  )





saveRDS(data_persons, "data/data_persons.rds")
data_persons <- readRDS("data/data_persons.rds")

data_persons%>%
  filter(!name %in% c("New York", "Thomas de", "Fan von", "de Maizière", "VON BILD", "SPIEGEL ONLINE", "SIE FAN", "Fan von BILD.de-News", "New York Times"))%>%
  group_by(name)%>%
  summarise(n = n())%>%
  arrange(desc(n))%>%
  mutate(rank = rank(-n))%>%
  filter(rank <= 20)%>%
  ggplot(aes(x = reorder(name, n), y = n, fill = rank))+
  geom_col()+
  geom_text(aes(label = n), hjust = 1)+
  coord_flip()+
  MetBrewer::scale_fill_met_c("Egypt")+
  labs(title = "Häufigkeit von Personen die in einem Artikel erwähnt werden",
       subtitle = "Top 20",
       x = "Personen",
       y = "Häufigkeit",
       fill = "Häufigkeit"
  )+
  ggthemes::theme_fivethirtyeight()


data_persons%>%
  left_join(data_persons, join_by(doc_id == doc_id, token_id > token_id))%>%
  filter(name.x != name.y)%>%
  filter( between(sentence_id.x, sentence_id.y-5,sentence_id.y +5 ))%>%
  filter((!name.x %like% "Thomas de" & !name.y %like% "Thomas de Maizière") 
         & (!name.x %like% "Thomas de Maizière" & !name.y %like% "Thomas de")
         & (!name.x %like% "Thomas de" & !name.y %like% "de Maizière")
         & (!name.x %like% "de Maizière" & !name.y %like% "Thomas de")
         )%>%
  rename(name1 = name.x, name2 = name.y)%>%
  mutate(name = case_when(name1 > name2 ~ paste(name1, name2)
                          , name2 > name1 ~ paste(name2, name1)
                          )
         , name_pretty = case_when(name1 > name2 ~ paste(name1, " - ", name2)
                                 , name2 > name1 ~ paste(name2, " - ", name1)
         )
  )%>%
  group_by(name, name_pretty)%>%
  summarise(n = n()) %>%
  ungroup()%>%
  filter(!name %in% c("Fan von BILD.de-News Fan von"
                      , "Santiago de Colonia Dignidad"
                      , "Valéry Giscard Helmut Schmidt"
                      , "New Orleans Mardi Gras"
                      , "New York Times New York"
                      , "Franz Josef Wagner Franz Josef"
                      , "Eagles of Death Eagles of"
                      , "Eagles of Death Metal Eagles of"
                      , "Stade de France Stade de"
                      , "Eagles of Death Metal Eagles of Death"
                      , "Charles de Gaulle Charles de"
                      , "Franz Josef Wagner F. J. Wagner"
                      , "Franz Josef F. J."
                      , "Franz Josef F. J. Wagner"
                      , "Franz Josef Wagner F. J."
                      , "F. J. Wagner F. J."
                      , "Franze Josef Wagner E-mail schreiben:fjwagner@bild.de"
                      , "Franze Josef E-mail schreiben:fjwagner@bild.de"
                      , "Human Rights Watch Human Rights"
                      , "VON BILD CHEMNITZ VON BILD"
                      , "World Trade Center World Trade"
                      , "Ibrahim El Bakraoui Ibrahim El"
                      , "Abu Bakr al-Baghdadi Abu Bakr"
                      , "New York Barack Obama"
                      )
         & !name %like% "%@%")%>%
  mutate(rank = rank(-n))%>%
  arrange(desc(n))%>%
  filter(rank <= 20)%>%
  ggplot(aes(x = reorder(name_pretty, n), y = n, fill = rank))+
  geom_col()+
  coord_flip()+
  MetBrewer::scale_fill_met_c("Egypt")+
  labs(title = "Häufigkeit von Personen die in einem Artikel gemeinsam erwähnt werden",
       subtitle = "Top 20",
       x = "Personen",
       y = "Häufigkeit",
       fill = "Häufigkeit"
       )+
  ggthemes::theme_fivethirtyeight()


data_persons%>%
  filter(name == "Angela Merkel")%>%
  left_join(data_full, join_by(doc_id == doc_id))%>%
  select(upload_date, Topic, sentiment_ai, outlet)%>%
  filter(upload_date < "2017-01-01")%>%
  mutate(upload_date = round_date(upload_date, "week"))%>%
  group_by(upload_date, Topic, outlet)%>%
  summarise(sentiment_ai = mean(sentiment_ai, na.rm = T))%>%
  ggplot(aes(x = upload_date, y = sentiment_ai, color = Topic))+
  geom_line() +
  geom_smooth(se = FALSE, method = "lm", linetype = "dashed", alpha = 0.3)+
  MetBrewer::scale_color_met_d(name = "Egypt") +
  facet_wrap(~outlet)+
  ggthemes::theme_fivethirtyeight() +
  labs(title = "Sentiment zu Berichterstattung die Angela Merkel betrifft",
       subtitle = "Durchschnittlich gemessenes Sentiment pro Woche",
       x = "Date",
       y = "Sentiment"
       , color = "Thema") 
