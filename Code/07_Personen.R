library(tidyverse)
#
max_by <- function(data, var, by) { 
  data %>%
    group_by({{ by }}) %>%
    summarise(maximum = max({{ var }}, na.rm = TRUE)) }

data_full<- readRDS("data/data_full.rds")%>%
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



data_persons <- readRDS("data/POS_DATA.rds")%>%
  group_by(doc_id, paragraph_id, sentence_id)%>%
  select(-xpos, -lemma, -head_token_id, -deps, -misc) %>%
  unique()%>%
  mutate(lag_token = lag(token, order_by = c(token_id))
         , lag_upos = lag(upos, order_by = c(token_id))
         , lag_token_2 = lag(token, n = 2, order_by = c(token_id))
         , lag_upos_2 = lag(upos, n = 2, order_by = c(token_id))
         , lag_token_3 = lag(token, n = 3, order_by = c(token_id))
         , lag_upos_3 = lag(upos, n = 3, order_by = c(token_id))
         , lag_token_4 = lag(token, n = 4, order_by = c(token_id))
         , lag_upos_4 = lag(upos, n = 4, order_by = c(token_id))
         , lag_token_5 = lag(token, n = 5, order_by = c(token_id))
         , lag_upos_5 = lag(upos, n = 5, order_by = c(token_id))
         , lag_token_6 = lag(token, n = 6, order_by = c(token_id))
         , lag_upos_6 = lag(upos, n = 6, order_by = c(token_id))
         , name_mentioned_2 = ifelse(lag_upos == "PROPN" & upos == "PROPN", paste(lag_token, token),NA)
         , name_mentioned_3 = ifelse(lag_upos == "PROPN" & upos == "PROPN" & lag_upos_2 == "PROPN", paste(lag_token_2, lag_token, token),NA)
         , name_mentioned_4 = ifelse(lag_upos == "PROPN" & upos == "PROPN" & lag_upos_2 == "PROPN" & lag_upos_3 == "PROPN", paste(lag_token_3, lag_token_2, lag_token, token),NA)
         , name_mentioned_5 = ifelse(lag_upos == "PROPN" & upos == "PROPN" &
                                     lag_upos_2 == "PROPN" & lag_upos_3 == "PROPN" &
                                     lag_upos_4 == "PROPN"
                                     , paste(lag_token_4, lag_token_3, lag_token_2, lag_token, token)
                                     , NA)
         , name_mentioned_6 = ifelse(lag_upos == "PROPN" & upos == "PROPN" &
                                       lag_upos_2 == "PROPN" & lag_upos_3 == "PROPN" &
                                       lag_upos_4 == "PROPN" & lag_upos_5 == "PROPN"
                                     , paste(lag_token_5, lag_token_4, lag_token_3, lag_token_2, lag_token, token)
                                     , NA)
         , name_mentioned_7 = ifelse(lag_upos == "PROPN" & upos == "PROPN" &
                                       lag_upos_2 == "PROPN" & lag_upos_3 == "PROPN" &
                                       lag_upos_4 == "PROPN" & lag_upos_5 == "PROPN" &
                                       lag_upos_6 == "PROPN"
                                     , paste(lag_token_6, lag_token_5, lag_token_4, lag_token_3, lag_token_2, lag_token, token)
                                     , NA)
         , name = coalesce(name_mentioned_7, name_mentioned_6, name_mentioned_5, name_mentioned_4, name_mentioned_3, name_mentioned_2)
  )%>%
  select(-contains("lag"), -c(name_mentioned_7, name_mentioned_6, name_mentioned_5, name_mentioned_4, name_mentioned_3, name_mentioned_2))%>%
  drop_na() %>%
  ungroup() %>%
  left_join(data_full, join_by(doc_id == urls), relationship = "many-to-many")

names(data_persons)

data_persons%>%
  select(name, Topic, outlet, CDU, SPD, GRN, LINKE, FDP, AFD)%>%
  group_by(name)%>%
  reframe(CDU = sum(CDU)
            , SPD = sum(SPD)
            , GRN = sum(GRN)
            , LINKE = sum(LINKE)
            , FDP = sum(FDP)
            , AFD = sum(AFD)) %>%
  filter(CDU > 2
         | SPD > 2
         | GRN > 2
         | LINKE > 2
         | FDP > 2
         | AFD > 2)%>%
  gather(key = "Partei", value = "mentions", -name )%>%
  group_by(name)%>%
  mutate(rank = row_number(-mentions)
         , total_mentions = sum(mentions))%>%
  ungroup()%>%
  filter(total_mentions >= 10 & rank == 1)%>%
  select(-mentions, -rank)%>%
  view()
  
  
