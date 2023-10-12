library(tidyverse)
data_parties_raw <- readRDS("data/data_full.rds")
data_parties_raw_silv <- data_parties_raw%>%
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


data_parties_gold_ts <- data_parties_raw_silv%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  group_by(upload_date, Topic)%>%
  summarise(CDU = sum(CDU)
            , SPD = sum(SPD)
            , GRN = sum(GRN)
            , LINKE = sum(LINKE)
            , FDP = sum(FDP)
            , AFD = sum(AFD))%>%
  rename(Grüne = GRN
         , Linke = LINKE
         , AfD = AFD)%>%
  gather(key = "Partei", value = "Erwähnungen",-Topic, -upload_date)


## Reordering data_parties_gold_ts$Partei
data_parties_gold_ts$Partei <- data_parties_gold_ts$Partei %>%
  fct_relevel(
    "CDU", "SPD", "Grüne", "FDP", "Linke", "AfD"
  )

library(dplyr)
library(ggplot2)

data_parties_gold_ts %>%
 filter(upload_date >= "2015-06-27 22:00:00" & upload_date <= "2016-06-12 02:38:42" | 
 is.na(upload_date), Topic == "Europäische Flüchtlingskrise") %>%
 ggplot() +
 aes(x = upload_date, y = Erwähnungen, colour = Partei) +
 geom_line() +
 scale_color_manual(values = c(AfD = "#0489DB", CDU = "#000000", FDP = "#FFEF00", Grüne = "#1AA037", 
 Linke = "#A6006B", SPD = "#E3000F")) +
 labs(x = "Woche", y = "Erwähungen", title = "Partei Erwähnungen in Thema Europäische Flüchtlingskrise", 
 subtitle = "Summe der Erwähnungen von Parteien nach Schlagwortsuche", caption = "Daten von SPON & BILD zusammen", 
 color = "Partei") +
 theme_light() +
 facet_wrap(vars(Partei))


library(readr)
Partei_Umfragen_Infratest <- read_csv("data/Partei Umfragen Infratest.csv", 
                                      col_types = cols(Datum = col_date(format = "%d.%m.%Y")
                                                       , FDP = col_double()))


Partei_Umfragen_Infratest_Gather <- Partei_Umfragen_Infratest%>%
  mutate(SPD = SPD - lag(SPD, n = 1L, default = NULL, order_by = Datum)
         , Union = Union - lag(Union, n = 1L, default = NULL, order_by = Datum)
         , Grüne = Grüne - lag(Grüne, n = 1L, default = NULL, order_by = Datum)
         , FDP = FDP - lag(FDP, n = 1L, default = NULL, order_by = Datum)
         , AfD = AfD - lag(AfD, n = 1L, default = NULL, order_by = Datum)
         , Linke = Linke - lag(Linke, n = 1L, default = NULL, order_by = Datum)
         , FW = FW - lag(FW, n = 1L, default = NULL, order_by = Datum)
         , Andere = Andere - lag(Andere, n = 1L, default = NULL, order_by = Datum)
         )%>%
  gather(key = Partei, value = "Wert", -Datum)
data_parties_gold_wahlen <- data_parties_raw_silv%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  group_by(upload_date)%>%
  summarise(CDU = sum(CDU)
            , SPD = sum(SPD)
            , GRN = sum(GRN)
            , LINKE = sum(LINKE)
            , FDP = sum(FDP)
            , AFD = sum(AFD))%>%
  rename(Grüne = GRN
         , Linke = LINKE
         , AfD = AFD
         , Union = CDU)%>%
  gather(key = "Partei", value = "Erwähnungen", -upload_date)%>%
  left_join(Partei_Umfragen_Infratest_Gather, join_by(closest(upload_date < Datum)
                                                      , Partei == Partei))%>%
  mutate(pre_sylvester_2016 = as.factor(case_when(ymd(upload_date) <= '2015-12-31' ~ "1"
                                        , ymd(upload_date) > '2015-12-31' ~ "0"))
         , pre_merkel_speech = as.factor(case_when(ymd(upload_date) <= '2015-08-31' ~ "1"
                                         , ymd(upload_date) > '2015-08-31' ~ "0"))
         , Partei = as.factor(Partei)
  )%>%
  dplyr::select(-Datum)

library(fastDummies)
library(jtools)
library(huxtable)
library(zoo)
# load and transoform polit bar data -----------------------------------------------------


polit_bar_prob <- read_excel("data/Polit Barometer Daten 1.xlsx", 
                             skip = 5)
polit_bar_prob <- polit_bar_prob%>%
  filter(date >= '2015-06-01' & date < '2016-04-01')

partei_sentiment_bild <- data_parties_raw_silv%>%
  filter(outlet == "BILD")%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(CDU,SPD,GRN,LINKE,FDP,AFD, sentiment_ai, upload_date)%>%
  gather(key = "partei", value = "value", -sentiment_ai, -upload_date)%>%
  filter(value == 1)%>%
  group_by(upload_date, partei)%>%
  summarise(sentiment = mean(sentiment_ai))%>%
  spread(key = partei, value = sentiment, fill = 0)%>%
  rename(Grüne = GRN
         , Linke = LINKE
         , AfD = AFD
         , Union = CDU)

partei_sentiment_spon <- data_parties_raw_silv%>%
  filter(outlet == "SPON")%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(CDU,SPD,GRN,LINKE,FDP,AFD, sentiment_ai, upload_date)%>%
  gather(key = "partei", value = "value", -sentiment_ai, -upload_date)%>%
  filter(value == 1)%>%
  group_by(upload_date, partei)%>%
  summarise(sentiment = mean(sentiment_ai))%>%
  spread(key = partei, value = sentiment, fill = 0)%>%
  rename(Grüne = GRN
         , Linke = LINKE
         , AfD = AFD
         , Union = CDU)


min_date <- min(data_parties_gold_wahlen$upload_date, na.rm = T)

data_parties_regression <- fastDummies::dummy_cols(data_parties_gold_wahlen
                        , select_columns = c("Partei")
                        , remove_selected_columns = T)%>%
  mutate(Wert_Lag = as.integer(lag(Wert, order_by = upload_date))
         , week_since_start = difftime(upload_date,min_date,units="weeks")) %>%
  left_join(polit_bar_prob, join_by(closest(upload_date >= date)))%>%
  left_join(partei_sentiment_bild, join_by(upload_date == upload_date))%>%
  left_join(partei_sentiment_spon, join_by(upload_date == upload_date))


library(correlation)
correlation(data_parties_regression%>%dplyr::select(migration, social_down_mobility))
reg_model_union <- lm(data = data_parties_regression%>%filter(Partei_Union == 1)%>%rename(sentiment_bild = Union.x, sentiment_spon = Union.y), 
                    Wert ~ Erwähnungen + pre_sylvester_2016 + pre_merkel_speech 
                    + week_since_start + Wert_Lag + migration + social_down_mobility 
                    + sentiment_bild + sentiment_spon)
reg_model_SPD <- lm(data = data_parties_regression%>%filter(Partei_SPD == 1)%>%rename(sentiment_bild = SPD.x, sentiment_spon = SPD.y), 
                      Wert ~ Erwähnungen + pre_sylvester_2016 + pre_merkel_speech 
                    + week_since_start + Wert_Lag + migration + social_down_mobility 
                    + sentiment_bild + sentiment_spon)
reg_model_fdp <- lm(data = data_parties_regression%>%filter(Partei_FDP == 1)%>%rename(sentiment_bild = FDP.x, sentiment_spon = FDP.y), 
                    Wert ~ Erwähnungen + pre_sylvester_2016 + pre_merkel_speech 
                    + week_since_start + Wert_Lag + migration + social_down_mobility 
                    + sentiment_bild + sentiment_spon)
reg_model_grn <- lm(data = data_parties_regression%>%filter(Partei_Grüne == 1)%>%rename(sentiment_bild = Grüne.x, sentiment_spon = Grüne.y), 
                    Wert ~ Erwähnungen + pre_sylvester_2016 + pre_merkel_speech 
                    + week_since_start + Wert_Lag + migration +social_down_mobility  
                    + sentiment_bild + sentiment_spon)
reg_model_linke <- lm(data = data_parties_regression%>%filter(Partei_Linke == 1)%>%rename(sentiment_bild = Linke.x, sentiment_spon = Linke.y), 
                    Wert ~ Erwähnungen + pre_sylvester_2016 + pre_merkel_speech 
                    + week_since_start + Wert_Lag + migration + social_down_mobility 
                    + sentiment_bild + sentiment_spon)
reg_model_afd <- lm(data = data_parties_regression%>%filter(Partei_AfD == 1)%>%rename(sentiment_bild = AfD.x, sentiment_spon = AfD.y), 
                Wert ~ Erwähnungen + pre_sylvester_2016 + pre_merkel_speech 
                + week_since_start + Wert_Lag + migration + social_down_mobility 
                + sentiment_bild + sentiment_spon)


jtools::export_summs(reg_model_union
                     , reg_model_SPD
                     , reg_model_grn
                     , reg_model_fdp
                     , reg_model_afd
                     , reg_model_linke
                     , model.names = c("Union","SPD","Grüne","FDP","AfD","Linke")
                     , error_format = "({std.error})"
                     , bold_signif = 0.1
                     , error_pos = "same"
                     , coefs = c("Erwähnungen"
                             , "Sent. Bild" = "sentiment_bild"
                             , "Sent. SPON" = "sentiment_spon"
                             , "Vor Sylv. Nacht" = "pre_sylvester_20161"
                             , "Vor Merkel Rede" = "pre_merkel_speech1"
                             , "Woche" = "week_since_start"
                             , "Letzte Veränderung" = "Wert_Lag"
                             , "Relev. Migration" = "migration"
                             , "Relev. Sozialangst" = "social_down_mobility")
                     , robust = TRUE
                     , to.file = "latex"
                     , file.name = "Regression"
                     )

