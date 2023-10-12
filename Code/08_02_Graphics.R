data_sentiment_topics <- readRDS("data_full.rds")

library(dplyr)
library(ggplot2)
library(owidR)
library(tidyverse)
library(vtable)


st(data_sentiment_topics%>%select(-Rank1_K4, -topic, -Topic)
   , file='sentiment_summary.html'
   , title = "Deskriptive Statistik")


# Menge an Artikeln ----

data_timeseries <- data_sentiment_topics%>%
  select(upload_date, outlet)%>%
  group_by(upload_date, outlet)%>%
  count()%>%
  unique()

mean(data_timeseries$n)
max(data_timeseries$n) - min(data_timeseries$n)

# Monatliche Sentiment Boxplots ----

data_sentiment_topics %>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(sentiment_ai, upload_date, outlet)%>%
  group_by(upload_date, outlet)%>%
  summarise(sentiment_ai = median(sentiment_ai))%>%
  unique() %>%
  filter(upload_date >= "2015-06-27 22:00:00" & upload_date <= "2017-07-28 04:58:03" & 
           !is.na(upload_date)) %>%
  ggplot() +
  aes(x = upload_date, y = sentiment_ai, colour = outlet, linetype = outlet) +
  geom_line() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  scale_color_manual(values = c(BILD = "#db006e", SPON = "#3a3da8")) +
  labs(x = ""
       , y = "Sentiment"
       , title = "Sentiment Zeitreihe"
       , subtitle = "Durchschnittles Sentiment pro Woche nach Medium"
       , caption = "Daten stammen aus den Archiven von Spiegel und Bild Online
Zeitraum Juli 2015 bis April 2016"
       , color = "Zeitung"
       , linetype = "Zeitung") +
  owidR::theme_owid()

# Wöchentliche Menge an Artikel nach Topic

data_sentiment_topics%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(upload_date, outlet)%>%
  group_by(upload_date, outlet)%>%
  count()%>%
  unique()%>%
  filter(upload_date >= "2015-06-27 22:00:00" & upload_date <= "2017-07-28 04:58:03" & 
           !is.na(upload_date)) %>%
  ggplot() +
  aes(x = upload_date, y = n, colour = outlet, linetype = outlet) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(x = ""
       , y = "Artikel"
       , title = "Artikelmenge Pro Woche"
       , subtitle = "Menge der Gescrapten Artikel zur Thematik der Flüchtlingskrise.
Menge der Beobachtungen pro Woche nach Leitmedium."
       , caption = "Daten von Juli 2015 bis März 2016", 
       color = "f", linetype = "f") +
  owidR::theme_owid()

# Wöchentliche Sentiment nach Topic ----

data_sentiment_topics %>%
 mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
 select(sentiment_ai, upload_date, outlet, Topic)%>%
 group_by(upload_date, outlet, Topic)%>%
 summarise(sentiment_ai = median(sentiment_ai))%>%
 unique() %>%
 filter(upload_date >= "2015-06-27 22:00:00" & upload_date <= "2017-07-28 04:58:03" & 
 !is.na(upload_date)) %>%
 ggplot() +
 aes(x = upload_date, y = sentiment_ai, colour = outlet, linetype = outlet) +
 geom_line() +
 geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  scale_color_manual(values = c(BILD = "#db006e", SPON = "#3a3da8")) +
 labs(x = ""
      , y = "Sentiment"
      , title = "Sentiment Zeitreihe"
      , subtitle = "Durchschnittles Sentiment pro Woche nach Topic und Medium"
      , caption = "Daten stammen aus den Archiven von Spiegel und Bild Online
Zeitraum Juli 2015 bis April 2016"
      , color = "Zeitung"
      , linetype = "Zeitung") +
 owidR::theme_owid() +
 facet_wrap(vars(Topic))

#K Plot result
plot <- readRDS("topics_plot.rds")
ggplot(plot, aes(K, value, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~variable,scales = "free_y") +
  labs(x = "Menge an Topics K",
       title = "Statistischer Fit Modelle mit verschiedenen K")

# Model Topics Words
library(topicmodels)
library(tidyverse)
library(reshape2)
library(stm) 

model <- readRDS("topics_model.rds")
plot(model,type="labels"
     , main = "Cluster Wort Asoziationen")
