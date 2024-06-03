data_sentiment_topics <- readRDS("Data/data_full.rds")

library(dplyr)
library(ggplot2)
library(owidR)
library(tidyverse)
library(vtable)
library(ggthemr)
ggthemr('light')
st(data_sentiment_topics%>%select( -Topic)
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
  filter(upload_date >= "2015-06-27 22:00:00" & upload_date < "2016-05-01 00:00:00" & 
           !is.na(upload_date)) %>%
  ggplot() +
  aes(x = upload_date, y = sentiment_ai, colour = outlet) +
  geom_line() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c(BILD = "#db006e", SPON = "#3a3da8")) +
  ggthemes::theme_fivethirtyeight() +
  labs(x = ""
       , y = "Sentiment"
       , title = "Sentiment Zeitreihe"
       , subtitle = "Durchschnittles Sentiment pro Woche nach Medium"
       , caption = "Daten stammen aus den Archiven von Spiegel und Bild Online
Zeitraum Juli 2015 bis April 2016"
       , color = "Zeitung"
       , linetype = "Zeitung")

# Wöchentliche Menge an Artikel nach Topic

data_sentiment_topics%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  select(upload_date, outlet)%>%
  group_by(upload_date, outlet)%>%
  count()%>%
  unique()%>%
  filter(upload_date >= "2015-06-27 22:00:00" & upload_date < "2016-05-01 00:00:00" & 
           !is.na(upload_date)) %>%
  ggplot() +
  aes(x = upload_date, y = n, colour = outlet) +
  geom_line() +
  scale_color_hue(direction = 1) +
  ggthemes::theme_fivethirtyeight() +
  labs(x = ""
       , y = "Artikel"
       , title = "Artikelmenge Pro Woche"
       , subtitle = "Menge der Gescrapten Artikel zur Thematik der Flüchtlingskrise.
Menge der Beobachtungen pro Woche nach Leitmedium."
       , caption = "Daten von Juli 2015 bis März 2016", 
       color = "f", linetype = "f")

# Wöchentliche Sentiment nach Topic ----

data_sentiment_topics %>%
 mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
 select(sentiment_ai, upload_date, outlet, Topic)%>%
 group_by(upload_date, outlet, Topic)%>%
 summarise(sentiment_ai = median(sentiment_ai))%>%
 unique() %>%
 filter(upload_date >= "2015-06-27 22:00:00" & upload_date < "2016-05-01 00:00:00" & 
          !is.na(upload_date)) %>%
 ggplot() +
 aes(x = upload_date, y = sentiment_ai, colour = outlet) +
 geom_line() +
 geom_smooth(formula = y ~ x, method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c(BILD = "#db006e", SPON = "#3a3da8")) +
 labs(x = ""
      , y = "Sentiment"
      , title = "Sentiment Zeitreihe"
      , subtitle = "Durchschnittles Sentiment pro Woche nach Topic und Medium"
      , caption = "Daten stammen aus den Archiven von Spiegel und Bild Online
Zeitraum Juli 2015 bis April 2016"
      , color = "Zeitung"
      , linetype = "Zeitung") +
 ggthemes::theme_fivethirtyeight() +
 facet_wrap(vars(Topic))

#K Plot result
prep_spec <- readRDS("Models/prep_spec.rds")
articlesPrevFit_spec <- readRDS("Models/articlesPrevFit_spec.rds")
articlesPrevFit <- readRDS("Models/articlesPrevFit.rds")

# visualisation ----
## Summary visualization ----

plot(articlesPrevFit
     , type = "summary"
     , xlim = c(0, 0.5)
     , main = "Cluster Wort Assoziationen")
plot(articlesPrevFit_spec
     , type = "summary"
     , xlim = c(0, 0.5))

plot(articlesPrevFit
     , type = "summary"
     , text.cex = 0.5
     , main = "Themenanteile am Datenset"
     , xlab = "geschätzter Themenanteil")

plot(articlesPrevFit
     , type = "perspectives"
     , topics = c(3,4)
     , xlim = c(0, 0.5)
     , labeltype = "score"
     , custom.labels = c("Europäische Flüchtlingskrise"
                         , "Rechtsextremismus"))


lda.themen.aehnlichkeit <- as.data.frame(articlesPrevFit_spec$beta) %>% 
  scale() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")
par(mar = c(0, 4, 4, 2))
plot(lda.themen.aehnlichkeit, main = "LDA-Themenähnlichkeit nach Features", xlab = "", sub = "")

## Metadata/topic relationship visualization ----
plot(articlesPrevFit_spec
     , covariate = "sentiment_ai"
     , topics = c(1,2,3,4,5,6,7,8)
     , model = articlesPrevFit_spec
     , method = "difference"
     , type = "summary"
     , cov.value1 = "Positive"
     , cov.value2 = "Negative"
     , xlab = "Eher Negativ ... Eher Positiv"
     , main = "Sentiment Zusammenhang der berechneten Themen"
     , xlim = c(-1, 1)
     , labeltype = "score"
     # , custom.labels = c("Sonstiges (US Wahlen, Kultur)"
     #                     , "Rechtsextremismus (NSU, Pegida)"
     #                     , "Flüchtlingskrise"
     #                     , "Syrischer Bürgerkrieg")
)


