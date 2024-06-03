library(tidyverse)
library(extrafont)
library(gt)
library(gtExtras)
library(lubridate)

ts_data <- readRDS("Data/data_full.rds")%>%
  select(outlet, upload_date, Topic, sentiment_ai)%>%
  mutate(upload_date = floor_date(upload_date, unit = "weeks"))%>%
  group_by(outlet, Topic, upload_date)%>%
  summarise(mean_sentiment_ai = mean(sentiment_ai))%>%
  ungroup()%>%
  arrange(upload_date)%>%
  group_by(outlet, Topic)%>%
  summarise(sentiment_ts = list(mean_sentiment_ai))


readRDS("Data/data_full.rds")%>%
  filter(upload_date <= as.Date("2017-01-01"))%>%
  mutate(Datum = format(as.Date(upload_date), "%b-%Y"))%>%
  select(Datum, upload_date, Topic, outlet, sentiment_ai)%>%
  arrange(upload_date, outlet, Topic)%>%
  select(!upload_date)%>%
  rename(Zeitung = outlet
         , Thema = Topic)%>%
  group_by(Zeitung, Thema)%>%
  summarise(`Median Sentiment` = round(median(sentiment_ai),2)
            , `Standard Abw. Sentiment` = round(sd(sentiment_ai),2)
            , `Min Sentiment` = round(min(sentiment_ai),2)
            , `Max Sentiment` = round(max(sentiment_ai),2)
            , sentiment_data = list(sentiment_ai))%>%
  ungroup()%>%
  left_join(ts_data, join_by(Zeitung == outlet, Thema == Topic))%>%
  rename(`Sentiment Verteilung` = sentiment_data
         , `Sentiment Zeitreihe` = sentiment_ts)%>%
  gt() |>
  tab_header(
    title = "Zusammenfassung Datenset"
    , subtitle = "Deskriptive Sentiment Statistiken Pro Monat Pro Zeitung"
  )%>%
  tab_source_note(
    source_note = "Quelle: Onlinearchive von Bild und Spiegel Juli 2015 - April 2016"
  )%>%
  gt_plt_dist(`Sentiment Verteilung`)%>%
  gt_plt_sparkline(`Sentiment Zeitreihe`)%>%
  gt_theme_538()%>%
  tab_spanner(
    label = "Dimensionen",
    columns = c(Zeitung, Thema)
  )%>%
  tab_spanner(
    label = "Statistiken",
    columns = c(`Median Sentiment`, `Standard Abw. Sentiment`
                , `Min Sentiment`, `Max Sentiment`)
  )%>%
  tab_spanner(
    label = "Aggregationen",
    columns = c(`Sentiment Verteilung`, `Sentiment Zeitreihe`)
  )%>%
  opt_stylize(style = 1, color = "gray", add_row_striping = TRUE)


