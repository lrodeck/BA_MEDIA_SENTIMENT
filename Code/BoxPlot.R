data_full <- readRDS("Data/data_full.rds")


library(tidyverse)
library(lubridate)

data_full <- data_full%>%
  filter(upload_date <= as.Date("2016-04-01"))%>%
  mutate(upload_week = round_date(upload_date, unit = "week") 
         )
         


ggplot() +
  geom_smooth(data = data_full%>%filter(Topic == "Europäische Flüchtlingskrise")%>%
                group_by(upload_week, outlet)%>%
                summarise(sentiment_mid = median(sentiment_ai))
              , aes(x = as.Date(upload_week), y = sentiment_mid, color = outlet)
              , method = "loess", se = FALSE) +
  geom_boxplot(data = data_full%>%filter(Topic == "Europäische Flüchtlingskrise")
               , aes(x = as.Date(upload_week), group = as.Date(upload_week)
                     , y = sentiment_ai
                     , color = outlet)
               , fill = NA) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(-1, 1)) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "Monatliche Sentiment Verteilung für Topic Europäische Flüchtlingskrise",
       subtitle = "Wöchentliche Verteilung nach verföffentlicher Zeitung mit Loes Trend der Median Sentimente",
       x = "Month",
       y = "Sentiment",
       color = "Zeitung",
       caption = "Daten stammen aus den Archiven von Spiegel und Bild Online | Smoothing Algorithmus: Loess | Zeitraum Juli 2015 bis April 2016") +
  facet_wrap(~outlet, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# median only
data_full%>%filter(Topic == "Europäische Flüchtlingskrise")%>%
  group_by(upload_week, outlet)%>%
  summarise(sentiment_mid = median(sentiment_ai))%>%
  ggplot() +
  aes(x = as.Date(upload_week), y = sentiment_mid, color = outlet) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(-1, 1)) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "Wöchentliches Median Sentiment für Topic Europäische Flüchtlingskrise",
       subtitle = "Wöchentliche Verteilung nach verföffentlicher Zeitung",
       x = "Month",
       y = "Sentiment",
       color = "Zeitung",
       caption = "Daten stammen aus den Archiven von Spiegel und Bild Online | Smoothing Algorithmus: Loess | Zeitraum Juli 2015 bis April 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


