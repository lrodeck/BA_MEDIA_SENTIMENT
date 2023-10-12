# Load Libraries ----------------------------------------------------------

# source https://benwiseman.github.io/sentiment.ai/
# Load the package
require(sentiment.ai)
require(SentimentAnalysis)
require(sentimentr)
require(tidyverse)
require(data.table)
require(ds4psy)
# Load data sets ----------------------------------------------------------

data_fin <- readRDS("table_spiegel_data.rds")
data_sent_raw <- readRDS("table_sentiments_raw.rds")
data_flucht_sent <- readRDS("table_data_flucht_sent_sentiments_raw.rds")
data_flucht <- readRDS("table_data_flucht_sentiments_raw.rds")
data_fin_bild <- readRDS(file = "table_bild_data.rds")



# Set Up Model ------------------------------------------------------------
# if you don't want to install the model jump right to exploratory data analysis 
# or timeseries & correlation

# install_sentiment.ai(method = "conda") # uncomment for reinstallation
# Initiate the model
# This will create the sentiment.ai.embed model
# Do this so it can be reused without recompiling - especially on GPU!

init_sentiment.ai(model = "multi.large", method = "conda")


# Data Transformation -----------------------------------------------------


data_flucht_spon <- data_fin%>%
  filter(grepl("flüchtling", tolower(content))
        | grepl("flüchtling", tolower(title))
        | grepl("migranten", tolower(content))
        | grepl("migranten", tolower(title))
        | grepl("migration", tolower(content)) 
        | grepl("migration", tolower(title))
        | grepl("ausland", tolower(content)) 
        | grepl("ausland", tolower(title)) 
        | grepl("ausländer", tolower(content)) 
        | grepl("ausländer", tolower(title))
        | grepl("terror", tolower(content)) 
        | grepl("terror", tolower(title))
        | grepl("schleuser", tolower(content))
        | grepl("schleuser", tolower(title))
        | grepl("asyl", tolower(content))
        | grepl("asyl", tolower(title))
        | grepl("integration", tolower(content))
        | grepl("integration", tolower(title))
        | grepl("abschiebung", tolower(content))
        | grepl("abschiebung", tolower(title))
        | grepl("nordafrika", tolower(content))
        | grepl("nordafrika", tolower(title))
            )%>%
  mutate(upload_date = as_datetime(str_split_fixed(str_replace(str_replace(upload_time,',',''),' Uhr',''),' ',2)[,1]
                                   , "%d.%m.%Y"
                                   , tz = "Europe/Berlin"
  ))%>%
  unique()

data_flucht_bild <- data_fin_bild%>%
  filter(grepl("flüchtling", tolower(content))
        | grepl("flüchtling", tolower(title))
        | grepl("migranten", tolower(content))
        | grepl("migranten", tolower(title))
        | grepl("migration", tolower(content)) 
        | grepl("migration", tolower(title))
        | grepl("ausland", tolower(content)) 
        | grepl("ausland", tolower(title)) 
        | grepl("ausländer", tolower(content)) 
        | grepl("ausländer", tolower(title))
        | grepl("terror", tolower(content)) 
        | grepl("terror", tolower(title))
        | grepl("schleuser", tolower(content))
        | grepl("schleuser", tolower(title))
        | grepl("asyl", tolower(content))
        | grepl("asyl", tolower(title))
        | grepl("integration", tolower(content))
        | grepl("integration", tolower(title))
        | grepl("abschiebung", tolower(content))
        | grepl("abschiebung", tolower(title))
        | grepl("nordafrika", tolower(content))
        | grepl("nordafrika", tolower(title))
  )%>%
  unique()



data_flucht_spon$upload_date <- as_datetime(str_split_fixed(str_replace(str_replace(data_flucht_spon$upload_time,',',''),' Uhr',''),' ',2)[,1]
                                       , "%d.%m.%Y"
                                       , tz = "Europe/Berlin"
                                       )


data_flucht_bild$upload_date <- as_datetime(
  str_trim(case_when(
            is.na(str_split_i(str_split_i(data_flucht_bild$upload_time,' - ', 1), ', ',2)) == TRUE ~ str_split_i(str_split_i(data_flucht_bild$upload_time,' - ', 1), ', ',1)
            , is.na(str_split_i(str_split_i(data_flucht_bild$upload_time,' - ', 1), ', ',2)) == FALSE ~ str_split_i(str_split_i(data_flucht_bild$upload_time,' - ', 1), ', ',2)
  )
)
, "%d.%m.%Y"
, tz = "Europe/Berlin"
)

data_flucht_spon$outlet <- 'SPON'
data_flucht_bild$outlet <- 'BILD'
data_flucht_bild$topic <- 'N/A'

data_bild_spon_flucht <- rbind(data_flucht_spon,data_flucht_bild)%>%
  mutate(content = sub("Mehr aktuelle News.*", "", content) 
         ,content = sub("WERDEN AUCH SIE ZUM LESER-REPORTER!,.*", "", content) 
         )%>%
  filter(!grepl("International", resort))




saveRDS(data_flucht_spon, file = "table_data_flucht_SPON_sentiments_raw.rds")
saveRDS(data_flucht_bild, file = "table_data_flucht_BILD_sentiments_raw.rds")
saveRDS(data_bild_spon_flucht, file = "table_data_flucht_full_sentiments_raw.rds")

data_bild_spon_flucht <- readRDS(file = "table_data_flucht_full_sentiments_raw.rds")



# Exploratory data analysis -----------------------------------------------

library(dplyr)
library(ggplot2)



data_flucht %>%
 ggplot() +
 aes(x = resort, fill = resort) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 labs(x = "Ressort", 
 y = "Anzahl", title = "Artikel nach Ressorts", subtitle = "Gescrapedte artikel von Spiegel Online über Flüchtlinge", 
 caption = "Zeitraum: Okt 2015 - Feb 2016", fill = "Ressort") +
 ggthemes::theme_solarized()


data_timeseries <- data_bild_spon_flucht%>%
  select(upload_date, outlet)%>%
  group_by(upload_date, outlet)%>%
  count()%>%
  unique()


library(dplyr)
library(ggplot2)

data_timeseries %>%
 filter((upload_date >= "2015-06-30 22:00:00" & upload_date <= "2016-03-30 23:00:00") | 
 is.na(upload_date)) %>%
 ggplot() +
 aes(x = upload_date, y = n, color = outlet) +
 geom_line() +
 labs(x = "Datum", 
 y = "Menge an hochgeladener Artikel", title = "Presseaktivität", subtitle = "Spiegel Online Artikel über Flüchtlinge Pro Tag", 
 caption = "Okt 2015") +
 theme_light()

# Calculate Sentiment Score -----------------------------------------------

pb <- progress_bar$new(total = length(data_bild_spon_flucht$content))
sentiment.ai.score <- c()
url <- c()
data_sentiment <- tibble(sentiment.ai = sentiment.ai.score
       , url = url)

for (i in data_bild_spon_flucht$urls) {
  c <- data_bild_spon_flucht%>%
    filter(urls == i)%>%
    select(content, urls)
  # sentiment.ai
  data_sentiment <- try(rbind(data_sentiment
                          , tibble(sentiment.ai.score = sentiment_score(c$content)
                          , url = i))
  )
  pb$tick()
  Sys.sleep(1 / length(data_bild_spon_flucht$urls))                            
}

# sentiment.ai
# sentiment.ai.score <- sentiment_score(data_bild_spon_flucht$content)
# # From Sentiment Analysis
# sentimentAnalysis.score <- analyzeSentiment(data_flucht_bild$content, language = "german")$SentimentQDAP
# 
# # From sentimentr
# sentimentr.score <- sentiment_by(get_sentences(data_flucht_bild$content), 1:length(data_flucht_bild$content))$ave_sentiment

data_flucht_sent <- data_bild_spon_flucht%>%
  unique()%>%
  left_join(data_sentiment%>%unique(), join_by(urls == url))%>%
  rename(sentiment_ai = sentiment.ai.score)%>%
  mutate(pre_sylvester_2016 = case_when(ymd(upload_date) <= '2015-12-31' ~ "1"
                                        , ymd(upload_date) > '2015-12-31' ~ "0")
         , pre_merkel_speech = case_when(ymd(upload_date) <= '2015-08-31' ~ "1"
                                         , ymd(upload_date) > '2015-08-31' ~ "0")
  )%>%
  unique()


saveRDS(data_flucht_sent, file = "table_data_flucht_sent_sentiments_raw.rds")


# Timeseries & Correlations -----------------------------------------------

data_flucht_sent%>%
  arrange(-sentiment_ai)%>%
  select(urls, sentiment_ai)%>%
  head(20)

data_flucht_sent_ts <- data_flucht_sent%>%
  filter(grepl("flüchtling", tolower(content))
         | grepl("flüchtling", tolower(title))
         | grepl("migranten", tolower(content))
         | grepl("migranten", tolower(title))
         | grepl("migration", tolower(content)) 
         | grepl("migration", tolower(title))
         | grepl("ausland", tolower(content)) 
         | grepl("ausland", tolower(title)) 
         | grepl("ausländer", tolower(content)) 
         | grepl("ausländer", tolower(title))
         | grepl("terror", tolower(content)) 
         | grepl("terror", tolower(title))
         | grepl("schleuser", tolower(content))
         | grepl("schleuser", tolower(title))
         | grepl("asyl", tolower(content))
         | grepl("asyl", tolower(title))
         | grepl("integration", tolower(content))
         | grepl("integration", tolower(title))
         | grepl("abschiebung", tolower(content))
         | grepl("abschiebung", tolower(title))
         | grepl("nordafrika", tolower(content))
         | grepl("nordafrika", tolower(title))
  )%>%
  select(upload_date, outlet, sentiment_ai)%>%
  group_by(upload_date, outlet)%>%
  mutate(sentiment_ai = median(sentiment_ai))%>%
  unique()



library(dplyr)
library(ggplot2)
library(plotly)


library(dplyr)
library(ggplot2)

data_flucht_sent_ts %>%
 filter(upload_date >= "2015-06-30 00:00:00" & upload_date <= "2016-04-14 06:58:03" & 
 !is.na(upload_date)) %>%
 ggplot() +
 aes(x = upload_date, y = sentiment_ai, colour = outlet) +
 geom_point(shape = "circle", 
 size = 1.5) +
 scale_color_manual(values = c(BILD = "#42C9E5", SPON = "#F69AC9")) +
 labs(x = "d", 
 y = "e", title = "a", subtitle = "b", caption = "c", color = "f") +
 ggthemes::theme_fivethirtyeight() +
 facet_wrap(vars(outlet))







