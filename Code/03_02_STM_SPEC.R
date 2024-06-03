# Libraries ----
library(tidyverse)
library(stm)
library(wordcloud)
# Reading in data  ----
data_flucht_sent <- readRDS("Data/table_data_flucht_sent_sentiments_raw.rds")
## clean data ----

data_flucht_sent <- data_flucht_sent%>%
  filter(!grepl("International", resort))%>%
  unique()

data_flucht_sent <- data_flucht_sent%>%
  mutate(days = as.integer(difftime(upload_date, as.Date("2015-07-01"), , units = "days"))
         , resort = as.factor(resort))

#removing the pattern indicating a line break
data_flucht_sent$content <- gsub(pattern = "\n", replacement = " ", x = data_flucht_sent$content)


## Pre-processing text content ----
processed <- textProcessor(data_flucht_sent$content
                           , metadata = data_flucht_sent
                           , language = "german"
                           , customstopwords = c("wurde", "wurden", "worden", "hat", "ist", "habe"
                                                 , "dass", "der", "die", "das", "und", "seien"
                                                 , "mehr", "immer", "sei", "sagte", "seit"
                                                 , "schon", "wegen", "gibt", "sagt", "schon"
                                                 , "eins", "zwei", "drei", "vier", "fünf", "sechs"
                                                 , "sieben", "acht", "neun", "zehn", "b"
                                                 , "wer", "geht", "müssen", "bereits", "mal"
                                                 , "macht", "bild", "spiegel", "heute")) 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Prepare ----
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)
# Estimate ----
# Evaluate ----
## Model selection for a fixed number of topics ----



articlesPrevFit_spec <- stm(documents = out$documents
                       , vocab = out$vocab
                       , K = 8
                       , prevalence = ~sentiment_ai + factor(outlet) + resort 
                       , max.em.its = 75
                       , data = out$meta
                       , init.type = "Spectral")

# Interpretation ----
## Topic Labels
labelTopics(articlesPrevFit_spec, c(1,2,3,4,5,6,7,8))


# metadata topice relationship ----
out$meta$sentiment_ai <- as.double(out$meta$sentiment_ai)
prep_spec <- estimateEffect(1:8 ~ sentiment_ai + factor(outlet)
                       , articlesPrevFit_spec
                       , meta = out$meta
                       , uncertainty = "Global"
                       , nsims = 25)
summary(prep_spec, topics = 1)
saveRDS(prep_spec, "Models/prep_spec.rds")
saveRDS(articlesPrevFit_spec, "Models/articlesPrevFit_spec.rds")

# visualisation ----
## Summary visualization ----
plot(articlesPrevFit_spec
     , type = "summary"
     , xlim = c(0, 0.5))

plot(articlesPrevFit_spec
     , type = "perspectives"
     , topics = c(3,4)
     , xlim = c(0, 0.5)
     , labeltype = "score"
     , custom.labels = c("Europäische Flüchtlingskrise"
                         , "Rechtsextremismus"))


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


## WordCloud ----

cloud(articlesPrevFit_spec, topic = 1, scale = c(2, 0.25), max.words = 50)









#First, we generate an empty data frame for both models
theta_spec <- make.dt(articlesPrevFit_spec)
data_sentiment_topics <- readRDS("Data/data_full.rds")
data_sentiment_topics$Topic_Specific <- colnames(theta_spec%>%select(-docnum))[apply(theta_spec%>%select(-docnum),1,which.max)]


data_sentiment_topics <- data_sentiment_topics%>%
  mutate(Topic_Specific = case_when(
                             Topic_Specific == "Topic1" ~ "Sonstiges"
                           , Topic_Specific == "Topic2" ~ "Rechtsextremismus (NSU, Pegida)"
                           , Topic_Specific == "Topic3" ~ "Europäische Flüchtlingskrise"
                           , Topic_Specific == "Topic4" ~ "Syrischer Bürgerkrieg"
                           , Topic_Specific == "Topic5" ~ "Islamistischer Terror"
                           , Topic_Specific == "Topic6" ~ "US Wahlen"
                           , Topic_Specific == "Topic7" ~ "Bundes Politik"
                           , Topic_Specific == "Topic8" ~ "Euro Krise")
         )


saveRDS(data_sentiment_topics,"Data/data_full.rds")
