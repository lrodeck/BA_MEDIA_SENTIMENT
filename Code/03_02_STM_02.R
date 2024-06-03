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
saveRDS(out,"Data/stm_out.rds")
# Estimate ----
## Estimation with prevalence parameter ----
ArticlePrevFit <- stm(documents = out$documents
                       , vocab = out$vocab
                       , K = 10
                       , prevalence = ~s(sentiment_ai) + factor(outlet) + resort 
                       , max.em.its = 75
                       , data = out$meta
                       , init.type = "Spectral")
# Evaluate ----
## Model selection for a fixed number of topics ----
ArticleSelect <- selectModel(out$documents
                             , out$vocab
                             , K = 8
                             , prevalence = ~sentiment_ai + factor(outlet) + resort 
                             , max.em.its =                      
                             , data = out$meta
                             , runs = 20
                             , seed = 2509)


plotModels(ArticleSelect
           , pch = c(1, 2, 3, 4, 5, 6, 7, 8)
           , legend.position = "bottomright")

selectedmodel <- ArticleSelect$runout[[4]]


## Model search across numbers of topics ----
storage <- searchK(out$documents
                   , out$vocab
                   , K = c(3:15)
                   , prevalence = ~sentiment_ai + factor(outlet) + resort 
                   , data = meta)
   
plot(storage)

articlesPrevFit <- stm(documents = out$documents
                       , vocab = out$vocab
                       , K = 4
                       , prevalence = ~sentiment_ai + factor(outlet) + resort 
                       , max.em.its = 75
                       , data = out$meta
                       , init.type = "Spectral")

# Interpretation ----
## Topic Labels
labelTopics(articlesPrevFit, c(1,2,3,4))


# metadata topice relationship ----
out$meta$sentiment_ai <- as.double(out$meta$sentiment_ai)
prep <- estimateEffect(1:4 ~ sentiment_ai + factor(outlet)
                       , articlesPrevFit
                       , meta = out$meta
                       , uncertainty = "Global"
                       , nsims = 25)
summary(prep, topics = 1)
saveRDS(prep, "Models/prep.rds")
saveRDS(articlesPrevFit, "Models/articlesPrevFit.rds")
saveRDS(ArticleSelect, "Models/ArticleSelect.rds")

# visualisation ----
## Summary visualization ----
plot(articlesPrevFit
     , type = "summary"
     , xlim = c(0, 0.5))

plot(articlesPrevFit
     , type = "perspectives"
     , topics = c(3,4)
     , xlim = c(0, 0.5)
     , labeltype = "score"
     , custom.labels = c("Europäische Flüchtlingskrise"
                         , "Rechtsextremismus"))


## Metadata/topic relationship visualization ----
plot(articlesPrevFit
     , covariate = "sentiment_ai"
     , topics = c(1,2,3,4)
     , model = articlesPrevFit
     , method = "difference"
     , type = "summary"
     , cov.value1 = "Positive"
     , cov.value2 = "Negative"
     , xlab = "Eher Negativ ... Eher Positiv"
     , main = "Sentiment Zusammenhang der berechneten Themen"
     , xlim = c(-1, 1)
     , labeltype = "score"
     , custom.labels = c("Sonstiges (US Wahlen, Kultur)"
                         , "Rechtsextremismus (NSU, Pegida)"
                         , "Flüchtlingskrise"
                         , "Syrischer Bürgerkrieg"))


## WordCloud ----

cloud(articlesPrevFit, topic = 4, scale = c(2, 0.25), max.words = 50)









#First, we generate an empty data frame for both models
theta <- make.dt(articlesPrevFit)
data_sentiment_topics <- meta
data_sentiment_topics$Topic <- colnames(theta%>%select(-docnum))[apply(theta%>%select(-docnum),1,which.max)]


data_sentiment_topics <- data_sentiment_topics%>%
  mutate(Topic = case_when(Topic == "Topic1" ~ "Sonstiges (US Wahlen, Kultur)"
                           , Topic == "Topic2" ~ "Rechtsextremismus (NSU, Pegida)"
                           , Topic == "Topic3" ~ "Europäische Flüchtlingskrise"
                           , Topic == "Topic4" ~ "Syrischer Bürgerkrieg"))


saveRDS(data_sentiment_topics,"Data/data_full.rds")
