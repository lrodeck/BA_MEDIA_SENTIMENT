data_flucht_sent <- readRDS("Data/table_data_flucht_sent_sentiments_raw.rds")

library(topicmodels)
library(tidyverse)
library(reshape2)
library(stm)
library(stmCorrViz)
# Pre-processing ----



library(stringr)
library(quanteda)

# clean data ----

data_flucht_sent <- data_flucht_sent%>%
  filter(!grepl("International", resort))%>%
  unique()

#removing the pattern indicating a line break
data_flucht_sent$content <- gsub(pattern = "\n", replacement = " ", x = data_flucht_sent$content)

## tokenization ----

#tokenization & removing punctuation/numbers/URLs etc.
tokens <- data_flucht_sent$content %>%
  tokens(what = "word",
         remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         include_docvars = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("german"))%>%
  tokens_remove(stopwords("english"))
  

#applying relative pruning
dfm <- dfm_trim(dfm(tokens), min_docfreq = 0.005, max_docfreq = 0.99, 
                docfreq_type = "prop", verbose = TRUE)

topfeatures(dfm, n = 20, scheme = "docfreq")

dfm <- dfm_remove(dfm, c("wurde", "wurden", "worden", "hat", "ist", "habe"
                         , "dass", "der", "die", "das", "und", "seien"
                         , "mehr", "immer", "sei", "sagte", "seit", "►"
                         , "schon", "wegen", "gibt", "sagt", "schon"
                         , "eins", "zwei", "drei", "vier", "fünf", "sechs"
                         , "sieben", "acht", "neun", "zehn", "+", "b"
                         , "wer", "geht", "müssen", "bereits", "mal"
                         , "macht", "bild"))


dfm_stm <- convert(dfm, to = "stm", docvars = data_flucht_sent)

stmCorrViz(dfm_stm$model, "corrviz.html",
           documents_raw= dfm_stm$raw_documents,
           documents_matrix= dfm_stm$documents_matrix
)


# create model ----



K <- c(2:10)
fit <- searchK(dfm_stm$documents, dfm_stm$vocab, K = K)

# Create graph
plot <- data.frame("K" = K, 
                   "Coherence" = unlist(fit$results$semcoh),
                   "Exclusivity" = unlist(fit$results$exclus))



# Reshape to long format
plot <- melt(plot, id=c("K"))
saveRDS(plot,"Data/topics_plot.rds")

#Plot result
ggplot(plot, aes(K, value, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~variable,scales = "free_y") +
  labs(x = "Menge an Topics K",
       title = "Statistischer Fit Modelle mit verschiedenen K")

# decide on the number of topics K ----
## Interpretability and relevance of topics ----
model_2 <- stm(documents = dfm_stm$documents,
               vocab = dfm_stm$vocab, 
               K = 2,
               verbose = TRUE)

saveRDS(model,"Data/topics_model.rds")



library(topicmodels)
library(tidyverse)
library(reshape2)
library(stm) 

model <- readRDS("Data/topics_model.rds")
plot(model_3,type="labels")

#for K = 4
topics <- labelTopics(model, n=10)
topics <- data.frame("features" = t(topics$frex))
colnames(topics) <- paste("Topics", c(1:4))
topics


theta <- make.dt(model)


#First, we generate an empty data frame for both models
data_sentiment_topics <- data_flucht_sent
data_sentiment_topics$topic <- NA #K = 4

#calculate Rank-1 for K = 4
for (i in 1:nrow(data_sentiment_topics)){
  column <- theta[i,-1]
  maintopic <- colnames(column)[which(column==max(column))]
  data_sentiment_topics$topic[i] <- maintopic
}
data_sentiment_topics <- data_sentiment_topics%>%
  mutate(Topic = case_when(topic == "Topic1" ~ "Rechtsextremismus"
                           , topic == "Topic2" ~ "Islamistischer Terror"
                           , topic == "Topic3" ~ "Europäische Flüchtlingskrise"
                           , topic == "Topic4" ~ "US Wahl"))


saveRDS(data_sentiment_topics,"data_full.rds")



