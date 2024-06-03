library(spacyr)
library(quanteda)
library(tictoc)
library(progress)
library(tidyverse)
# Only run once:
# spacy_install()
# spacy_download_langmodel(lang_models =  "de_core_news_sm")
spacy_initialize(model = "de_core_news_sm")
data_sentiment_topics <- readRDS("Data/data_full.rds")

data_sentiment_topics <- data_sentiment_topics%>%
  rownames_to_column%>%
  mutate(textid = paste0("text",rowname))%>%
  select(-rowname)

parsedtxt_all <- spacy_parse(data_sentiment_topics$content[1]
                         , nounphrase = TRUE
                         , dependency = TRUE
                         , multithread = TRUE
                         , 
                         , additional_attributes = c("sentiment"))[0,]
parsedtxt_actors_all <- nounphrase_consolidate(parsedtxt_all)[0,]

for (i in data_sentiment_topics$textid) {
  tic()
  text <- data_sentiment_topics%>%
    filter(textid == i)
  parsedtxt <- spacy_parse(text$content
                           , nounphrase = TRUE
                           , dependency = TRUE
                           , multithread = TRUE
                           , additional_attributes = c("sentiment")
                           )
  parsedtxt_actors <- nounphrase_consolidate(parsedtxt
                                             , concatenator = " ")
  parsedtxt_all <- rbind(parsedtxt_all
                         , parsedtxt)
  parsedtxt_actors_all <- rbind(parsedtxt_actors_all
                                , parsedtxt_actors)
  
  
  print(i)
  toc()
}
parsedtxt_actors_all <- parsedtxt_actors_all%>%
  mutate(doc_id = paste0("text", cumsum
                         (case_when
                           (sentence_id == 1 & (lag(sentence_id) > 1 | is.na(lag(sentence_id))) ~ 1
                             , .default = 0)
                         )
  )
  )  
parsedtxt_all <- parsedtxt_all%>%
  mutate(doc_id = paste0("text", cumsum
                         (case_when
                           (sentence_id == 1 & (lag(sentence_id) > 1 | is.na(lag(sentence_id))) ~ 1
                             , .default = 0)
                         )
  )
  )

saveRDS(parsedtxt_actors_all
        , "Data/parsedtxt_actors_all.rds")
saveRDS(parsedtxt_all
        , "Data/parsedtxt_all.rds")



spacy_finalize()

