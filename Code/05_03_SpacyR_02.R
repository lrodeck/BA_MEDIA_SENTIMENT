library(tidyverse)
parsedtxt_actors_all <- readRDS("~/R Projects/BACHELOR_ARBEIT/Data/parsedtxt_actors_all.rds")%>%
  mutate(doc_id = paste0("text", cumsum
                         (case_when
                           (sentence_id == 1 & (lag(sentence_id) > 1 | is.na(lag(sentence_id))) ~ 1
                             , .default = 0)
                         )
  )
  )
parsedtxt_all <- readRDS("~/R Projects/BACHELOR_ARBEIT/Data/parsedtxt_all.rds")%>%
  mutate(doc_id = paste0("text", cumsum
                         (case_when
                           (sentence_id == 1 & (lag(sentence_id) > 1 | is.na(lag(sentence_id))) ~ 1
                             , .default = 0)
                         )
  )
  )

German.NRC.EmoLex <- read.delim("~/R Projects/BACHELOR_ARBEIT/Lexica/German-NRC-EmoLex.txt")
German.NRC.EmoLex%>%
parsedtxt_actors_all

