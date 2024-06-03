library(tidyverse)
library(stm)
library(wordcloud)

ArticleSelect <- readRDS("Models/ArticleSelect.rds")

plotModels(ArticleSelect
           , pch = c(1, 2, 3, 4, 5, 6, 7, 8)
           , legend.position = "bottomright"
           , main = "Themenanteile am Datenset")


out <- readRDS("Data/stm_out.rds")
documents <- out$documents
vocab <- out$vocab
meta <- out$meta
set.seed(2509)
K<-c(3,4,5,6,7,8,9,10,15,20) 
kresult <- searchK(documents
                   , vocab
                   , K
                   , prevalence=~s(sentiment_ai) + factor(outlet) + resort 
                   , data=meta)
saveRDS(kresult,"Models/searchK.rds")
kresult <- readRDS("Models/searchK.rds")
plot(kresult
     , main="K Auswahl"
     )



