# load packages
library(tidyverse)
library(flextable)
library(here)
library(progress)
library(text)
textrpp_initialize(save_profile = TRUE)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load text
text_1 <- readRDS("data_full.rds")%>%select(content)%>%sample_n(1)
# clean data
text_1 <- text_1 %>%
  str_squish()


# tokenise, tag, dependency parsing
text_anndf <- textNER(x = text_1
                      , model = 'wietsedv/xlm-roberta-base-ft-udpos28-de'
                      , tokenizer_parallelism = T
                      , set_seed = 2509)

POS_ANN_DF <- text_anndf[0,]
# load text
text <- readRDS("data_full.rds")
n <- nrow(text)
pb <- progress_bar$new(total = n)
for (i in text$urls) {
  print(i)

  text_selected <- text %>%
    filter(urls == i)%>%
    select(content)%>%
    str_squish()
  
  # tokenise, tag, dependency parsing
  text_anndf <- try(textNER(x = text_selected$content
                            , model = 'wietsedv/xlm-roberta-base-ft-udpos28-de'
                            , tokenizer_parallelism = T
                            , set_seed = 2509))
  POS_ANN_DF <- rbind(POS_ANN_DF,text_anndf)
  
  pb$tick()
  Sys.sleep(1 / n)
}

POS_ANN_DF%>%
  select(doc_id)%>%
  unique()%>%
  count()

saveRDS(POS_ANN_DF, "POS_DATA.rds")

