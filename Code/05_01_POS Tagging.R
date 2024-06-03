# load packages
library(tidyverse)
library(flextable)
library(here)
library(progress)
library(text)
library(udpipe)
textrpp_initialize(save_profile = TRUE)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load text
text_1 <- readRDS("data_full.rds")%>%select(content)%>%sample_n(1)
# clean data
text_1 <- text_1 %>%
  str_squish()

m_de <- udpipe_load_model(file = "Models/german-gsd-ud-2.5-191206.udpipe")

# tokenise, tag, dependency parsing
text_anndf <- udpipe::udpipe_annotate(m_de, x = text_1, doc_id = ) %>%
  as.data.frame() %>%
  dplyr::select(-sentence)

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
  url_selected <- text %>%
    filter(urls == i)%>%
    select(urls)%>%
    str_squish()
  
  # tokenise, tag, dependency parsing
  text_anndf <- try(udpipe::udpipe_annotate(m_de, x = text_selected, doc_id = url_selected) %>%
                      as.data.frame() %>%
                      dplyr::select(-sentence))
  POS_ANN_DF <- rbind(POS_ANN_DF,text_anndf)
  
  pb$tick()
  Sys.sleep(1 / n)
}

POS_ANN_DF%>%
  select(doc_id)%>%
  unique()%>%
  count()

POS_ANN_DF%>%
  unique()

saveRDS(POS_ANN_DF%>%
          unique(), "Data/POS_DATA.rds")

