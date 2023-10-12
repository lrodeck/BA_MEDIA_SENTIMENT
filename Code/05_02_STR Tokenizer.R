data_full <- readRDS("data/data_full.rds")

library(text)
library(tidyverse)
library(progress)
# Models stored here:
# https://docs.google.com/spreadsheets/d/1aZiK_qtRdHATMo1MLnnudAsbKjxxuOeTdzIhVctBPFw/edit#gid=0
# Install text required python packages in a conda environment (with defaults)
# textrpp_install()

# Initialize the installed conda environment
# save_profile = TRUE saves the settings so that you do not have to run textrpp_initialize() again after restarting R
textrpp_initialize(save_profile = TRUE)
NER <- textNER(x = data_full$content[1]
               , model = 'Babelscape/wikineural-multilingual-ner'
               , tokenizer_parallelism = T
               , set_seed = 2509)
data_actors <- tibble(NER$x_NER%>%
                       mutate(url = data_full$urls[1])%>%
                       select(url, entity, score, index, word, start, end)
                     )[0,]

n <- nrow(data_full)
pb <- progress_bar$new(total = n)
for (i in data_full$urls) {
  data_temp <- data_full%>%
    filter(urls == i)%>%
    select(urls, content)
  NER <- try(textNER(x = data_temp$content[1]
                            , model = 'Babelscape/wikineural-multilingual-ner'
                            , tokenizer_parallelism = T
                            , set_seed = 2509))
  data_ner <- tibble(NER$x_NER)
  data_actors <- try(rbind(data_actors, data_ner%>%
                        mutate(url = data_temp$urls[1])
  )
  )

  pb$tick()
  Sys.sleep(1 / n)
}

saveRDS(data_actors%>%
  mutate(rn = row_number())%>%
  filter(rn != 1)%>%
  mutate(score = as.double(score)
         , index = as.integer(index)
         , start = as.integer(start)
         , end = as.integer(end))%>%
  group_by(url)%>%
  mutate(actor = paste0(word
                        , if_else(str_starts("^[[:upper:]]+$", lead(word)) & !is.na(lead(word)) & (index == lead(index) -1 | index == lead(index)) , lead(word),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word)) & !is.na(lead(word)) & (index == lead(index) -1 | index == lead(index)) , paste0(' ', lead(word)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 2)) & !is.na(lead(word, n = 2)) & (index == lead(index, n = 2) -2 | index == lead(index, n = 2)) , lead(word, n = 2),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 2)) & !is.na(lead(word, n = 2)) & (index == lead(index, n = 2) -2 | index == lead(index, n = 2)) , paste0(' ', lead(word, n = 2)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 3)) & !is.na(lead(word, n = 3)) & (index == lead(index, n = 3) -3 | index == lead(index, n = 3)) , lead(word, n = 3),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 3)) & !is.na(lead(word, n = 3)) & (index == lead(index, n = 3) -3 | index == lead(index, n = 3)) , paste0(' ', lead(word, n = 3)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 4)) & !is.na(lead(word, n = 4)) & (index == lead(index, n = 4) -4 | index == lead(index, n = 4)) , lead(word, n = 4),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 4)) & !is.na(lead(word, n = 4)) & (index == lead(index, n = 4) -4 | index == lead(index, n = 4)) , paste0(' ', lead(word, n = 4)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 5)) & !is.na(lead(word, n = 5)) & (index == lead(index, n = 5) -5 | index == lead(index, n = 5)) , lead(word, n = 5),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 5)) & !is.na(lead(word, n = 5)) & (index == lead(index, n = 5) -5 | index == lead(index, n = 5)) , paste0(' ', lead(word, n = 5)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 6)) & !is.na(lead(word, n = 6)) & (index == lead(index, n = 6) -6 | index == lead(index, n = 6)) , lead(word, n = 6),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 6)) & !is.na(lead(word, n = 6)) & (index == lead(index, n = 6) -6 | index == lead(index, n = 6)) , paste0(' ', lead(word, n = 6)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 7)) & !is.na(lead(word, n = 7)) & (index == lead(index, n = 7) -7 | index == lead(index, n = 7)) , lead(word, n = 7),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 7)) & !is.na(lead(word, n = 7)) & (index == lead(index, n = 7) -7 | index == lead(index, n = 7)) , paste0(' ', lead(word, n = 7)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 8)) & !is.na(lead(word, n = 8)) & (index == lead(index, n = 8) -8 | index == lead(index, n = 8)) , lead(word, n = 8),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 8)) & !is.na(lead(word, n = 8)) & (index == lead(index, n = 8) -8 | index == lead(index, n = 8)) , paste0(' ', lead(word, n = 8)),'')
                        , if_else(str_starts("^[[:upper:]]+$", lead(word, n = 9)) & !is.na(lead(word, n = 9)) & (index == lead(index, n = 9) -9 | index == lead(index, n = 9)) , lead(word, n = 9),'')
                        , if_else(!str_starts("^[[:upper:]]+$", lead(word, n = 9)) & !is.na(lead(word, n = 9)) & (index == lead(index, n = 9) -9 | index == lead(index, n = 9)) , paste0(' ', lead(word, n = 9)),'')
  )
  , actor = str_replace_all(actor, " ##","")
  )%>%
  ungroup()%>%
  filter(entity == "B-PER" 
         | entity == "B-LOC"
         | entity == "B-ORG"
         | entity == "B-MISC")
, "data/data_tokens_nested.RDS")





data_tokens_class_refined <- readRDS("data/data_tokens_nested.RDS")




