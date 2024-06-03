library(readr)
library(tidyverse)
library(tidytext)
library(ggpage)
library(sjmisc)
library(pradadata)
#remotes::install_github("abhy/sentiment")
German_NRC_EmoLex <- pradadata::germanlex
data_full <- readRDS("data/data_full.rds")

p1 <- data_full%>%
  unique()%>%
  filter(urls == "https://www.bild.de/politik/inland/menschen-bei-maischberger/csu-politiker-liebaeugelt-mit-afd-43723768.bild.html")%>%
  select(urls, title, content, upload_date, sentiment_ai, Topic)%>%
  rename(text = content)%>%
  tidytext::unnest_tokens(text, text)%>% 
  ggpage_build(wtl = T, )%>%
  left_join(tidyTX::hash_lemma_de, join_by(word == word))%>%
  mutate(word = ifelse(is.na(lemma), word, lemma))%>%
  left_join(Emotion_Lex, join_by(word == word))%>%
  select(word, page, line, xmin, xmax, ymin, ymax, qualifier, polarity_strength)%>%
  unique()%>%
  mutate(qualifier = case_when(qualifier == "NEG" ~ "Negativ"
                               , qualifier == "POS" ~ "Positiv"
                               , .default =  "Neutral"))%>%
  ggpage_plot(aes(fill = qualifier, alpha = polarity_strength), page.number = "top-left" , paper.show = T) +
  scale_fill_manual(values = c("Positiv" = "green4", "Negativ" = "red3", "Neutral" = "grey50")) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.line = element_blank()
        , axis.text = element_blank()
        , axis.ticks = element_blank()
        , axis.title = element_blank()
        , panel.grid = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        ) +
  labs(title = "Bild Online: Seiten Analyse"
       , subtitle = "Bild Aritkel Artikel nach Lemma Sentiment"
       , caption = "Emotions Lexikon Quelle: Sauer 2024"
       , fill = "Sentiment"
       , alpha = "Stärke")

p2 <- data_full%>%
  unique()%>%
  filter(urls == "https://www.spiegel.de/politik/deutschland/spd-fraktionschef-oppermann-gabriel-ist-der-natuerliche-kanzlerkandidat-a-1068501.html")%>%
  select(urls, title, content, upload_date, sentiment_ai, Topic)%>%
  rename(text = content)%>%
  tidytext::unnest_tokens(text, text)%>% 
  ggpage_build(wtl = T, )%>%
  left_join(tidyTX::hash_lemma_de, join_by(word == word))%>%
  mutate(word = ifelse(is.na(lemma), word, lemma))%>%
  left_join(Emotion_Lex, join_by(word == word))%>%
  select(word, page, line, xmin, xmax, ymin, ymax, qualifier, polarity_strength)%>%
  unique()%>%
  mutate(qualifier = case_when(qualifier == "NEG" ~ "Negativ"
                               , qualifier == "POS" ~ "Positiv"
                               , .default =  "Neutral"))%>%
  ggpage_plot(aes(fill = qualifier, alpha = polarity_strength)
              , page.number = "top-left" 
              , paper.show = T
              ) +
  scale_fill_manual(values = c("Positiv" = "green4", "Negativ" = "red3", "Neutral" = "grey50")) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.line = element_blank()
        , axis.text = element_blank()
        , axis.ticks = element_blank()
        , axis.title = element_blank()
        , panel.grid = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
  ) +
  labs(title = "Spiegel Online: Seiten Analyse"
       , subtitle = "Spiegel Aritkel Artikel nach Lemma Sentiment"
       , fill = "Sentiment"
       , alpha = "Stärke")

patchwork::wrap_plots(p1, p2, guides = "collect")

data_full%>%
  unique()%>%
  filter(urls == "https://www.spiegel.de/politik/deutschland/spd-fraktionschef-oppermann-gabriel-ist-der-natuerliche-kanzlerkandidat-a-1068501.html")%>%
  select(urls, title, content, upload_date, sentiment_ai, Topic)%>%
  rename(text = content)%>%
  tidytext::unnest_tokens(text, text)%>% 
  ggpage_build(wtl = T, )%>%
  left_join(tidyTX::hash_lemma_de, join_by(word == word))%>%
  mutate(word = ifelse(is.na(lemma), word, lemma))%>%
  left_join(Emotion_Lex, join_by(word == word))%>%
  select(word, page, line, xmin, xmax, ymin, ymax, qualifier, polarity_strength)%>%
  unique()%>%
  mutate(qualifier = case_when(qualifier == "NEG" ~ "Negativ"
                               , qualifier == "POS" ~ "Positiv"
                               , .default =  "Neutral"))%>%
  select(qualifier, polarity_strength)%>%
  group_by(qualifier)%>%
  summarise(sum = sum(polarity_strength, na.rm = T)
            , mean = mean(polarity_strength, na.rm = T)
            , count = n() )
