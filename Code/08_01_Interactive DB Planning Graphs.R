library(tidyverse)
library(highcharter)
library(echarts4r)
data_full <- readRDS("data/data_full.rds")

data_ts <- data_full%>%
  filter(upload_date <= as.Date("2016-04-03"))%>%
  select(sentiment_ai, outlet, upload_date)%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  group_by(outlet, upload_date)%>%
  summarise(sentiment = round(mean(sentiment_ai),2))



data_ts|> 
  group_by(outlet)%>%
  e_chart(x = upload_date) %>%
  e_line(sentiment)%>%
  e_mark_point(serie = c("BILD","SPON"), data = list(
    name = "Max",
    type = "max"
  )) %>% 
  e_mark_point(serie = c("BILD","SPON"), data = list(
    name = "Min",
    type = "min"
  )) %>%
  e_title("Zeitreihe Sentiment Entwicklung der Flüchtlingskrise 2015/2016"
          , "Mittleres AI Berechnetes Sentiment Pro Woche nach Zeitung
          ")%>%
  e_tooltip(trigger = "axis")%>%
  e_theme("vintage")%>%
  e_legend(
    left = 0, 
    top = 170
  )%>%
  #e_animation(duration = 2000)%>%
  e_toolbox()%>%
  e_toolbox_feature(feature = "dataView")%>%
  e_toolbox_feature(feature = "saveAsImage")%>%
  e_datazoom(x_index = 0, type = "slider")




data_full%>%
  filter(Topic == "Europäische Flüchtlingskrise" & upload_date <= as.Date("2016-04-03"))%>%
  select(sentiment_ai, outlet, upload_date)%>%
  mutate(upload_date = round_date(upload_date, unit = "weeks"))%>%
  group_by(outlet, upload_date)%>%
  summarise(sentiment = round(mean(sentiment_ai),2))%>%
  group_by(outlet)%>%
  e_chart(x = upload_date) %>%
  e_line(sentiment)%>%
  e_mark_point(serie = c("BILD","SPON"), data = list(
    name = "Max",
    type = "max"
  )) %>% 
  e_mark_point(serie = c("BILD","SPON"), data = list(
    name = "Min",
    type = "min"
  )) %>%
  e_title("Zeitreihe Sentiment Entwicklung des Themas Europäische Flüchtlingskrise"
          , "Mittleres AI Berechnetes Sentiment Pro Woche nach Zeitung")%>%
  e_tooltip(trigger = "axis")%>%
  e_theme("vintage")%>%
  e_legend(
    left = 400, 
    top = 25
  )%>%
  #e_animation(duration = 2000)%>%
  e_toolbox()%>%
  e_toolbox_feature(feature = "dataView")%>%
  e_toolbox_feature(feature = "saveAsImage")%>%
  e_datazoom(x_index = 0, type = "slider")




data_full%>%
  filter(Topic == "Europäische Flüchtlingskrise")%>%
  e_color_range(sentiment_ai, color, colors = c("#a40000", "#f19e65", "#339966"))%>%
  e_chart()%>%
  e_cloud(title, sentiment_ai, color, shape = "circle", sizeRange = c(1, 9))%>%
  e_mark_point(serie = c("BILD","SPON"), data = list(
    name = "Max",
    type = "max"
  )) %>% 
  e_mark_point(serie = c("BILD","SPON"), data = list(
    name = "Min",
    type = "min"
  )) %>%
  e_title("Artikel Cloud"
          , "Titel der Artikel von SPON und BILD Online mit Sentiment Farbcodierung")%>%
  e_tooltip(trigger = "item")%>%
  e_theme("vintage")%>%
  e_legend()%>%
  e_toolbox()%>%
  e_toolbox_feature(feature = "dataView")%>%
  e_toolbox_feature(feature = "brush")%>%
  e_toolbox_feature(feature = "saveAsImage")


# network_graph ----
## network actors ----

library(echarts4r)

data_tokens_class_refined <- readRDS("data/data_tokens_nested.RDS")
data_pos_tagged <- readRDS("data/POS_DATA.RDS")


data_tagged_actors <-data_tokens_class_refined%>%
  left_join(data_full, join_by(url == urls))%>%
  filter(Topic == "Europäische Flüchtlingskrise")%>%
  left_join(data_tokens_class_refined, join_by(url), relationship = "many-to-many")%>%
  filter(actor.x != actor.y)%>%
  rename(entity_rep = entity.x
         , entity_partner = entity.y
         , rep = actor.x
         , partner = actor.y)%>%
  select(entity_rep, rep, entity_partner, partner)%>%
  group_by_all()%>%
  count()%>%
  ungroup()%>%
  filter(n > 2, !grepl("##", rep), !grepl("##", partner))



nodes <- c(data_tagged_actors$rep, data_tagged_actors$partner) %>%
  unique() %>%
  tibble(label = .) %>%
  rowid_to_column("id")%>%
  left_join(data_tokens_class_refined%>%select(entity, actor)%>%unique(), join_by(label== actor))%>%
  rename(group = entity)%>%
  group_by(id, label)%>%
  mutate(rn = row_number())%>%
  filter(rn == 1)%>%
  select(-rn)%>%
  unique()%>%
  ungroup()%>%
  mutate(value = id
         , size = 6)

edges <- data_tagged_actors %>%
  left_join(nodes, by = c("rep"="label")) %>%
  rename(from = "id") %>%
  left_join(nodes, by = c("partner"="label")) %>%
  rename("to" = "id") %>%
  mutate(N = n)%>%
  select(from, to, N)


e_chart()%>%
  e_graph(roam = TRUE,
          lineStyle = list(
            color = "source"
          ),
          label = list(
            position = "right",
            formatter = "{b}"
          )) |> 
  e_graph_nodes(nodes
                , names = label
                , category = group
                , value = value
                , size = size
                , legend = TRUE) |> 
  e_graph_edges(edges
                , from
                , to) |> 
  e_grid(height = "10%", top = "15%") |>
  e_tooltip() |>
  e_title("Netzwerk der in den Texten genannten Akteure"
          , "Mit Token Classifier Klassifiezierte Orte, Personen, Organisationen o.ä,")|> 
  e_theme("vintage") |> # add a theme
  e_brush() |>
  e_legend(type = "scroll")|> 
  e_toolbox_feature(feature = "saveAsImage")|> 
  e_toolbox_feature(feature = "dataView")

## Networks Verbs Adjectives ----
library(tidyverse)
library(quanteda.sentiment)
data_tokens_class_refined <- readRDS("data/data_tokens_nested.RDS")
data_pos_tagged <- readRDS("data/POS_DATA.RDS")

actor_verbs <- data_pos_tagged%>%
  select(doc_id, paragraph_id, sentence_id, token_id, token, upos, xpos, dep_rel, feats)%>%
  filter(upos != "PUNCT" | token == "-")%>%
  group_by(doc_id, paragraph_id, sentence_id)%>%
  inner_join(data_tokens_class_refined%>%select(url, entity, score, index, actor, word), join_by(doc_id == url
                                                                                                 , token == word))%>%
  select(doc_id, paragraph_id, sentence_id, actor)%>%
  unique()%>%
  inner_join(data_pos_tagged%>%
               select(doc_id, paragraph_id, sentence_id, token_id, token, upos, xpos, dep_rel, feats)%>%
               filter(upos == "VERB", dep_rel != "conj")%>%
               group_by(doc_id, paragraph_id, sentence_id)
             , join_by(doc_id, paragraph_id, sentence_id))%>%
  rename(verb = token)%>%
  ungroup()%>%
  select(actor, verb)%>%
  group_by(actor, verb)%>%
  count()%>%
  unique()%>%
  ungroup()%>%
  filter(n > 3)


actor_adj <- data_pos_tagged%>%
  select(doc_id, paragraph_id, sentence_id, token_id, token, upos, xpos, dep_rel, feats)%>%
  filter(upos != "PUNCT" | token == "-")%>%
  group_by(doc_id, paragraph_id, sentence_id)%>%
  inner_join(data_tokens_class_refined%>%select(url, entity, score, index, actor, word), join_by(doc_id == url
                                                                                                 , token == word))%>%
  select(doc_id, paragraph_id, sentence_id, actor)%>%
  unique()%>%
  inner_join(data_pos_tagged%>%
               select(doc_id, paragraph_id, sentence_id, token_id, token, upos, xpos, dep_rel, feats)%>%
               filter(upos == "ADJ")%>%
               group_by(doc_id, paragraph_id, sentence_id)
             , join_by(doc_id, paragraph_id, sentence_id))%>%
  rename(adj = token)%>%
  ungroup()%>%
  select(actor, adj)%>%
  group_by(actor, adj)%>%
  count()%>%
  unique()%>%
  ungroup()%>%
  filter(n > 3)

### network verbs ----

dict_sentiment_pos <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$positive
                             , pos = 1)
dict_sentiment_neg <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$negative
                             , neg = 1)

nodes <- c(actor_verbs$actor, actor_verbs$verb) %>%
  unique() %>%
  tibble(label = .) %>%
  rowid_to_column("id")%>%
  left_join(dict_sentiment_pos, join_by(label == word))%>%
  left_join(dict_sentiment_neg, join_by(label == word))%>%
  select(id, label, pos, neg)%>%
  group_by(id, label)%>%
  mutate(rn = row_number())%>%
  filter(rn == 1)%>%
  select(-rn)%>%
  unique()%>%
  ungroup()%>%
  mutate(value = case_when(pos == 1 ~ 1
                           , neg == 1 ~ -1
                           , label %in% actor_adj$actor ~ -2
                           , .default = 0)
         , group = case_when(value == 1 ~ "Positive"
                             , value == -1 ~ "Negative"
                             , value == 0 ~ "Neutral"
                             , value == -2 ~ "Actor")
         , size = abs(value)*5
  )%>%
  select(-pos,-neg)


edges <- actor_verbs %>%
  left_join(nodes, by = c("verb"="label")) %>%
  rename(from = "id") %>%
  left_join(nodes, by = c("actor"="label")) %>%
  rename("to" = "id") %>%
  mutate(N = n)%>%
  select(from, to, N)


library(echarts4r)


e_chart()%>%
  e_graph(roam = TRUE,
          lineStyle = list(
            color = "source"
          ),
          itemStyle = list(opacity = 0.5), 
          label = list(
            position = "right",
            formatter = "{b}"
          )) |> 
  e_graph_nodes(nodes
                , names = label
                , category = group
                , value = value
                , size = size
                , legend = TRUE) |> 
  e_graph_edges(edges
                , from
                , to) |> 
  e_grid(height = "10%", top = "15%") |>
  e_tooltip() |>
  e_title("Akteure und Häufig Benutzte Verben nach Sentiment"
          , "Ein Netzwerk das zeigt welche Werben häufig mit Akteuren zusammen genannt wurden")|> 
  e_theme("vintage") |> # add a theme
  e_theme_custom('{"color":["#d7ab82", "#61a0a8","#d87c7c","#919e8b"]}') |>
  e_brush() |>
  e_legend(left = 0, 
           top = 45)|> 
  e_toolbox_feature(feature = "saveAsImage")|> 
  e_toolbox_feature(feature = "dataView")

### Network Adjectives ----


dict_sentiment_pos <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$positive
                             , pos = 1)
dict_sentiment_neg <- tibble(word = quanteda.sentiment::data_dictionary_Rauh$negative
                             , neg = 1)

nodes <- c(actor_adj$actor, actor_adj$adj) %>%
  unique() %>%
  tibble(label = .) %>%
  rowid_to_column("id")%>%
  left_join(dict_sentiment_pos, join_by(label == word))%>%
  left_join(dict_sentiment_neg, join_by(label == word))%>%
  select(id, label, pos, neg)%>%
  group_by(id, label)%>%
  mutate(rn = row_number())%>%
  filter(rn == 1)%>%
  select(-rn)%>%
  unique()%>%
  ungroup()%>%
  mutate(value = case_when(pos == 1 ~ 1
                           , neg == 1 ~ -1
                           , label %in% actor_adj$actor ~ -2
                           , .default = 0)
         , group = case_when(value == 1 ~ "Positive"
                             , value == -1 ~ "Negative"
                             , value == 0 ~ "Neutral"
                             , value == -2 ~ "Actor")
         , size = 4
  )%>%
  select(-pos,-neg)


edges <- actor_adj %>%
  left_join(nodes, by = c("adj"="label")) %>%
  rename(from = "id") %>%
  left_join(nodes, by = c("actor"="label")) %>%
  rename("to" = "id") %>%
  mutate(N = n)%>%
  select(from, to, N)


library(echarts4r)
library(quanteda.sentiment)

e_chart()%>%
  e_graph(roam = TRUE,
          lineStyle = list(
            color = "source"
          ),
          itemStyle = list(opacity = 0.5), 
          label = list(
            position = "right",
            formatter = "{b}"
          )) |> 
  e_graph_nodes(nodes
                , names = label
                , category = group
                , value = value
                , size = size
                , legend = TRUE) |> 
  e_graph_edges(edges
                , from
                , to) |> 
  e_grid(height = "10%", top = "15%") |>
  e_tooltip() |>
  e_title("Akteure und Häufig Benutzte Adjektive nach Sentiment"
          , "Ein Netzwerk das zeigt welche Verben häufig mit Akteuren zusammen genannt wurden")|> 
  e_theme("vintage") |> # add a theme
  e_theme_custom('{"color":["#d7ab82", "#61a0a8","#d87c7c","#919e8b"]}') |>
  e_brush() |>
  e_legend(left = 0, 
           top = 45)|> 
  e_toolbox_feature(feature = "saveAsImage")|> 
  e_toolbox_feature(feature = "dataView")
