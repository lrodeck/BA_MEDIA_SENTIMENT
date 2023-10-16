library(reactable)
library(tidyverse)
library(extrafont)
library(reactablefmtr)
#extrafont::font_import(pattern = ".TTF", prompt=FALSE)
extrafont::loadfonts(device="all")
data_full <- readRDS("Data/data_full.rds")


reactable(data_full%>%
            select(upload_date, urls, title, content, outlet, Topic, sentiment_ai)%>%
            mutate(sentiment_ai = round(sentiment_ai,2))%>%
            arrange(upload_date, title, outlet)
          , theme = nytimes(font_size = 12, font_color = "grey28", cell_padding = 3)
          , filterable = TRUE
          , minRows = 10
          , searchable = TRUE
          , showPageSizeOptions = TRUE, paginationType = "jump"
          , columns = list(
            upload_date = colDef(name = "Datum", format = colFormat(date = TRUE)
                                 , sticky = "left"
                                 # Add a left border style to visually distinguish the sticky column
                                 , style = list(borderRight = "1px solid #eee")
                                 , headerStyle = list(borderLeft = "1px solid #eee")),
            urls = colDef(name = "URL"
                          , sticky = "left"
                          # Add a left border style to visually distinguish the sticky column
                          , style = list(borderRight = "1px solid #eee")
                          , headerStyle = list(borderLeft = "1px solid #eee")),
            title = colDef(name = "Überschrift"),
            content = colDef(name = "Text"),
            
            outlet = colDef(name = "Zeitung"),
            sentiment_ai = colDef(name = "Sentiment (Berechnet)"
                                  , cell = data_bars(data_full
                                                   #, round_edges = TRUE
                                                   , fill_color = c("#d87c7c","#d7ab82","#919e8b")
                                                   , background = "lightgrey"
                                                   , text_position = "inside-end"
                                                   , min_value = -1
                                                   , max_value = 1
                                                   , box_shadow = TRUE
                                                   )
                                  )
            ,
            Topic = colDef(name = "Thema (Berechnet)")
          )
          #, bordered = TRUE
          #, highlight = TRUE
          #, striped = TRUE
          , wrap = FALSE
          , resizable = TRUE
          , language = reactableLang(
            searchPlaceholder = "Suchen...",
            noData = "Keine Ergebnisse",
            pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
            pagePrevious = "\u276e",
            pageNext = "\u276f",
            
            # Accessible labels for assistive technologies such as screen readers.
            # These are already set by default, but don't forget to update them when
            # changing visible text.
            pagePreviousLabel = "Vorherige Seite",
            pageNextLabel = "Nächste Seite"
          )
        )%>%
  google_font("Fira Sans")

