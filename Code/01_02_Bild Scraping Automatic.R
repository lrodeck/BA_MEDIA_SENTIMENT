# Load the required packages
library(rvest)
library(dplyr)
library(xml2)
library(DescTools)
library(stringr)
library(tidyverse)
library(progress)

# important dates: 31. August 2015 
# --> merkel PK: "wir schaffen das"
# --> sylvester nacht köln
# --> timeframe Jul 2015 - Mär 2016

# source: https://www.bild.de/themen/uebersicht/archiv/archiv-82532020.bild.html?archiveDate=
#
# data_fin_bild <- tibble(urls = character()
#                    , content = character()
#                    , upload_time = character()
#                    , author = character()
#                    , resort = character()
#                    , resort_sub = character()
#                    , topic = character())

data_fin_bild <- readRDS(file = "table_bild_data.rds")
dates <- seq.Date(as.Date("2016-02-01", format="%Y-%m-%d")
                  , as.Date("2016-03-31", format="%Y-%m-%d")
                  , by = "day")




main_page_url <- paste0("https://www.bild.de/themen/uebersicht/archiv/archiv-82532020.bild.html?archiveDate="
                        , dates)


data_bild <- tibble(urls = character())
pb <- progress_bar$new(total = length(main_page_url))

for (y in main_page_url) {
  
  #print(paste0(now(),' - ', y))
  main_page <- read_html(y)
  
  urls <- main_page %>% 
    html_nodes("section") %>%  # Find the div element
    html_nodes("a")%>%
    html_attr("href")
  
  data_bild <- rbind(as.data.frame(urls), data_bild)
  pb$tick()
  Sys.sleep(1 / length(main_page_url))
}

data_urls_bild <- data_bild%>%
  filter(!grepl("uebersicht", urls)
         & !grepl("/video", urls)
         & !grepl("fotostrecke", urls)
         & !grepl("home", urls) 
         & !grepl("schlagzeilen-des-tages", urls) 
         & !grepl("/byou", urls) 
         & !grepl("/unterhaltung", urls) 
         & !grepl("/spiele", urls)
         & !grepl("/sport", urls)
         & !grepl("/bild-plus", urls)
         & !grepl("/lifestyle", urls)
         & !grepl("http", urls)
         & !grepl("/regional/berlin", urls))%>%
  mutate(urls = paste0("https://www.bild.de",urls))%>%
  drop_na()



pb <- progress_bar$new(total = length(data_urls_bild$urls))

data_temp <- data_temp[0,]
for (y in data_urls_bild$urls) {
  html_y <- try(read_html(y))
  data_temp <- try(data_temp[0,])
  data_temp <- try(tibble(urls = y
                      , title = html_y%>%
                        html_nodes("h1") %>% 
                        html_text()%>%
                        str_replace_all("[\r\n]" , "")
                      , content = toString(html_y %>% 
                                             html_nodes(".article-body") %>%  # Find the h1 element
                                             html_nodes("p") %>%  # Find the h1 element
                                             html_text()%>%
                                             str_replace_all("[\r\n]" , ""))
                      , upload_time = toString(html_y %>% 
                                                 html_nodes("time") %>%  # Find the h1 element
                                                 html_text()%>%
                                                 str_replace_all("[\r\n]" , ""))
                      , author = "BILD"
                      , resort = c(html_y %>%
                                     html_nodes("nav")%>%
                                     html_nodes("ol") %>%
                                     html_nodes("li") %>%  # Find the h1 element
                                     html_text()%>%
                                     str_replace_all("[\r\n]" , ""))[2]
                      , resort_sub = c(html_y %>%
                                         html_nodes("nav")%>%
                                         html_nodes("ol") %>%
                                         html_nodes("li") %>%  # Find the h1 element
                                         html_text()%>%
                                         str_replace_all("[\r\n]" , ""))[3]
  ))
  data_fin_bild <- try(rbind(data_fin_bild, data_temp))
  pb$tick()
  Sys.sleep(1 / length(length(data_urls_bild$urls)))
  
}


library(haven)
# create rds
saveRDS(data_fin_bild, file = "table_bild_data.rds")

