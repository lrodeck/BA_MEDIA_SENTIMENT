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

# source: https://www.spiegel.de/nachrichtenarchiv/

data_fin <- tibble(urls = character()
                   , content = character()
                   , upload_time = character()
                   , author = character()
                   , resort = character()
                   , resort_sub = character()
                   , topic = character())

data_fin <- readRDS("table_spiegel_data.rds")

dates <- seq.Date(as.Date("01.02.2016", format="%d.%m.%Y")
                  , as.Date("31.03.2016", format="%d.%m.%Y")
                  , by = "day")




main_page_url <- paste0("https://www.spiegel.de/nachrichtenarchiv/artikel-"
                        , sprintf("%02d", day(dates))
                        , "."
                        , sprintf("%02d", month(dates))
                        , "."
                        , year(dates),".html")


data <- tibble(urls = character())
pb <- progress_bar$new(total = length(main_page_url))

for (y in main_page_url) {
  
  #print(paste0(now(),' - ', y))
  main_page <- try(read_html(y))
  
  urls <- try(main_page %>% 
                  html_nodes("section") %>%  # Find the div element
                  html_nodes("a")%>%
                  html_attr("href"))
  
 data <- try(rbind(as.data.frame(urls), data))
 pb$tick()
 Sys.sleep(1 / length(main_page_url))
}

data_urls <- data%>%
  filter(!grepl("nachrichtenarchiv", urls)
        & !grepl("video", urls)
        & !grepl("fotostrecke", urls)
        & !grepl("spiegeltv", urls) 
        & !grepl("sptv", urls) 
        & !grepl("/wissenschaft", urls) 
        & !grepl("/karriere", urls) 
        & !grepl("/reise", urls)
        & !grepl("/sport", urls))%>%
  filter(grepl("www.spiegel.de", urls))%>%
  drop_na()



pb <- progress_bar$new(total = length(data_urls$urls))

for (y in data_urls$urls) {
  html_y <- try(read_html(y))
  data_temp <- try(tibble(urls = y
                      , title = html_y%>%
                        html_nodes("h1") %>% 
                        html_text()%>%
                        str_replace_all("[\r\n]" , "")
                      , content = toString(html_y %>% 
                                   html_nodes("p") %>% 
                                   html_text()%>%
                                   str_replace_all("[\r\n]" , ""))
                      , upload_time = toString(html_y %>% 
                                                 html_nodes("header")%>%
                                                 html_nodes("time") %>%  # Find the h1 element
                                                 html_text()%>%
                                                 str_replace_all("[\r\n]" , ""))
                      , author = toString(html_y %>% 
                                            html_nodes("header")%>%
                                            html_nodes(".font-sansUI") %>%
                                            html_nodes("a") %>%  # Find the h1 element
                                            html_text()%>%
                                            str_replace_all("[\r\n]" , ""))
                      , resort = c(html_y %>% 
                                     html_nodes("nav")%>%
                                     html_nodes(".polygon-swiper")%>%
                                     html_nodes("li") %>%  # Find the h1 element
                                     html_text()%>%
                                     str_replace_all("[\r\n]" , ""))[2]
                      , resort_sub = c(html_y %>% 
                                         html_nodes("nav")%>%
                                         html_nodes(".polygon-swiper")%>%
                                         html_nodes("li") %>%  # Find the h1 element
                                         html_text()%>%
                                         str_replace_all("[\r\n]" , ""))[3]
                      , topic = c(html_y %>% 
                                    html_nodes("nav")%>%
                                    html_nodes(".polygon-swiper")%>%
                                    html_nodes("li") %>%  # Find the h1 element
                                    html_text()%>%
                                    str_replace_all("[\r\n]" , ""))[4]
                      ))
  data_fin <- try(rbind(data_fin, data_temp))
  pb$tick()
  Sys.sleep(1 / length(length(data_urls$urls)))

}

library(haven)
# create rds
saveRDS(data_fin, file = "table_spiegel_data.rds")

