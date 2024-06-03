library(rvest)
library(XML)
library(xml2)
library(tidyverse)
library(lubridate)
library(progress)
library(tictoc)
"https://taz.de/sitemap-article-online-2015-10-01-2015-10-31.xml"

sitemaps <- tibble(start_date = seq.Date(from = as.Date("2015-07-01")
         , to = as.Date("2016-04-01")
         , by = "month"
)

, end_date = as.Date(seq.Date(from = as.Date("2015-07-01")
                 , to = as.Date("2016-04-01")
                 , by = "month"
)) + months(1) - days(1)
, sitemap = paste0("https://taz.de/sitemap-article-online-", start_date, "-", end_date,".xml")
, name = paste0("sitemap", start_date, "-", end_date,".xml")
)

url_data_taz <- tibble(loc = NA, lastmod  = NA)[0,1:2]
pb <- progress_bar$new(total = length(sitemaps$sitemap))
for (i in sitemaps$sitemap) {
  tic()
  download_data_temp <- sitemaps%>%
    filter(sitemap == i)
  file <- paste0("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/TAZ/", download_data_temp$name)
  url <- download_data_temp$sitemap
  download.file(url, file, mode="wb") # get data into test
  data <- xmlToDataFrame(file)
  url_data_taz <- rbind(url_data_taz, data)
  pb$tick()
  Sys.sleep(1 / length(length(sitemaps$sitemap)))
  toc()
}



data_taz <- tibble(url = NA, date = NA, title = NA, subtitle = NA, content = NA, author = NA, outlet = NA)[0,]
pb <- progress_bar$new(total = length(url_data_taz$loc))
for (i in url_data_taz$loc) {
  tic()
  data_taz <- rbind(data_taz, try(tibble(url = i
                                     , date = read_html(i)%>% 
                                       html_elements(".date")%>% 
                                       html_text2()
                                     , title = read_html(i)%>% 
                                       html_elements("article")%>% 
                                       html_element("h1")%>% 
                                       html_text2()
                                     , subtitle = read_html(i)%>% 
                                       html_elements("article")%>% 
                                       html_element("p")%>% 
                                       html_text2()
                                     , content = read_html(i)%>% 
                                       html_element("article")%>% 
                                       html_elements(".odd, .even, .strong, h6")%>% 
                                       html_text2()%>%
                                       paste(collapse="\n ")
                                     
                                     , author = read_html(i)%>% 
                                       html_elements(".sect_profile")%>% 
                                       html_elements("h4")%>% 
                                       html_text2()%>%
                                       as.array()%>%
                                       unique()
                                     , outlet = "TAZ"
                                     )
                        )
                  )
  pb$tick()
  Sys.sleep(1 / length(length(url_data_taz$loc)))
  toc()
}

library(haven)
# create rds
saveRDS(data_taz, file = "C:/Users/lasse/OneDrive/Dokumente/R Projects/BACHELOR_ARBEIT/Data/table_TAZ_data.rds")
