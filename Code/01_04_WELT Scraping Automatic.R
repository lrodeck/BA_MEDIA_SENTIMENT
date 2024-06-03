library(rvest)
library(XML)
library(xml2)
library(tidyverse)
library(DescTools)
library(lubridate)
library(progress)
"https://www.welt.de/sitemaps/sitemap/2020/08/sitemap.xml.gz"


range(2015:2016)

sitemap_welt <- tibble(start_date = seq.Date(from = as.Date("2015-07-01")
                                         , to = as.Date("2016-04-01")
                                         , by = "month"
)
, url = paste0("https://www.welt.de/sitemaps/sitemap/", year(start_date), "/", str_pad(month(start_date), width = 2, pad = '0'),"/sitemap.xml.gz")
, name = paste0("Welt Sitemap ", year(start_date), "-", str_pad(month(start_date), width = 2, pad = '0'),".xml.gz")
)

# Download all Sitemaps for timeframe (Only do this once!)
# library(XML)

# for (i in sitemap_welt$url) {
#   file_df <- sitemap_welt%>%
#     filter(url == i)
#   tf <- paste0("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/WELT/", file_df$name)
#   download.file(file_df$url, paste0("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/WELT/", file_df$name))
#   print("Download Done!")
# 
# }


list.files("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/WELT/")[1]
file <- paste0("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/WELT/", list.files("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/WELT/")[1])
data <- xmlToDataFrame(file)%>%
  filter(loc %like% "%.html")
url_data <- rbind(url_data, data)




url_data_welt <- tibble(loc = NA, lastmod  = NA)[0,1:2]
files_list <- list.files("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/WELT/")
library(tictoc)
{tic()
for (i in files_list) {
  print(i)
  file <- paste0("C:/Users/lasse/OneDrive/Desktop/Neuer Ordner/WELT/", i)
  data <- xmlToDataFrame(file)%>%
    filter(loc %like% "%.html")
  url_data_welt <- rbind(url_data_welt, data)
}
toc()}

url_data_welt <- url_data_welt%>%
  dplyr::filter(!grepl(x = loc, "/geschichte/")
         & !grepl(x = loc, "/sport/")
         & !grepl(x = loc, "/print_sport/")
         & !grepl(x = loc, "/reise/")
         & !grepl(x = loc, "/bloomberg/")
         & !grepl(x = loc, "/dpa_nt/")
         & !grepl(x = loc, "/satire/")
         & !grepl(x = loc, "/finanzen/")
         & !grepl(x = loc, "/motor/")
         & !grepl(x = loc, "/gesundheit/")
         & !grepl(x = loc, "/wissenschaft/")
         & !grepl(x = loc, "/iconist/")
         )

data_welt <- tibble(url = NA, date = NA, title = NA, subtitle = NA, content = NA, author = NA, topics = NA, outlet = NA)[0,]
pb <- progress_bar$new(total = length(url_data_welt$loc))
for (i in url_data_welt$loc) {
  tic()
  data_welt <- rbind(data_welt, try(tibble(url = i
                                       , date = read_html(i)%>% 
                                         html_elements("article")%>% 
                                         html_elements(".c-publish-date")%>%
                                         html_text2()%>%
                                         str_remove("Veröffentlicht am ")
                                       
                                       , title = read_html(i)%>% 
                                         html_elements("article")%>% 
                                         html_elements(".c-sticky-container")%>%
                                         html_elements("header")%>%
                                         html_element("h2")%>% 
                                         html_text2()
                                       
                                       , subtitle = read_html(i)%>% 
                                         html_elements(".c-sticky-container")%>% 
                                         html_element(".c-summary__intro")%>% 
                                         html_text2()%>%
                                         paste(collapse="\n ")
                                       
                                       , content = read_html(i)%>% 
                                         html_element(".c-article-text")%>% 
                                         html_elements("p, h3")%>% 
                                         html_text2()%>%
                                         paste(collapse="\n ")
                                       
                                       , author = read_html(i)%>% 
                                         html_elements(".c-author__by-line")%>% 
                                         html_text2()%>%
                                         as.array()%>%
                                         unique()
                                       
                                       , topics = read_html(i)%>% 
                                         html_elements(".rf-o-section")%>% 
                                         html_text2()%>%
                                         as.array()%>%
                                         unique()
                                       , outlet = "WELT"
                                       )
                         )
                   )
  pb$tick()
  Sys.sleep(1 / length(length(url_data_welt$loc)))
  toc()
}




# read_html("https://www.welt.de/regionales/baden-wuerttemberg/article143441898/Kaenguru-buext-aus-Zirkus-aus.html")%>% 
#   html_elements("article")%>% 
#   html_elements(".c-publish-date")%>%
#   html_text2()%>%
#   str_remove("Veröffentlicht am ")%>%
#   lubridate::dmy()


library(haven)
# create rds
saveRDS(data_welt, file = "Data/table_WELT_data.rds")
