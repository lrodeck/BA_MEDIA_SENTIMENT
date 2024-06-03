library(timelineS)
library(vistime)
library(tidyverse)
library(scales)
library(extrafont)



FlüchtlingskriseTL <- read_csv("Data/FlüchtlingskriseTL.csv")%>%
  rename(start = `Start Datum`
         , end = `...2`)%>%
  mutate(start = dmy(start)
         , end = dmy(end))



positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "start"=unique(FlüchtlingskriseTL$start),
  "position"=rep(positions, length.out=length(unique(FlüchtlingskriseTL$start))),
  "direction"=rep(directions, length.out=length(unique(FlüchtlingskriseTL$start)))
)

FlüchtlingskriseTL <- merge(x=FlüchtlingskriseTL, y=line_pos, by="start", all = TRUE)
FlüchtlingskriseTL <- FlüchtlingskriseTL[with(FlüchtlingskriseTL, order(start)), ]

head(FlüchtlingskriseTL)


text_offset <- 0.05

FlüchtlingskriseTL$month_count <- ave(FlüchtlingskriseTL$start==FlüchtlingskriseTL$start
                                      , FlüchtlingskriseTL$start
                                      , FUN=cumsum)
FlüchtlingskriseTL$text_position <- (FlüchtlingskriseTL$month_count * text_offset * FlüchtlingskriseTL$direction) + FlüchtlingskriseTL$position
head(FlüchtlingskriseTL)


month_buffer <- 2

month_date_range <- seq(min(FlüchtlingskriseTL$start) - months(month_buffer)
                        , max(FlüchtlingskriseTL$start) + months(month_buffer)
                        , by='month')
month_format <- format(month_date_range, '%B')
month_df <- data.frame(month_date_range, month_format)
month_df$year_date_range <- year(month_df$month_date_range)

month_df$month_format <- paste0(month_df$month_format, ' ', month_df$year_date_range)

year_date_range <- seq(min(FlüchtlingskriseTL$start) - months(month_buffer), max(FlüchtlingskriseTL$start) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

#### PLOT ####
ggplot(FlüchtlingskriseTL
       , aes(x=start 
             , y=0
             #, col=Event
             , label=Event)
       ) + 
  labs(col="Event"
       , title = "Zeitreihe der wichtigsten Geschehnisse"
       , subtitle = "Ausgewählte Ereignisse auf Basis der gesammelten Artikel"
       , caption = "Daten aus Spiegel Online und BILD Online (Jul 2015 - Apr 2016)") +
  theme_classic() +
  geom_hline(yintercept=0
             , color = "black"
             , size=0.3) +
  geom_segment(data=FlüchtlingskriseTL[FlüchtlingskriseTL$month_count == 1,]
               , aes(y= position
                     , yend= 0
                     , xend= start)
               , color= 'black'
               , size= 0.2) +
  geom_point(aes(y= 0)
             , size= 3) +
  ggthemes::theme_fivethirtyeight() +
  theme(strip.background = element_rect(fill="orange")
        , axis.line.y=element_blank()
        , axis.text.y=element_blank()
        , axis.title.x=element_blank()
        , axis.title.y=element_blank()
        , axis.ticks.y=element_blank()
        , axis.text.x =element_blank()
        , axis.ticks.x =element_blank()
        , axis.line.x =element_blank()
        , legend.position = "none"
        , text = element_text(family = "Arial")
        , plot.title = element_text(size = 24
                                    , color = "grey5")
        , plot.subtitle = element_text(size = 18
                                       , color = "grey20")
        , plot.caption = element_text(size = 8
                                      , color = "grey30")
        
        ) +
  geom_text(data=month_df
            , aes(x= month_date_range
                  , y= -0.1
                  , label= month_format)
            , size= 3
            , vjust= 0.5
            , color='black'
            , angle= 0) +
  geom_text(data=year_df
            , aes(x= year_date_range
                  , y= -0.2
                  , label= year_format
                  , fontface= "bold")
            , size= 6
            , color='black'
            , vjust= 20) +
  geom_text(aes(y= text_position
                , label= Event)
            , size=3.5)
