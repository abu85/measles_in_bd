
# add libraries (install them if needed)
library(ggplot2)
library(dplyr)

# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/measles---number-of-reported-cases 
measles <- data.frame(
  Year = c(
    1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,
    1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,
    1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
    2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,
    2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026
  ),
  Measles_cases = c(
    3030,40171,22973,18576,31895,11077,10441,18116,9569,11851,
    11699,14352,13631,11471,27327,1705,9292,5768,5443,9571,
    4995,4929,10329,6522,5666,5098,4414,3484,4067,9743,
    25934,6192,2924,2660,718,788,5625,1986,237,289,
    240,972,4001,2263,5827,2410,203,311,281,247,132,40000
  )
)



# Add highlight flag
measles <- measles %>%
  mutate(
    highlight = ifelse(Year %in% c(1976, 2005, 2026), "Highlight", "Normal")
  )



ggplot(measles, aes(x = factor(Year), y = Measles_cases, fill = highlight)) +
  geom_bar(
    stat = "identity",
    width = 0.8,
    color = "grey30"
  ) +
  
  scale_fill_manual(
    values = c(
      "Highlight" = "#b22222",   # dark red (brick red)
      "Normal"    = "grey80"
    ),
    guide = "none"
  ) +
  
  labs(
    title = "বাংলাদেশে বার্ষিক হাম রোগীর সংখ্যা\n(Annual measles cases in Bangladesh)",
    x = "বছর\n(Year)",
    y = "বার্ষিক হাম রোগীর সংখ্যা\n(Annual number of cases)",
    caption = "Data source: WHO & NewAge | Figure: ABS Biplob"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      lineheight = 1.15
    ),
    
    axis.title = element_text(size = 11,face = "bold"),
    
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      size = 8,
      color = "black"
    ),
    
    axis.text.y = element_text(color = "black"),
    
    plot.caption = element_text(
      size = 8.5,
      hjust = 0,
      color = "grey40"
    ),
    
    plot.margin = margin(8, 10, 8, 8)
  ) +



ggsave(
  "Measles_Bangladesh_WHO_Lancet_style.tiff",
  width = 7,
  height = 5,
  dpi = 600,
  compression = "lzw"
)

ggplot(measles, aes(x = Year, y = Measles_cases)) +
  geom_line(color = "black", linewidth = 0.8) +
  scale_y_log10() +
  labs(
    title = "বাংলাদেশে মিজলস রোগীর সংখ্যা (Measles cases in Bangladesh, log scale)",
    x = "বছর (Year)",
    y = "মিজলস রোগীর সংখ্যা (log10 scale)",
    caption = "Source: WHO/UNICEF Joint Reporting Form (JRF)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0)
  )





## Measles
## From Rauk R , NBIS
library(tidyverse)

# data is here https://www.dropbox.com/s/19p8vku0i9np26b/data_wsj.csv?dl=1

# custom summing function
fun1 <- function(x) ifelse(all(is.na(x)),NA,sum(x,na.rm=TRUE))

# read data
me3 <- read.csv("https://www.dropbox.com/s/19p8vku0i9np26b/data_wsj.csv?dl=1",header=T,stringsAsFactors=F,skip=2) %>%
  gather(key=state,value=value,-YEAR,-WEEK) %>%
  mutate(value=str_replace(value,"^-$",NA_character_),
         value=as.numeric(value)) %>%
  group_by(YEAR,state) %>% 
  summarise(total=fun1(value)) %>%
  mutate(state=str_replace_all(state,"[.]"," "),
         state=str_to_title(state))

colnames(me3) <- tolower(colnames(me3))

# custom colors
cols <- c("#e7f0fa","#c9e2f6","#95cbee","#0099dc","#4ab04a", "#ffd73e","#eec73a","#e29421","#f05336","#ce472e")

# plotting
p <- ggplot(me3,aes(x=year,y=reorder(state,desc(state)),fill=total))+
  geom_tile(color="white",size=0.25)+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),breaks=seq(1930,2010,by=10))+
  scale_fill_gradientn(colors=cols,na.value="grey95",
                       limits=c(0,4000),
                       values=c(0,0.01,0.02,0.03,0.09,0.1,0.15,0.25,0.4,0.5,1),
                       labels=c("0k","1k","2k","3k","4k"),
                       guide=guide_colourbar(ticks=T,nbin=50,
                                             barheight=.5,label=T, 
                                             barwidth=10))+
  labs(x="",y="",fill="",title="Measles")+
  coord_fixed()+
  geom_segment(x=1963,xend=1963,y=0,yend=51.5,size=.9) +
  annotate("text",label="Vaccine introduced",x=1963,y=53, 
           vjust=1,hjust=0,size=I(3),family="Gidole")+
  theme_minimal(base_family="Gidole")+
  theme(legend.position=c(.5,-.13),
        legend.direction="horizontal",
        legend.text=element_text(color="grey20"),
        plot.margin=grid::unit(c(.5,0,1.5,0),"cm"),
        axis.text.y=element_text(size=6,hjust=1,vjust=0.5),
        axis.text.x=element_text(size=8),
        axis.ticks.y=element_blank(),
        # panel.grid=element_blank(),
        title=element_text(hjust=-.07,vjust=1),
        panel.grid=element_blank())

p
