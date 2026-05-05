############################################################
# Measles cases analysis: Bangladesh & USA
# Author: Abu Bakar Siddique
# Purpose: Clean, reproducible, beginner-friendly workflow
############################################################


############################
# 1. Library setup
############################

# List of required packages
required_packages <- c(
  "ggplot2",
  "dplyr",
  "readxl",
  "tidyr",
  "stringr",
  "forcats"
)

## Install missing packages (safe for beginners)
# installed_packages <- rownames(installed.packages())
# 
# for (pkg in required_packages) {
#   if (!pkg %in% installed_packages) {
#     install.packages(pkg, dependencies = TRUE)
#   }
# }

# Load libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(forcats)


############################
# 2. Project directories
############################

# Define project directories (assumes scripts/ data/ results/)
base_dir    <- normalizePath("..")
data_dir    <- file.path(base_dir, "data")
results_dir <- file.path(base_dir, "results")

# Create results directory if it does not exist
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}


############################
# 3. Bangladesh measles data
############################

# OPTION A: Read data from WHO Excel file
# Source:
# https://immunizationdata.who.int/global/wiise-detail-page/measles-reported-cases-and-incidence

measles_file <- file.path(
  data_dir,
  "Measles reported cases and incidence 2026-05-05 11-46 UTC.xlsx"
)

# Uncomment if you want to read from Excel
# measles <- read_excel(measles_file)


# OPTION B: Manual data entry (fully reproducible fallback)

measles <- data.frame(
  Year = 1975:2026,
  Measles_cases = c(
    3030,40171,22973,18576,31895,11077,10441,18116,9569,11851,
    11699,14352,13631,11471,27327,1705,9292,5768,5443,9571,
    4995,4929,10329,6522,5666,5098,4414,3484,4067,9743,
    25934,6192,2924,2660,718,788,5625,1986,237,289,
    240,972,4001,2263,5827,2410,203,311,281,247,132,40000
  )
)


############################
# 4. Data preparation
############################

# Years to be highlighted in plots
highlight_years <- c(1976, 2005, 2026)

# Add a highlight flag for plotting
measles <- measles %>%
  mutate(
    highlight = if_else(
      Year %in% highlight_years,
      "Highlight",
      "Normal"
    )
  )


############################
# 5. Bar plot (Bangladesh)
############################

p_bar <- ggplot(
  measles,
  aes(
    x = factor(Year),
    y = Measles_cases,
    fill = highlight
  )
) +
  geom_col(
    width = 0.8,
    color = "grey30"
  ) +
  scale_fill_manual(
    values = c(
      "Highlight" = "#b22222",
      "Normal"    = "grey80"
    ),
    guide = "none"
  ) +  
  scale_x_discrete(expand=c(0,0),breaks=seq(1980,2030,by=10))+  
  labs(
    title = "বাংলাদেশে বার্ষিক হাম রোগীর সংখ্যা\n(Annual measles cases in Bangladesh)",
    x = "বছর (Year)",
    y = "হাম রোগীর সংখ্যা\n(Number of cases)",
    caption = "Data: WHO & New Age | Figure: A. B. Siddique"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold"),
    axis.title   = element_text(),
    axis.text.x  = element_text(vjust = 0.5, size = 8),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# Display plot
print(p_bar)

# Save plot
ggsave(
  filename = file.path(
    results_dir,
    "Measles_Bangladesh_barplot.tiff"
  ),
  plot        = p_bar,
  width       = 7,
  height      = 5,
  dpi         = 600,
  compression = "lzw"
)


############################
# 6. Line plot (log scale)
############################

p_line <- ggplot(measles, aes(x = Year, y = Measles_cases)) +
  geom_line(color = "black",linewidth = 0.8) +
  scale_y_log10() +
  labs(title = "Measles cases in Bangladesh (log scale)",
       x = "Year",
       y = "Number of cases (log10)",
       caption = "Data: WHO & New Age | Figure: A. B. Siddique") +
  theme_classic(base_size = 12) +
  theme(plot.title   = element_text(face = "bold"),
        plot.caption = element_text(size = 9, hjust = 0))

# Display plot
print(p_line)

# Save plot
ggsave(filename = file.path(results_dir, "Measles_Bangladesh_lineplot.tiff"),
       plot        = p_line,
       width       = 7,
       height      = 5,
       dpi         = 600,
       compression = "lzw")


############################
# 7. USA measles heatmap
############################

# Data source:
# https://www.dropbox.com/s/19p8vku0i9np26b/data_wsj.csv?dl=1
# Original inspiration: Rauk R (NBIS)

# custom summing function
fun1 <- function(x) ifelse(all(is.na(x)),NA,sum(x,na.rm=TRUE))

# read data
me3 <- read.csv("https://www.dropbox.com/s/19p8vku0i9np26b/data_wsj.csv?dl=1",
                header=T,stringsAsFactors=F,skip=2) %>%
  gather(key=state,value=value,-YEAR,-WEEK) %>%
  mutate(value=str_replace(value,"^-$",NA_character_),
         value=as.numeric(value)) %>%
  group_by(YEAR,state) %>% 
  summarise(total=fun1(value)) %>%
  mutate(state=str_replace_all(state,"[.]"," "),
         state=str_to_title(state))

colnames(me3) <- tolower(colnames(me3))

# custom colors
cols <- c("#e7f0fa","#c9e2f6","#95cbee","#0099dc","#4ab04a",
          "#ffd73e","#eec73a","#e29421","#f05336","#ce472e")



############################
# 8. Heatmap plot (USA)
############################

p_usa <- ggplot(me3,aes(x=year,y=reorder(state,desc(state)),fill=total))+
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

# Display plot
print(p_usa)

# Save plot
ggsave(
  filename = file.path(
    results_dir,
    "Measles_USA_heatmap.tiff"
  ),
  plot        = p_usa,
  width       = 7,
  height      = 5,
  dpi         = 600,
  compression = "lzw"
)


############################################################
# End of script
############################################################