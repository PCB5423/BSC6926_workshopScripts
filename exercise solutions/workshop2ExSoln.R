#' """ Solutions to Workshop 2 exercises
#'     @author: BSC 6926 B53
#'     date: 9/13/2022"""
    
library(tidyverse)
library(ggpubr)

#1 - Read in the LDWFBayAnchovy2007.csv and create a column that calculates 
#    the catch per unit effort (CPUE) for Bay anchovy within the dataframe.

df = read_csv(url('https://raw.githubusercontent.com/PCB5423/BSC6926_workshopScripts/master/data/LDWFBayAnchovy2007.csv')) %>% 
  mutate(CPUE = num/seines)

df$CPUE2 = df$num/df$seines

#2 - Create a dataframe or tibble that contains the basin names for the 
#    LDWFBayAnchovy2007.csv dataset (Barataria, Terrebonne, Ponchartrain, 
#    Vermilion-Teche, and Calcasieu) and the and 
#    abbreviation for each basin as a new column.

abrv = tibble(basin = c('Barataria', 'Terrebonne', 'Ponchartrain',
                        'Vermilion-Teche','Calcasieu'),
              abv = c('BAR', 'TER', 'PON', 'VER', 'CAL'))

#3 - Merge the dataframe/tibbles from exercises 1 and 2.
df = left_join(df, abrv, by = 'basin')

#4 - Plot the CPUE for each basin both over time and as 
#    a summary of the entire year using a different color for each basin.
colors = c('Calcasieu' = 'darkred',
           'Vermilion-Teche' = 'cadetblue4',
           'Terrebonne' = '#FFC125',
           'Barataria' = '#5d478b',
           'Pontchartrain' = 'grey55')

a = ggplot(df, aes(x = basin, y = CPUE, fill = basin))+
  geom_boxplot()+
  labs(x = NULL, y = 'Bay Anchovy CPUE')+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 14, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        legend.position = 'none',
        legend.title = element_blank())

b = ggplot(df, aes(x = date, y = CPUE, color = basin))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy CPUE', color = 'Basin')+
  theme_bw()+
  scale_color_manual(values = colors)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom')

ggarrange(a,b,
          align = 'v',
          ncol = 1)


              