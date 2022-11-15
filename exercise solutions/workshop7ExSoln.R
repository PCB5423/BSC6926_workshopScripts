#' """ Solutions to Workshop 7 exercises
#'     @author: BSC 6926 B53
#'     date: 10/17/2022"""

library(tidyverse)
library(vegan)

# Exercises 
# 1. Using the Calcasieu dataset calculate the mean and SD for species richness, Shannon, Simpson, and inverse Simpson for each sampling site.
df = read_csv('data/Calcasieu.csv') %>% 
  mutate(richness = specnumber(across(`Speckled Madtom`:`Smooth Puffer`)),
         H = diversity(across(`Speckled Madtom`:`Smooth Puffer`)),
         simp = diversity(across(`Speckled Madtom`:`Smooth Puffer`), "simpson"),
         invsimp = diversity(across(`Speckled Madtom`:`Smooth Puffer`),"inv")) %>% 
  group_by(site) %>% 
  summarise(mean_richness = mean(richness, na.rm = TRUE),
            sd_richness = sd(richness, na.rm = TRUE),
            mean_H = mean(H, na.rm = TRUE),
            sd_H = sd(H, na.rm = TRUE),
            mean_simp = mean(simp, na.rm = TRUE),
            sd_simp = sd(simp, na.rm = TRUE),
            mean_invS = mean(invsimp, na.rm = TRUE),
            sd_invS = sd(invsimp, na.rm = TRUE))


# 2. Plot the dominance (Whittiker) and K-dominance curves for each site.
## Dominance curves / Whittaker curves
#Dominance as a function of species rank
df = read_csv('data/Calcasieu.csv') %>%
  pivot_longer(cols = `Speckled Madtom`:`Smooth Puffer`, 
               names_to = "Species", 
               values_to = "Count") %>% 
  group_by(site) %>% 
  filter(Count > 0) %>% 
  mutate(Total = sum(Count)) %>% 
  ungroup() %>% 
  group_by(site, Species) %>%
  summarise(Count_Spp = sum(Count),
            Total_Count = max(Total)) %>% 
  mutate(pi = Count_Spp/Total_Count, 
         rank = length(unique(Species))-rank(pi)) %>% 
  ungroup()

ggplot(df, aes(rank, pi, color = as.factor(site)))+
  geom_line(size = 1)+
  labs(x = 'Species rank',
       y = 'Dominance',
       color = 'Site')+
  theme_bw()

## K-dominance curves
# Cumulative dominance by species rank


df = read_csv('data/Calcasieu.csv') %>%
  pivot_longer(cols = `Speckled Madtom`:`Smooth Puffer`, 
               names_to = "Species", 
               values_to = "Count") %>% 
  group_by(site) %>% 
  filter(Count > 0) %>% 
  mutate(Total = sum(Count)) %>% 
  ungroup() %>% 
  group_by(site, Species) %>%
  summarise(Count_Spp = sum(Count),
            Total_Count = max(Total)) %>% 
  mutate(pi = Count_Spp/Total_Count, 
         rank = length(unique(Species))-rank(pi))%>% 
  arrange(rank, .by_group = T) %>% 
  mutate(cumsum = cumsum(pi))

ggplot(df, aes(rank, cumsum, color = as.factor(site)))+
  geom_line(size = 1)+
  labs(x = 'Species rank',
       y = 'Cumulative Dominance',
       color = 'Site')+
  theme_bw()

# 3. Plot the alpha, beta, and gamma diversity for each site for each sampling date in 2007. Use either additive or multiplicative $\beta$ diversity. Plot diversity over time. 
df_l = read_csv('data/Calcasieu.csv') %>%
  pivot_longer(cols = `Speckled Madtom`:`Smooth Puffer`, 
               names_to = "Species", 
               values_to = "Count")

gammaDiv = length(unique(df_l$Species))

div = read_csv('data/Calcasieu.csv') %>% 
  mutate(richness = specnumber(across(`Speckled Madtom`:`Smooth Puffer`))) %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(site, month) %>% 
  summarize(alpha = mean(richness)) %>% 
  mutate(gamma = gammaDiv,
         beta_a = gamma - alpha,
         beta_m = gamma/alpha)

library(ggpubr)

ggplot(div, aes(month, alpha, color = as.factor(site)))+
  geom_point()+
  geom_line()+
  labs(x = 'Month', y = expression(alpha ~ 'diversity'), 
       color = 'Site')+
  theme_bw()

ggplot(div, aes(month, beta_a, color = as.factor(site)))+
  geom_point()+
  geom_line()+
  labs(x = 'Year', y = expression('Additive'~ beta ~ 'diversity'), 
       color = 'Site')+
  theme_bw()

ggplot(div, aes(month, beta_m, color = as.factor(site)))+
  geom_point()+
  geom_line()+
  labs(x = 'Year', y = expression('Multiplicative'~ beta ~ 'diversity'), 
       color = 'Site')+
  theme_bw()

# 4. Plot the Bray-Curtis and Jaccard dissimilarity for each site.
df = read_csv('data/Calcasieu.csv') 

bc = metaMDS(df[,4:62], distance = "bray", k = 2, try = 100)
jc = metaMDS(df[,4:62], distance = "jaccard", k = 2, try = 100)

b = bc$points %>% as.data.frame() %>% 
  mutate(Dissimilarity = 'Bray',
         site = as.factor(df$site))

j = jc$points %>% as.data.frame() %>% 
  mutate(Dissimilarity = 'Jaccard',
         site = as.factor(df$site))

dis = bind_rows(b,j)

ggplot(dis, aes(MDS1, MDS2, color = site))+
  geom_point()+
  facet_wrap(~Dissimilarity)+
  theme_bw()
  