#' """ Workshop 6: Community data: diversity metrics
#'     @author: BSC 6926 B53
#'     date: 10/11/2022"""

library(tidyverse)

## Community Data
#Community data can vary in format, but typically involves abundance, biomass, or CPUE data for multiple species collected in each sample. Data can be stored in wide (species ID for each column) or long format. When examining community data, the first step is usually data exploration which can be done by calculating summary statistics or plotting.



# data in wide format
shrimp_w = read_csv('data/shrimp.csv')

shrimp_w

# convert to long format for plotting
shrimp_l = shrimp_w %>% 
  pivot_longer(cols = 7:16, 
               names_to = "Species", 
               values_to = "Count") %>% 
  select(-`...1`)

shrimp_l


shrimp_ss = shrimp_l %>% 
  group_by(STREAM) %>% 
  summarise(mean_count = mean(Count, na.rm = TRUE),
            sd_count = sd(Count, na.rm = TRUE),
            total = sum(Count, na.rm = TRUE))

shrimp_ss


### Plot density of Abundance

ggplot(shrimp_l, aes(x = Count, fill = STREAM))+
  geom_density(alpha=0.4) +
  geom_vline(data=shrimp_ss, aes(xintercept=mean_count, color=STREAM),
             linetype="dashed", size = 1) +
  theme_bw()

ggplot(shrimp_l, aes(x = Count, fill = STREAM))+
  geom_density(alpha=0.4) +
  geom_vline(data=shrimp_ss, aes(xintercept=mean_count, color=STREAM),
             linetype="dashed", size = 1) +
  xlim(0, 6) +
  theme_bw()


### Violin plot of abundance


ggplot(shrimp_l, aes(x = STREAM, y = Count, fill = STREAM))+
  geom_violin(alpha=0.4) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="red") +
  ylim(0, 10)+
  theme_bw()



## Summarize and plot by species

shrimp_summary2 = shrimp_l %>% 
  group_by(STREAM, Species) %>% 
  summarise(mean_count = mean(Count, na.rm = TRUE),
            sd_count = sd(Count, na.rm = TRUE),
            total = sum(Count, na.rm = TRUE)) %>% 
  mutate(Species = fct_reorder(Species, mean_count, .desc = TRUE))

ggplot(shrimp_summary2, aes(x = Species, y = mean_count, fill = STREAM))+
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(y = 'Mean Count', x = 'Species', fill = 'Stream')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


## Diversity metrics
#Community data due to its multidimensionality is difficulty to interpret. Researchers have developed different indices and metrics to provide information about the biodiversity of the community data.

## Species Richness
#Species richness ($S$) is the total number of species. 

shrimp_l %>% 
  group_by(STREAM, YEAR, MONTH) %>% 
  filter(Count > 0) %>% 
  summarise(richness = length(unique(Species))) %>% 
  ungroup() %>% 
  group_by(STREAM) %>% 
  summarise(mean_richness = mean(richness, na.rm = TRUE),
            sd_richness = sd(richness, na.rm = TRUE))

## Shannon 
# The Shannon diversity index ($H'$) is a diversity metric that accounts for species proportions and is calculated with the following formula: 
# $$H' = -\sum_{i=1}^S p_i \log(p_i)$$
#   where $p_i$ is the proportion of species $i$. The higher the value of $H'$, the higher the diversity of species in a particular community. The lower the value of H, the lower the diversity. A value of $H'$ = 0 indicates a community that only has one species.

# for loop
df = unique(shrimp_l[c("STREAM","MONTH","YEAR")])
df$H = NA

df

for (i in 1:nrow(df)){
  d = shrimp_l %>% filter(STREAM == df$STREAM[i],
                          MONTH == df$MONTH[i],
                          YEAR == df$YEAR[i],
                          Count > 0)
  d = d %>% count(Species,wt = Count) %>% 
    mutate(pi = n/sum(n),
           ln_pi = log(pi),
           p_ln_pi = pi*ln_pi)
  
  df$H[i] = -sum(d$p_ln_pi)
}

df %>% 
  group_by(STREAM) %>% 
  summarise(mean_H = mean(H, na.rm = TRUE),
            sd_H = sd(H, na.rm = TRUE))


# dplyr
shrimp_l %>% 
  group_by(STREAM, YEAR, MONTH) %>% 
  filter(Count > 0) %>% 
  mutate(Total = sum(Count)) %>% 
  ungroup() %>% 
  group_by(STREAM, YEAR, MONTH, Species) %>%
  summarise(Count_Spp = sum(Count),
            Total_Count = max(Total)) %>% 
  mutate(p = Count_Spp/Total_Count, 
         ln_pi = log(p), 
         p_ln_pi = p*ln_pi) %>% 
  ungroup() %>% 
  group_by(STREAM, YEAR, MONTH) %>% 
  summarise(H = -sum(p_ln_pi)) %>% 
  ungroup() %>% 
  group_by(STREAM) %>% 
  summarise(mean_H = mean(H, na.rm = TRUE),
            sd_H = sd(H, na.rm = TRUE))

## Simpson
# Another popular set of indices are Simpson's indices. The Simpson index calculated is a dominance metric and is calculated
# $$D = \sum_{i=1}^S p_i^2$$ It ranges between 0 and 1 with high values indicating that abundance is made up of a few species. Its counter part $1 - D$ is an evenness index. The inverse $1/D$ is an indication of the richness in a community with uniform evenness that would have the same level of diversity.

# for loop
df$D = NA
df

for (i in 1:nrow(df)){
  d = shrimp_l %>% filter(STREAM == df$STREAM[i],
                        MONTH == df$MONTH[i],
                        YEAR == df$YEAR[i],
                        Count > 0)
  d = d %>% count(Species,wt = Count) %>% 
    mutate(pi = n/sum(n))
  
  df$D[i] = sum(d$pi^2)
}
df$even = 1 - df$D
df$inv = 1/df$D

df %>% 
  group_by(STREAM) %>% 
  summarise(mean_D = mean(D, na.rm = TRUE),
            sd_D = sd(D, na.rm = TRUE),
            mean_even = mean(even, na.rm = TRUE),
            sd_even = sd(even, na.rm = TRUE),
            mean_inv = mean(inv, na.rm = TRUE),
            sd_inv = sd(inv, na.rm = TRUE))

# dplyr
shrimp_l %>% 
  group_by(STREAM, YEAR, MONTH) %>% 
  filter(Count > 0) %>% 
  mutate(Total = sum(Count)) %>% 
  ungroup() %>% 
  group_by(STREAM, YEAR, MONTH, Species) %>%
  summarise(Count_Spp = sum(Count),
            Total_Count = max(Total)) %>% 
  mutate(p = Count_Spp/Total_Count, 
         p2 = p^2) %>% 
  ungroup() %>% 
  group_by(STREAM, YEAR, MONTH) %>% 
  summarise(s_dominance = sum(p2),
            s_evenness = 1 - s_dominance,
            inverse_s = 1/s_dominance) %>% 
  ungroup() %>% 
  group_by(STREAM) %>% 
  summarise(mean_D = mean(s_dominance, na.rm = TRUE),
            sd_D = sd(s_dominance, na.rm = TRUE),
            mean_even = mean(s_evenness, na.rm = TRUE),
            sd_even = sd(s_evenness, na.rm = TRUE),
            mean_inv = mean(inverse_s, na.rm = TRUE),
            sd_inv = sd(inverse_s, na.rm = TRUE))

## Species accumulation curves
# Also called rarefaction curve, plots the number of species as a function of the number of samples.
# add unique ID for each sampling event
shrimp_l = shrimp_l %>% 
  group_by(STREAM, MONTH, YEAR, POOL, TRAP) %>% 
  mutate(sample_ID = cur_group_id()) %>% 
  ungroup()

# curve for B5
b5 = shrimp_l %>% 
  filter(STREAM == 'B5')

b5_sample_ID  = unique(b5$sample_ID)

# store data
sp_b5 = tibble(stream = 'B5', n_samp = 1:length(b5_sample_ID), n_spp = NA)

for (i in 1:length(b5_sample_ID)){
  # sample ID to include
  samp = b5_sample_ID[1:i]
  
  # include only sample numbers 
  d = b5 %>% 
    filter(sample_ID %in% samp,
           Count > 0)
  
  sp_b5$n_spp[i] = length(unique(d$Species))
}

# curve for QP
qp = shrimp_l %>% 
  filter(STREAM == 'QP')

qp_sample_ID  = unique(qp$sample_ID)

# store data
sp_qp = tibble(stream = 'QP', n_samp = 1:length(qp_sample_ID), n_spp = NA)

for (i in 1:length(qp_sample_ID)){
  # sample ID to include
  samp = qp_sample_ID[1:i]
  
  # include only sample numbers 
  d = qp %>% 
    filter(sample_ID %in% samp,
           Count > 0)
  
  sp_qp$n_spp[i] = length(unique(d$Species))
}

# bind and plot
sac = bind_rows(sp_b5, sp_qp)

ggplot(sac, aes(n_samp, n_spp, color = stream))+
  geom_line(size = 1)+
  labs(x = 'Number of Samples',
       y = 'Number of Species',
       color = 'Stream')+
  theme_bw()

### Iterate and use based on random samples


# curve for B5
b5 = shrimp_l %>% 
  filter(STREAM == 'B5')

b5_sample_ID  = unique(b5$sample_ID)

iterations = 5

# store data
sp_b5 = tibble(stream = 'B5', 
               n_samp = rep(1:length(b5_sample_ID),times = iterations), 
               n_spp = NA,
               i = rep(1:iterations, each = length(b5_sample_ID)))

for (j in 1:iterations) {
  # create random sample order
  sID = sample(b5_sample_ID)
  for (i in 1:length(b5_sample_ID)) {
    # sample ID to include
    samp = sID[1:i]
    
    # include only sample numbers
    d = b5 %>%
      filter(sample_ID %in% samp,
             Count > 0)
    
    sp_b5$n_spp[i+((j-1)*length(b5_sample_ID))] = length(unique(d$Species))
  }
}

avg = sp_b5 %>% 
  group_by(n_samp) %>% 
  summarize(n_spp = mean(n_spp, na.rm = T))

ggplot(sp_b5, aes(n_samp, n_spp))+
  geom_line(color = 'grey')+
  geom_line(data = avg, aes(n_samp, n_spp), size = 1)+
  labs(x = 'Number of Samples',
       y = 'Number of Species',
       color = 'Stream')+
  theme_bw()


## Dominance curves / Whittaker curves
Dominance as a function of species rank

df = shrimp_l %>% 
    group_by(STREAM) %>% 
    filter(Count > 0) %>% 
    mutate(Total = sum(Count)) %>% 
    ungroup() %>% 
    group_by(STREAM, Species) %>%
    summarise(Count_Spp = sum(Count),
              Total_Count = max(Total)) %>% 
    mutate(pi = Count_Spp/Total_Count, 
           rank = length(unique(shrimp_l$Species))-rank(pi)) %>% 
    ungroup()

ggplot(df, aes(rank, pi, color = STREAM))+
  geom_line(size = 1)+
  labs(x = 'Species rank',
       y = 'Dominance',
       color = 'Stream')+
  theme_bw()

## K-dominance curves
# Cumulative dominance by species rank


df = shrimp_l %>% 
    group_by(STREAM) %>% 
    filter(Count > 0) %>% 
    mutate(Total = sum(Count)) %>% 
    ungroup() %>% 
    group_by(STREAM, Species) %>%
    summarise(Count_Spp = sum(Count),
              Total_Count = max(Total)) %>% 
    mutate(pi = Count_Spp/Total_Count, 
           rank = length(unique(shrimp_l$Species))-rank(pi)) %>% 
    arrange(rank, .by_group = T) %>% 
  mutate(cumsum = cumsum(pi))

ggplot(df, aes(rank, cumsum, color = STREAM))+
  geom_line(size = 1)+
  labs(x = 'Species rank',
       y = 'Cumulative Dominance',
       color = 'Stream')+
  theme_bw()
