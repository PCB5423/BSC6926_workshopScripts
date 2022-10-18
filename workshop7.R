#' """ Workshop 7: Community data: diversity metrics
#'     @author: BSC 6926 B53
#'     date: 10/18/2022"""

## Community Data
#Community data can vary in format, but typically involves abundance, biomass, or CPUE data for multiple species collected in each sample. Data can be stored in wide (species ID for each column) or long format. The `vegan` package can be useful for calculating diversity metrics. `vegan` calculates metrics from a community matrix (long format).

# calculate metrics for each pool

library(tidyverse)

# group by pool  
shrimp_l = read_csv('data/shrimp.csv') %>% 
  pivot_longer(cols = 7:16, 
               names_to = "Species", 
               values_to = "Count") %>% 
  select(-`...1`) %>% 
  group_by(STREAM, MONTH, YEAR, POOL, Species) %>% 
  summarise(Trap_Total = sum(Count, na.rm = TRUE))

# convert to wide format
shrimp_w = shrimp_l %>% 
  pivot_wider(names_from = Species, values_from = Trap_Total)


### `vegan` diversity functions
# There are useful functions in `vegan` that can be used to calculate diversity metrics.
# 
# `specnumber()` calculates species richness. `diversity()` can calculate Shannon, Simpson, and inverse Simpson metrics.

library(vegan)

shrimp_div = shrimp_w %>% 
  mutate(POOL = as.factor(POOL)) %>% 
  mutate(richness = specnumber(across(ATY:XIPH)),
         H = diversity(across((ATY:XIPH))),
         simp = diversity(across((ATY:XIPH)), "simpson"),
         invsimp = diversity(across((ATY:XIPH)),"inv")) 

#average
average_div = shrimp_div %>% 
  group_by(STREAM, YEAR) %>% 
  summarise(mean_richness = mean(richness, na.rm = TRUE),
            sd_richness = sd(richness, na.rm = TRUE),
            mean_H = mean(H, na.rm = TRUE),
            sd_H = sd(H, na.rm = TRUE),
            mean_simp = mean(simp, na.rm = TRUE),
            sd_simp = sd(simp, na.rm = TRUE),
            mean_invS = mean(invsimp, na.rm = TRUE),
            sd_invS = sd(invsimp, na.rm = TRUE))

average_div

ggplot(average_div, aes(YEAR, mean_richness, color = STREAM))+
  geom_point(size = 2)+
  geom_line(aes(group = STREAM), size = 1)+
  labs(x = 'Year', y = 'Species richness')+
  theme_bw()

## Diversity partitioning (From Stevens 2010 - A primer of ecology with R)

# We frequently refer to biodiversity (i.e., richness, Simpson’s, and Shannon diversity) at different spatial scales as $\alpha$, $\beta$, and $\gamma$ diversity.\
# 
# Alpha diversity, $\alpha$, is the diversity of a point location or of a single sample.\
# Beta diversity, $\beta$, is the diversity due to multiple localities and can be used to describe differences in species composition among sites.\
# Gamma diversity, $\gamma$, is the diversity of a region, or at least the diversity of all the species in a set of samples collected over a large area (with large extent relative to a single sample).
# 
# Diversity across spatial scales can be further be partitioned in one of two ways, either using additive or multiplicative partitioning.\

### Additive partitioning
# _Additive partitioning_ is 
# $$\overline{\alpha} + \beta = \gamma$$
#   where $\alpha$ is the average diversity of samples, $\gamma$ is the diversity of pooled samples and $\beta$ is found  as 
# $$\beta = \gamma - \overline{\alpha}$$
#   We can think of $\beta$ as the average number of species not found in a sample, but which we know to be in the region. Additive partitioning allows direct comparison of average richness among samples at any hierarchical level of organization because all three measures of diversity $\alpha$, $\beta$, and $\gamma$ are expressed in the same units. This makes it analogous to partitioning variance in ANOVA. This is not the case for multiplicative partitioning diversity.

# gamma diversity
gammaDiv = length(unique(shrimp_l$Species))

# calculate beta diversity
betaDiv = shrimp_div %>% 
  group_by(STREAM, YEAR) %>% 
  summarise(alpha = mean(richness, na.rm = TRUE),
            gamma = gammaDiv,
            beta_a = gamma - alpha)

# plot 
library(ggpubr)

a = ggplot(betaDiv, aes(YEAR, alpha, color = STREAM, group = STREAM))+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits = c(0,10))+
  labs(x = 'Year', y = expression(alpha ~ 'diversity'), 
       color = 'Stream')+
  theme_bw()

b = ggplot(betaDiv, aes(YEAR, beta_a, color = STREAM, group = STREAM))+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits = c(0,10))+
  labs(x = 'Year', y = expression(beta ~ 'diversity'), color = 'Stream')+
  theme_bw()

ggarrange(a,b,nrow =1, common.legend = T,align = 'h')

### Multiplicative partitioning
# _Multiplicative partitioning_ is 
# $$\overline{\alpha} \beta = \gamma$$ and 
# 
# $$\beta = \gamma/\overline{\alpha}$$
#   where $\beta$ is a conversion factor that describes the relative change in species composition among samples. Sometimes this type of $\beta$ diversity is thought of as the number of different community types in a set of samples. However, use this interpretation with great caution, as $\beta$ diversity depends completely on the sizes or extent of the samples used for $\alpha$ diversity.

betaDiv = betaDiv %>% 
  mutate(beta_m = gamma/alpha)


b = ggplot(betaDiv, aes(YEAR, beta_a, color = STREAM, group = STREAM))+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits = c(0,10))+
  labs(x = 'Year', y = expression('Additive'~ alpha ~ 'diversity'), 
       color = 'Stream')+
  theme_bw()

m = ggplot(betaDiv, aes(YEAR, beta_m, color = STREAM, group = STREAM))+
  geom_point()+
  geom_line()+
  scale_y_continuous(limits = c(0,10))+
  labs(x = 'Year', y = expression('Multiplicative'~beta ~ 'diversity'), color = 'Stream')+
  theme_bw()

ggarrange(b,m,nrow =1, common.legend = T,align = 'h')

## Species composition/Community structure
# Instead of distilling community data into metrics, comparisons can be made doing with the multivariate method. This is commonly done with distance or dissimilarity, and represent how similar communities are.

# filter to 2 species in 1 stream for visualization
b5_spp = shrimp_l %>% 
  filter(STREAM == "B5", Species %in% c("ATY", "MACcarc")) %>% 
  group_by(POOL, Species) %>% 
  summarise(mean_count = mean(Trap_Total, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = mean_count) %>% 
  tibble() 

ggplot(b5_spp, aes(ATY, MACcarc, color = as.factor(POOL))) +
  geom_point(size = 4)+
  labs(color = 'Pool')+
  theme_bw()


### Euclidean distance
# One way to measure the distance between two points is to use euclidean distance, which is the straight line distance between two points. Euclidean distance between two points $d(p,q)$ can be measured using the following formula
# $$d(p,q) = \sqrt{\sum_{i = 1}^{n} (p_i - q_i)^2}$$
#   
#   Note that this is not great for community data.

#Example - Euclidean distance between Pool 0 and Pool 13

aty_diff = b5_spp$ATY[3] - b5_spp$ATY[5] #Atya abundance pool 0 - pool 13
mac_diff = b5_spp$MACcarc[3] - b5_spp$MACcarc[5] #MAC abundance pool 0 - pool 13

sqrt(aty_diff^2+mac_diff^2)

### Bray-Curtis 
# A common distance method for community data is Bray-Curtis. 
# This is the difference in species abundance between two sites divided by the total abundance at each site. 
# Interpret as the proportion of all individuals that would remain unpaired - percentage of dissimilarity. Reflects changes in composition and changes in relative. This can be calculated with the `vegan` package. 

#First, lets create a community species matrix for just B5 and pools

b5_pool_comm = shrimp_l %>% 
  filter(STREAM == "B5") %>% 
  group_by(POOL, Species) %>% 
  summarise(mean_count = mean(Trap_Total, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = mean_count) %>% 
  mutate(POOL = as.factor(POOL)) %>% 
  column_to_rownames(var = "POOL") %>% 
  data.frame() 

b5_pool_comm 

euc_dist = vegdist(b5_pool_comm, method = "euclidean")
euc_dist

bray_dist = vegdist(b5_pool_comm, method = "bray")
bray_dist

### Jaccard
# Another common way to analyze community data is with presence absence data. This is best compared with Jaccard dissimilarity. 
# Represents the proportion of unshared species. Frequently used to interpret turnover. 
# To some extent, the average Jaccard dissimilarity is a measure of beta diversity.

jac_dist = vegdist(b5_pool_comm, method = "jaccard")
jac_dist

mean(jac_dist)

### Plot distance in ordination space
# Ordination - represent data along a reduced number of orthogonal axis. 
# Or, show us patterns of relationship between samples in the high dimensional space in way smaller number of dimension (2 or 3, or more depending on complexity). 
# Different techniques, which use will depend on the research question or objectives e.g., Principal component analysis, Correspondence Analysis, Pricinpal Coordinate Analysis, and MDS.
# 
# non-metric MDS, most commonly used for data exploration and illustrate patterns. Technique that maximize the rank correlation between dissimilarity matrix and n dimensions space
# through an iterative process.

b5.nmds.bc = metaMDS(b5_pool_comm, distance = "bray", k = 3, try = 100)
b5.nmds.ec = metaMDS(b5_pool_comm, distance = "euclidean", k = 3, try = 100)
b5.nmds.jc = metaMDS(b5_pool_comm, distance = "jaccard", k = 3, try = 100)

plot(b5.nmds.bc, display = "sites", type = "text")

# The output is a list, so need to extract data to plot in ggplot

nmds_output = bind_rows(bc = data.frame(b5.nmds.bc[["points"]]),
                        ec = data.frame(b5.nmds.ec[["points"]]),
                        jc = data.frame(b5.nmds.jc[["points"]])) %>% 
  mutate(Pool = rep(unique(b5_spp$POOL), times = 3),
         Dissimilarity = rep(c("Bray", "Euclidean", "Jaccard"),
                             each = length(unique(Pool))))

ggplot(nmds_output, aes(MDS1, MDS2, color = as.factor(Pool)))+
  geom_point(size = 2)+ 
  facet_wrap(~Dissimilarity)+
  labs(color = 'Pool')+
  theme_bw()

## Diversity partitioning 
# Analysis of species replacement (turnover) and richness differences (or nestedness) based on Podani or Baselga Family Indices. From Chapter 8 - Numerical Ecology with R


library(adespatial)

beta.div.comp(b5_pool_comm, coef = "BJ")

b5_spp.2 = shrimp_l %>% 
  filter(STREAM == "B5") %>% 
  mutate(PA = if_else(Trap_Total > 0, 1, 0)) %>% 
  group_by(POOL, Species) %>% 
  summarise(mean_count = mean(Trap_Total, na.rm = TRUE),
            mean_PA = mean(PA, na.rm = TRUE)) 


# Let's explore how presence/absence and counts varies between species and pools

PA_dist = ggplot(b5_spp.2, aes(x = Species, y = as.factor(POOL), fill = mean_PA))+
  labs(y = 'Pool')+
  geom_raster()

Co_dist = ggplot(b5_spp.2, aes(x = Species, y = as.factor(POOL), fill = mean_count))+
  labs(y = 'Pool')+
  geom_raster()

ggarrange(PA_dist, Co_dist,
          nrow = 2, align = 'v')

# ## Exercises
# For these exercises use the [LDWF seine sampling dataset](https://raw.githubusercontent.com/PCB5423/BSC6926_workshopScripts/master/data/Calcasieu.csv). This dataset is the abundance for each species (in wide format) for 6 sites over 1 year.
# 
# 1. Using the Calcasieu dataset calculate the mean and SD for species richness, Shannon, Simpson, and inverse Simpson for each sampling site.
# 
# 2. Plot the dominance (Whittiker) and K-dominance curves for each site.
# 
# 3. Plot the $\alpha$, $\beta$, and $\gamma$ diversity for each site for each sampling date in 2007. Use either additive or multiplicative $\beta$ diversity. Plot diversity over time. 
# 
# 4. Plot the Bray-Curtis and Jaccard dissimilarity for each site. 

