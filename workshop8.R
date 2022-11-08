#' """ Workshop 8: Food webs: stable isotope mixing models
#'     @author: BSC 6926 B53
#'     date: 11/8/2022"""

# This workshop covers using stable isotope mixing models to estimate resource use in food webs.\
# 
# [R script of workshop 8](workshop8.R)
# 
# 
# ## Stable isotope data
# Energy flow is a key ecosystem function in all ecosystems, and can be understood through understanding food web interactions. Stable isotopes of C, N, and S are a common method to investigate food webs. Stable isotope data is reported in delta ($\delta$) notation and calculated with the following formula:
#   $$ 
#   \delta X = ((R_{sample}/R_{standard}) - 1) * 1000
# $$
#   where $\delta X$ is the stable isotope ratio of element $X$ of the sample relative to a standard value, $R_{sample}$ is the ratio of rare isotope to common isotope (e.g. ^13^C/^12^C) of the sample of element $X$, and $R_{standard}$ is the ratio of a standard of element $X$. Standards are specific to each element and are Pee Dee Belmenite for carbon, air for nitrogen, and Vienna Canyon Diablo Troilite for sulfur. The units are reported in per mil (‰).\
# 
# ### Isotope biplots
# Stable isotope values are typically visualized using a biplot with $\delta$^13^C values on the x-axis and $\delta$^15^N or $\delta$^34^S values on the y-axis. Typical plots have source values plotted with mean $\pm$ SD and consumers plotted as individual values of the sample. 
# 
# ```{r}
# # calculate metrics for each pool

library(tidyverse)

bf = read_csv('data/bf.csv')
s = read_csv('data/sFLbayMan.csv')

# carbon and nitrogen
ggplot(s, aes(Meand13C, Meand15N))+
  geom_point(size = 3, pch=20)+
  geom_errorbar(aes(ymin = Meand15N - SDd15N, ymax = Meand15N + SDd15N), width = 0)+
  geom_errorbarh(aes(xmin = Meand13C - SDd13C, xmax =  Meand13C + SDd13C), height = 0)+
  geom_text(aes(label = Source),hjust=-.1, vjust=-1) +
  geom_point(data = bf, aes(x = d13C, y = d15N,color = ID), size=3, pch=c(20))+
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))+
  theme_bw()

# carbon and sulfur
ggplot(s, aes(Meand13C, Meand34S))+
  geom_point(size = 3, pch=20)+
  geom_errorbar(aes(ymin = Meand34S - SDd34S, ymax = Meand34S + SDd34S), width = 0)+
  geom_errorbarh(aes(xmin = Meand13C - SDd13C, xmax =  Meand13C + SDd13C), height = 0)+
  geom_text(aes(label = Source),hjust=-.1, vjust=-1) +
  geom_point(data = bf, aes(x = d13C, y = d34S,color = ID), size=3, pch=c(20))+
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{34}, "S (\u2030)")))+
  theme_bw()

# ## Mixing models
# The saying you are what you eat is true, and consumers become a mixture of the of the atoms they consume. Therefore, if the sources are known a consumer can become 'unmixed' into its sources. This is because stable isotopes are predictable in how they change through the food web (i.e. trophic fractionation). It is important to note that this can only be done when all sources are accounted for and the sources differ in isotope values.\
# 
# Stable isotope mixing models use a system of equations to mass balance the mixture of source isotope values to make the consumer's isotope value. The simplest form of a stable isotope mixing model is with 2 sources (also commonly referred to as endmembers) and 1 isotope. In a 2 source, 1 isotope mixing model system:
# $$
# \delta X_{c} = f_1*\delta X_1 + f_2*\delta X_2
# $$
# where $\delta X_{c}$ is the stable isotope value of element $X$ for the consumer, $f_1$ is contribution of source 1 to the consumer (i.e. resource use of source 1), $f_2$ is the contribution of source 2, $\delta X_1$ is the isotope value element $X$ of source 1, and $\delta X_2$ is the isotope value element $X$ of source 2.\
# 
# Since the consumer can only use these 2 sources we also know that
# 
# $$
# f_1 + f_2 = 1
# $$
# We can rearrange and substitute to solve for $f_1$ with this equation:
# $$
# \delta X_{c} = f_1*\delta X_1 + (1 - f_1)*\delta X_2
# $$
# which can be rearranged to be 
# $$
# f_1 = (\delta X_{c}-\delta X_{2})/(\delta X_{1}-\delta X_{2})
# $$
# Since we know the isotope values of the consumer ($\delta X_{c}$) and sources ($\delta X_1$ and $\delta X_2$) we can then solve for $f_1$ and $f_2$. 

# source isotope values 
d13C_s1 = -27 # mangrove
d13C_s2 = -12 # seagrass

# generate random consumer values between endmembers calculate 
df = tibble(d13C_c = runif(20, min = d13C_s1, max = d13C_s2), 
            d13C_s1 = d13C_s1, d13C_s2 = d13C_s2) 

# calculate each contribution
df = df %>% 
  mutate(f1 = (d13C_c-d13C_s2)/(d13C_s1-d13C_s2),
         f2 = 1 - f1)

df

### Bayesian mixing models
# Since food webs are not as simple as the example above, researchers have developed more complex models to account for variation in source values, trophic fractionation, concentration of each element in a source, and underdetermined systems (when # of sources > # of isotopes + 1). These models use a Bayesian framework, and `MixSIAR` is the most common. MixSIAR uses an R interface to run Bayesian hierarchical models by Markov Chain Monte Carlo with JAGs. You will need to download [JAGs](https://sourceforge.net/projects/mcmc-jags/) for `MixSIAR` to run. In addition, `MixSIAR` has a great [manual](https://github.com/brianstock/MixSIAR) with many examples of how to use the package. 

library(MixSIAR)
library(tidyverse)
options(max.print = 6000000)

# load consumer data
df = read_csv('data/bf.csv')
df
mix = load_mix_data(file = "data/bf.csv",
                    iso_names=c("d13C","d34S", 'd15N'),
                    factors= c("ID"),
                    fac_random=c(F),
                    fac_nested=c(F),
                    cont_effects=NULL)

# load source data
df = read_csv('data/sFLbayMan.csv')
df
source = load_source_data(file = "data/sFLbayMAN.csv",
                          source_factors=NULL,
                          conc_dep=TRUE,
                          data_type="means",
                          mix)
# load TEF data
df = read_csv("data/FLbayTEF3.csv")
df
discr = load_discr_data(file = "data/FLbayTEF3.csv", mix)

# Make an isospace plot
#plot_data(filename="isospace_plot", plot_save_pdf=FALSE, plot_save_png=FALSE, mix,source,discr)

# Write the JAGS model file
model_filename = "MixSIAR_model.txt"
resid_err = F #variation in consumer assimilation (Stock and Semmens 2016 for in depth explanation https://esajournals-onlinelibrary-wiley-com.ezproxy.fiu.edu/doi/10.1002/ecy.1517)
process_err = T #variation in sampling of source values
write_JAGS_model(model_filename, resid_err, process_err, mix, source)


# run model 
# | run ==  | Chain Length | Burn-in | Thin | # Chains |
#   | ------------- | ------------- | ------------- | ------------- | ------------- |
#   | "test" | 1,000 | 500 | 1 | 3 |
#   | "very short" | 10,000 | 5,000 | 5 | 3 |
#   | "short" | 50,000 | 25,000 | 25 | 3 |
#   | "normal" | 100,000 | 50,000 | 50 | 3 |
#   | "long" | 300,000 | 200,000 | 100 | 3 |
#   | "very long" | 1,000,000 | 500,000 | 500 | 3 |
#   | "extreme" | 3,000,000 | 1,500,000 | 500 | 3 |

# run jags
jags.bf = run_model(run="long", mix, source, discr, model_filename,
                    alpha.prior = 1, resid_err, process_err)

# Process JAGS output
output_bf = list(summary_save = TRUE,
                 summary_name = "MixingModels/mm_results/bf_ss",
                 sup_post = FALSE,
                 plot_post_save_pdf = FALSE,
                 plot_post_name = "lower_posterior_density",
                 sup_pairs = FALSE,
                 plot_pairs_save_pdf = FALSE,
                 plot_pairs_name = "lower_pairs_plot",
                 sup_xy = TRUE,
                 plot_xy_save_pdf = FALSE,
                 plot_xy_name = "lower_xy_plot",
                 gelman = TRUE,
                 heidel = FALSE,
                 geweke = TRUE,
                 diag_save = TRUE,
                 diag_name = "mm_results/bf_diag",
                 indiv_effect = FALSE,
                 plot_post_save_png = F,
                 plot_pairs_save_png = FALSE,
                 plot_xy_save_png = FALSE)

output_JAGS(jags.bf, mix, source, output_bf)


### Load mixing model data
# 
# This is a custom function that was written to import the output of the mixing models 
# 
# **note** for this to work, all columns must be on the same row like this:
# 
# and not wrapped like this:
# 
# Use the `mixTable` function to load data
# 
# `file =` name of name of .txt of summary stats from `MixSIAR`
# 
# `type =` 'identifier' for use of the data set
# 
# `ind = F` if `TRUE` will output with columns as source values, default is `FALSE` and will output data with columns as mean, sd, and posterior distribution quantiles: lowend = 2.5%, highend = 97.5%, mid = 50%, low = 25%, up = 75%, ymax = 75% quantile + 1.5 * IQR, ymin = 25% quantile - 1.5 * IQR
# 
# `nest = F` if nested mixing model, `TRUE` will return the sources of nested

mixTable = function(file,type,ind = F,nest = F){
  require(tidyverse)
  cn = c('ID', 'Mean', 'SD', '2.5%', '5%', '25%', '50%', '75%', '95%', '97.5%')
  x = read_fwf(file, skip = 8)
  names(x) = cn
  x$source = NA
  x$name = NA
  x$code = NA
  
  if (nest == F){
    for (i in 1:nrow(x)){
      temp = strsplit(x$ID, split = '.', fixed = T)
      x$source[i] = temp[[i]][3]
      x$name[i] = temp[[i]][2]
      
      x$type = type
      x$ymax = x$`75%` + 1.5*(x$`75%` - x$`25%`)
      x$ymin = x$`25%` - 1.5*(x$`75%` - x$`25%`)
      
      df = data.frame(x$name, x$type, x$source, x$Mean, x$SD, x$`2.5%`, x$`97.5%`,
                      x$`50%`, x$`25%`, x$`75%`, x$ymax, x$ymin)
      colnames(df) = c('name', 'type', 'source', 'mean', 'sd', 'lowend', 'highend',
                       'mid', 'low', 'up', 'ymax', 'ymin')
    }
  }else{
    for (i in 1:nrow(x)){
      temp = strsplit(x$ID, split = '.', fixed = T)
      x$source[i] = temp[[i]][4]
      x$code[i] = temp[[i]][3]
      x$name[i] = temp[[i]][2]
      
      x$type = type
      x$ymax = x$`75%` + 1.5*(x$`75%` - x$`25%`)
      x$ymin = x$`25%` - 1.5*(x$`75%` - x$`25%`)
      
      df = tibble(x$name, x$type, x$source, x$code, x$Mean, x$SD, x$`2.5%`, x$`97.5%`,
                  x$`50%`, x$`25%`, x$`75%`, x$ymax, x$ymin)
      colnames(df) = c('name', 'type', 'source', 'code', 'mean', 'sd', 'lowend', 'highend',
                       'mid', 'low', 'up', 'ymax', 'ymin')
    }
  }
  
  for (i in 1:nrow(df)){
    if (df$ymax[i] > df$highend[i]){
      df$ymax[i] = df$highend[i]
    }
    if (df$ymin[i] < df$lowend[i]){
      df$ymin[i] = df$lowend[i]
    }
  }
  df = df %>% drop_na %>%
    filter(name != 'global')
   
  
  if (ind == T){
    if (nest == T){
      df = df %>% select(name, type, code, source, mean) %>%
        pivot_wider(names_from = 'source', values_from = 'mean')
    }else{
      df = df %>% select(name, type, source, mean)%>%
        pivot_wider(names_from = 'source', values_from = 'mean')
    }
  }
  
  return(df)
}


bf = mixTable('data/bf_ss.txt', 'Bonefish', ind = T, nest = F)
bf

b = bf %>% 
  pivot_longer(Algae:Seagrass, names_to = 'source', values_to = 'value')

ggplot(b, aes(source,value, fill = source))+
  geom_boxplot(alpha = 0.6)+
  geom_point(aes(color = source), position = 'jitter')+
  labs(x = NULL, y = 'Source Contribution')+
  theme_bw()+
  theme(legend.position = 'none')

### Calculate trophic level
# You can calculate the trophic level ($TL$) of a consumer with the folowing formula:
# $$
# TL = \frac{\delta^{15}N_c - \sum f_i*\delta^{15}N_i}{\Delta^{15}N} + 1
# $$
# where $\delta^{15}N_c$ is the consumer $\delta^{15}N$ of the consumer and $\sum f_i*\delta^{15}N_i$ is the isotope value of the consumer at trophic level 1 based on the mixing model outputs for contribution of source $i$ ($f_i$) multiplied by the $\delta^{15}N$ of that source, and $\Delta^{15}N$ is the trophic enrichment factor for $\delta^{15}N$. 

# combine mixing model output with isotope source files
bf1 = bf %>% 
  select(ID = name, Algae, Epiphytes, Mangrove, Seagrass)

df = read_csv('data/bf.csv') %>% 
  left_join(bf1, by = 'ID')

s = read_csv('data/sFLbayMan.csv')

tef = 3

df = df %>%
  mutate(nTL1 = Algae*s$Meand15N[s$Source == 'Algae']+
                Epiphytes*s$Meand15N[s$Source == 'Epiphytes']+
                Mangrove*s$Meand15N[s$Source == 'Mangrove']+
                Seagrass*s$Meand15N[s$Source == 'Seagrass'],
         TL = (d15N - nTL1)/tef + 1)

df$TL


