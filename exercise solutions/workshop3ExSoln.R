#' """ Solutions to Workshop 3 exercises
#'     @author: BSC 6926 B53
#'     date: 9/13/2022"""

library(tidyverse)
library(ggpubr)


#1 - Download primer and open primer package. Upload the sparrows dataset
#    from package and plot counts as function of years.
install.packages('primer')
library(primer)

data("sparrows")

ggplot(data=sparrows, aes(x=Year, y=Count)) +
  geom_line() + 
  geom_point() +
  theme_bw()

#2 - Calculate the annual rate of increase (λ) for each time step 
#    and plot λ as function of years as a scatterplot
#    Hint: Look at the for loop/dplyr examples above

# for loop
df = sparrows

# make empty column to store data
df$lambda = NA


for (i in 1:length(df$Year)){
  df$lambda[i] = df$Count[i+1]/df$Count[i]
}

#dpylr 
df = df %>% 
  mutate(labmda2 = lead(Count)/Count)


#3 - Challenge: Simulate a population with varying λ at each time step.

# Use random numbers to generate lambda
# initial parameters
N_0 = 10
years = 100
# store data
sim = tibble(time = 0:years,
             Nt = NA)
# set N0
sim$Nt[sim$time == 0] = N_0
sim

# calculate random lambda from normal distribution 
sim = sim %>% 
  mutate(lambda = rnorm(nrow(sim), mean = 1.05, sd = 0.2))

for(i in 2:nrow(sim)){
  sim$Nt[i] = sim$lambda[i-1]*sim$Nt[i-1]
}

ggplot(sim, aes(time, Nt))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  labs(x = 'Time', y = expression(italic(N[t])))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


# simulate from sparrow lambdas
# initial parameters
N_0 = 10
years = 100
# store data
sim2 = tibble(time = 0:years,
              Nt = NA)
# set N0
sim2$Nt[sim2$time == 0] = N_0
sim2

# place to store lambdas
sim2$lambda = NA

# vector of lambdas from sparrow data
# drop the NAs
ls = df$lambda[!is.na(df$lambda)] 

for (i in 1:(length(sim2$time)-1)){
  sim2$lambda[i] = sample(ls, 1)
  sim2$Nt[i + 1] = sim2$lambda[i]*sim2$Nt[i]
}

ggplot(sim2, aes(time, Nt))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  labs(x = 'Time', y = expression(italic(N[t])))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))