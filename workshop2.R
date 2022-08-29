# """ BSC 6926 B53 
#     Workshop 2: Introduction to R continued
#     authors: Santos and James
#     date: 8/30/2022"""

## Getting to know the basics 

# R is a programming language that has become the standard in Ecology due to its flexibility and open source nature. R can be used from simple math to complex models and is very useful for generating figures. R, like all computer languages, using a specific syntax to run commands that it is programmed to do. In other words, R will only do what it is commanded to do, and therefore, many common errors are due to errors in syntax (e.g. misspellings, missed commas, or unclosed brackets). 
# 
# This example gives a basic intro into R syntax that can be useful for ecological research. This script gives examples of how to:
#   
# 1.  Working with dataframes (10 min)
# 2.  Indexing (5 min)
# 3.  Conditional Statements (5 min)
# 4.  For loops (5 min)
# 5.  Vector operations (5 min)
# + Custom Functions
# + `purr`
# 6.  Figures with `ggplot2` (5 min)
# + Combining plots
# 7. Practice Exercises (30 min)

## Working with `dataframes` and `tibbles`
#Using either `dataframes` or `tibbles` will likely be the most common data structure for ecological data. Making these data structures is easy with the `data.frame()` or `tibble()` functions. Tibbles have more flexibility than dataframes and are part of the `tidyverse`. Dataframes are base R. When reading in tabular data, `read.csv()` will create a dataframe, while `read_csv()` will generate a tibble. `read_csv()` can be paired with `url()` to use data directly from the internet from sites like github. Note that if from github the raw file (click on raw tab when looking at github file) is needed for this to work.

library(tidyverse)
# create a dataframe

df = data.frame(name = c('GOOG', 'AMC', 'GME'),
                Jan = c(1000, 2, 4),
                Feb = c(1010, 15, 30),
                March = c(1005, 25, 180))

df

# create a tibble
tib = tibble(name = c('GOOG', 'AMC', 'GME'),
             Jan = c(1000, 2, 4),
             Feb = c(1010, 15, 30),
             March = c(1005, 25, 180))

tib

#read in data file on computer
# change file path to path location on computer
read.csv('data/LDWFBayAnchovy2007.csv')

read_csv('data/LDWFBayAnchovy2007.csv')

# read in data file from github
# need to use raw file
read_csv(url('https://raw.githubusercontent.com/PCB5423/BSC6926_workshopScripts/master/data/LDWFBayAnchovy2007.csv'))


### Renaming and making columns
#There are a few different ways to create a new column. The base R way is to use `$` with the object name of the dataframe on the left and the new column name on the right. This can be used to do vector operations as well. The other way is to the `mutate()` function which is part of the `dplyr` package in tidyverse. This function alows for more flexibility and can be very useful. The easiest way to rename columns is with `dplyr` functions like `rename()` or within function like `select()`.

df = tibble(name = c('GOOG', 'AMC', 'GME'),
            Jan = c(1000, 2, 4),
            Feb = c(1010, 15, 30),
            March = c(1005, 25, 180))

df$new = 'new column'

df$tot = df$Jan + df$Feb + df$March

df 

# using mutate
df = df %>% 
  mutate(newCol = 'blue')

# multiple columns at a time
df = df %>%
  mutate(sum = Jan + Feb + March, 
         big = sum > 500)
df

# rename columns
df %>%
  rename(Name = name, January = Jan, February = Feb)

# rename, reorder, only include certain columns 
df %>%
  select(Name = name, January = Jan, sum, everything())


### Summarizing data
#There are a few different useful ways to summarize the data in a dataframe or tibble. If you want to know everything about the dataframe, then the base function `summary()` is useful. If you would like to have more control to create summary tables, then `dplyr::summarize()` or `dplyr::summarise()` are great. This can be paired with `group_by()` to summarize over specific groups of data.


summary(iris)

iris %>% 
  summarize(mean(Petal.Width),
            sd(Petal.Width))

iris %>% 
  group_by(Species)%>%
  summarize(mean(Petal.Width),
            sd(Petal.Width))

### Merging and combining mulitple dataframes
#Combining data together is very common, and depending on the type of combination needed. 

#### Binding
#If data has the same column names and needs to paste together, then `rbind()` and `dplyr::bind_rows()` are the tools need. For `rbind()`, the column names need to have the same name. `bind_rows()` does not have this problem.

# bind data together 
sal = tibble(species = rep(c('Salmon'),times = 3),
             year = c(1999,2005,2020),
             catch = c(50, 60, 40))

cod = tibble(species = rep('Cod', times = 3),
             year = c(1999,2005,2020),
             catch = c(50, 60, 100))

crab = tibble(species = rep('Crab', times = 3),
              catch = c(50, 60, 100),
              effort = c(20, 30, 50))

rbind(sal,cod)

rbind(sal, crab)

bind_rows(sal, cod)

bind_rows(sal, crab)

#### Merge/Join
# If two data frames contain different columns of data, then they can be merged together with the family of join functions.
# 
# +`left_join()` = uses left df as template and joins all matching columns from right df 
# +`right_join()` = uses right df as template and joins all matching columns from left df
# +`inner_join()` = only matches columns contained in both dfs
# +`full_join()` = combines all rows in both dfs

left = tibble(name = c('a', 'b', 'c'),
              n = c(1, 6, 7), 
              bio = c(100, 43, 57))

right = tibble(name = c('a', 'b', 'd', 'e'),
               cals = c(500, 450, 570, 600))

left_join(left, right, by = 'name')

right_join(left, right, by = 'name')

inner_join(left, right, by = 'name')

full_join(left, right, by = 'name')

# multiple matches
fish = tibble(species = rep(c('Salmon', 'Cod'),times = 3),
              year = rep(c(1999,2005,2020), each = 2),
              catch = c(50, 60, 40, 50, 60, 100))

col = tibble(species = c('Salmon', 'Cod'),
             coast = c('West', 'East'))

left_join(fish, col, by = 'species')

##  Indexing
#Once data is stored in an object, being able to retrieve those values is useful. Referred to as indexing, the syntax is specific to how the data is stored. With indexing specific values within your object can be modified. 

# vector 
b = 1:15
# 3rd object 
b[3]

# make a character vector 
c = c('a', 'b', 'c')
c
# 2nd object
c[2]
# change 
c[2] = 'new'
c

# dataframe and tibbles
mtcars
# first column
mtcars[1]
# first row
mtcars[1,]
# 2nd row of first column
mtcars[2,1]
# can call specific columns (called as a vector)
mtcars$mpg
mtcars$cyl
#same for tibble
d = mtcars %>% as_tibble
d[1]
d$mpg
d$cyl
# specific row in specific column
mtcars$cyl[1]
d$cyl[1]


## Conditional statements
# In programing there are times that if something is true then you want an operation to occur, but not when a condition is not true. 
# ### Base R
# These can be done with `if` and `if else` statements in base R. These are written if a condition is true then the operation is done. They can be built upon with `else if` if the first condition is false to do test a second condition. If you want it to be If true and if false do something else then `if` and `else` structure can be used. 

b = 5 

if (b == 5){
  cat('b = 5 \n')
}

if (TRUE){
  c = 6
}else(
  c = 10
)
c 
if (F){
  c = 6
}else(
  c = 10
)
c 

if (b == 10){
  cat('b = 10 \n')
}else if (b == 5){
  cat('it worked \n')
}else{
  cat('nothing')
}

### `dplyr` functions
#`dplyr` has two functions that are very useful for conditional statements. Because they are a function they can be vectorized which will be useful as you see below. `if_else()` is a function that based on if the input is `TRUE` or `FALSE` produces a different answer. `case_when()` is more flexible and allows for multple outputs based on conditions being `TRUE`

x = 1:20

if_else(x > 10,
        'x > 10',
        'x < 10')


case_when(x < 6 ~ 'x < 6',
          between(x, 6, 15) ~ '6 < x < 15',
          x > 15 ~ 'x > 15')


##  For loops
#Another useful tool in programming is `for` loops. For loops repeat a process for a certain number of iterations. These can be useful iterate over a dataset or when using information in a time series. The `for` loop works over the number sequence indicated and does the code within the loop (inside of `{}`) for each number in the sequence. The iteration is typically indicated with `i`, but is just an object that is replaced at the begining of each loop and can be anything.

for(i in 1:10){
  print(i)
}

for(turtle in 5:10){
  print(turtle)
}

for(flower in 1:nrow(iris)){
  cat('The species for this iteration is ',
      as.character(iris$Species[flower]), '\n')
}

d = seq(1,15, 2)
d
for(i in 1:length(d)){
  b = d[i] + 1
  cat('d =',d[i], 'b = d + 1 =', b, '\n' )
}

b = 1:10
for (i in 2:10){
  z = b[i] - b[i-1]
  
  cat('z =', z, 'b[i] =', b[i], 'b[i-1] =', b[i-1], '\n')
}


start = 10 
pop = tibble(time = 0:10, n = NA)
pop$n[pop$time == 0] = start
pop
for (t in 1:10){
  growth = rnorm(n =1, mean = 3, sd = 1)
  pop$n[pop$time == t] = growth + pop$n[pop$time == (t-1)]
}
pop

##  Vector operations 
#As we have seen above, we can do operations over vectors. We sometimes want to do this to vectors stored in dataframes/tibbles, and the `mutate()` function makes this easy. 
iris %>% 
  mutate(petalArea = Petal.Length*Petal.Width)

iris %>%
  mutate(petalArea = Petal.Length*Petal.Width,
         PetalSize = if_else(condition = petalArea > 0.2, true ='big',
                             false = 'small'))

iris %>%
  mutate(petalArea = Petal.Length*Petal.Width,
         PetalSize = if_else(condition = petalArea > 0.2, true ='big',
                             false = 'small'))%>%
  group_by(PetalSize)%>%
  summarize(mean = mean(Petal.Width),
            n())

## `purr`
# The newest and new standard package with `tidyverse` is `purr` with its set of `map()` functions. Some similarity to `plyr` (and base) and `dplyr` functions but with more consistent names and arguments. Notice that map function can have some specification for the type of output.
# + `map()` makes a list.
# + `map_lgl()` makes a logical vector.
# + `map_int()` makes an integer vector.
# + `map_dbl()` makes a double vector.
# + `map_chr()` makes a character vector.

df = iris %>%
  select(-Species)
#summary statistics
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

###
#Creating models by group
#Similar of what we did with plyr and dplyr examples
###
data(mtcars)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared) #or map_dbl("r.squared")

###
#Mapping over multiple arguments
###

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% #rnorm - function to extract values from a normal continuous distribution based on some parameters
  
  str()
#> List of 3

#Adding SD as well
sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()
#> List of 3

###
#map2 or pmap allows you to iterate over two or more vectors in parallel
###
map2(mu, sigma, rnorm, n = 5) %>% str()

#or with pmap
n <- list(1, 3, 5)
args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()


## Figures with `ggplot2` 
# The `ggplot2` package is part of the packages that load with `tidyverse` and has become the standard in ecology. The syntax builds upon on a base function and is very customizable [see cheat sheet](https://www.rstudio.com/resources/cheatsheets/). 
# 
# The base of all `ggplot2` begins with `ggplot()` and `geom_...()` are built upon them 

# read in data
df = read_csv(url('https://raw.githubusercontent.com/PCB5423/BSC6926_workshopScripts/master/data/LDWFBayAnchovy2007.csv'))

# plot number of Bay anchovy caught per month
ggplot(df, aes(x = date, y = num))+
  geom_point()

#Show color based on basin number and add line connecting dots

ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()


#Change labels and style of plot

ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  theme_classic()

#Modify the size of axis label text and legend position  

ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance', color = 'Basin')+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom')

#Only plot specific range of the dates on x axis 

ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  scale_x_date(limits = c(lubridate::ymd('2007-04-01'), lubridate::ymd('2007-10-01')))+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())

#Split each trial into own grid

ggplot(df, aes(x = date, y = num))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  facet_wrap(~basin)+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())

#Modify the date labels on x axis ([list of date abbreviations](https://rdrr.io/r/base/strptime.html)) and make 1 column of plots

ggplot(df, aes(x = date, y = num))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  scale_x_date(date_breaks = '2 months', date_labels = '%m/%y')+
  facet_wrap(~basin, ncol = 1)+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())


#Modify the label and size of strip text


# doesn't change the order
labels = c('Calcasieu' = 'CAL',
           'Vermilion-Teche' = 'VER',
           'Terrebonne' = 'TER',
           'Barataria' = 'BAR',
           'Pontchartrain' = 'PON')

ggplot(df, aes(x = date, y = num))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  scale_x_date(date_breaks = '2 months', date_labels = '%m/%y')+
  facet_wrap(~basin, ncol = 1, labeller = as_labeller(labels))+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_text(size = 12))

#Remake figure with the mean Abundance and min and max values from each basin and the summarized line through the points


ggplot(df, aes(x = date, y = num))+
  geom_pointrange(stat = "summary",
                  fun.min = 'min',
                  fun.max = 'max',
                  fun = 'mean')+
  stat_summary(aes(y = num), fun = mean, geom = 'line')+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  scale_x_date(date_breaks = '2 months', date_labels = '%m/%y')+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

#Make box plot of number of seines per month within each basin 

ggplot(df, aes(x = basin, y = seines))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  theme_bw()

#Change order of x axis (make basin order from west to east) and color of plot. Colors can be both hex code or from names that R has. A help website for picking colors is [here](https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html).


df = df %>% 
  mutate(basin = factor(basin, levels = c('Calcasieu',
                                          'Vermilion-Teche',
                                          'Terrebonne',
                                          'Barataria',
                                          'Pontchartrain' )))

colors = c('Calcasieu' = 'darkred',
           'Vermilion-Teche' = 'cadetblue4',
           'Terrebonne' = '#FFC125',
           'Barataria' = '#5d478b',
           'Pontchartrain' = 'grey55')

ggplot(df, aes(x = basin, y = seines, fill = basin))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  scale_fill_manual(values = colors)+
  theme_bw()

#Modify the labels and remove the legend


ggplot(df, aes(x = basin, y = seines, fill = basin))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.title = element_text(size = 18), 
        axis.text.y = element_text(size = 18, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        legend.position = 'none',
        legend.title = element_blank())

### Combining plots 
#Sometimes we would like to combine different sub figures together to make a single figure. There are a few packages that can do this with `ggpubr` and `patchwork` some of the most common. I like `ggpubr` and use this one, but people seem to like `patchwork`. 

library(ggpubr)
library(wesanderson)

a = ggplot(df, aes(x = basin, y = seines, fill = basin))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 14, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        legend.position = 'none',
        legend.title = element_blank())

b = ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance', color = 'Basin')+
  theme_bw()+
  scale_color_manual(values = colors)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom')

# plot combined
ggarrange(a,b,
          labels = c('a)','b)'),
          ncol = 1)

# arrange vertically and move position of labels
ggarrange(a,b,
          labels = c('a)','b)'),
          ncol = 1,
          align = 'v',
          hjust=-1.5)

# common legend
a = ggplot(mtcars, aes(wt, fill = as.character(cyl), 
                       color = as.character(cyl)))+
  geom_density(alpha = 0.4)+
  labs(x = 'Weight of car (tonnes)', 
       fill = '# of engine cylinders')+
  scale_color_manual(values = wes_palette('GrandBudapest1'),
                     guide = "none")+
  scale_fill_manual(values = wes_palette('GrandBudapest1'))+
  theme_bw()+
  theme(axis.title = element_text(size = 10), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 7))

b = ggplot(mtcars, aes(mpg, color = as.character(cyl),
                       fill = as.character(cyl)))+
  geom_density(alpha = 0.4)+
  labs(x = 'Miles/gallon',
       fill = '# of engine cylinders')+
  scale_color_manual(values = wes_palette('GrandBudapest1'),
                     guide = "none")+
  scale_fill_manual(values = wes_palette('GrandBudapest1'))+
  theme_bw()+
  theme(axis.title = element_text(size = 10), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 7))


c = ggplot(mtcars, aes(wt, mpg, group = cyl, color = as.character(cyl)))+
  geom_point(size = 2)+
  geom_smooth(method = 'lm',size = 1)+
  labs(x = 'Weight of car (tonnes)', 
       y = 'Miles/gallon',
       color = '# of engine cylinders')+
  scale_color_manual(values = wes_palette('GrandBudapest1'))+
  theme_bw()+
  theme(axis.title = element_text(size = 10), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 7))


ggarrange(a,b,c, 
          labels = c('A','B','C'),
          nrow = 2,ncol = 2,
          common.legend = F)

ggarrange(a,b,c, 
          labels = c('A','B','C'),
          nrow = 2, ncol = 2,
          common.legend = T,
          legend = 'top')

ggarrange(ggarrange(a,b, labels = c('A','B'), common.legend = T),c,
          labels = c('','C'),
          nrow = 2,
          legend = 'none')


# ## Exercises 
# 1.    Read in the LDWFBayAnchovy2007.csv and create a column that calculates the catch per unit effort (CPUE) for Bay anchovy within the dataframe.
# 
# 2.    Create a dataframe or tibble that contains the basin names for the LDWFBayAnchovy2007.csv dataset (Barataria, Terrebonne, Ponchartrain, Vermilion-Teche, and Calcasieu) and the and abbreviation for each basin as a new column. 
# 
# 3.    Merge the dataframe/tibbles from exercises 1 and 2. 
# 
# 4.    Plot the CPUE for each basin both over time and as a summary of the entire year using a different color for each basin. 