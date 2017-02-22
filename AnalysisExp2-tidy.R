# DAU: PLY101
# Analysis of experimental data.

## This is a modified version of sspicer's AnalysisExp2 script, 
## using tidyverse for data manipulation. Staying within tidyverse
## makes data manipulation simpler since the syntax is consistent
## across all packages -- otherwise you'd have to learn a new syntax
## for every package...
## tidyverse (and ggplot) is developed by the same people who develop RStudio.
##
## I tried to keep the same steps and output from sspicers original script.

## Loading the tidyverse packages - of course it needs to be installed first...
## Have a look at the descriptions and books of all the packages at http://tidyverse.org/
library(tidyverse)

## lightweight library for skew & kurtosis
library(moments)

## Instead you could use skew() and kurtosi() from the psych package
#- library(psych)

# The first step is to import the data we want to analyse:
#- testdata <- read.csv("coldata/ply101testdata.csv")
## loading data using readr (part of tidyverse)
testdata <- read_csv("coldata/ply101testdata.csv")

# We need to represent the experimental stage in a non-numeric format
# for analysis:
#-testdata$stg <- NULL
#-testdata$stg[testdata$stage == '2'] <- 'Test1'
#-testdata$stg[testdata$stage == '4'] <- 'Test2'

## Changing string values using a "code book" the
testdata <- testdata %>% 
  mutate(stg = case_when(
    (.$stage == '2') ~ 'Test1',
    (.$stage == '4') ~ 'Test2'
  ))
           
# First of all we need the descriptive statistics (we need
# to load up the psych package for this):

#-library(psych)

# Descriptives are generated using the following function:


#-descrip <- describeBy(testdata$resp,list(testdata$stim,
#-                                         testdata$stg),mat=TRUE)

## The "tidyverse-way" of descriptive stats in detail. 
## This gives more control over each aggregated value...
descrip <- testdata %>%
  group_by(group1 = stim, group2 = stg) %>%
  summarise(
    n = n() ,
    mean = mean(resp),
    sd = sd(resp),
    median = median(resp),
    trimmed = mean(resp, trim = 0.1),
    mad = mad(resp),
    min = min(resp),
    max = max(resp),
    skew = skewness(resp),
    kurtosis = kurtosis(resp),
    # skew = skew(resp),        # If you want to use the psych package
    # kurtosis = kurtosi(resp), # note that they result in different numbers
    # for a discussion look in the psych package
    se = sqrt(var(resp)/length(resp))
  )

print(descrip)
#-write.csv(descrip,'analysis/Exp2Descriptives.csv',row.names=FALSE)
## Using readr to write out the file
write_csv(descrip, 'analysis/Exp2Descriptives.csv')


# This next function produces graphs. A bar chart of the mean
# ratings for each stimuli is a good way of visualising the
# data. First graph has a title, while the second is a direct
# replication with no title:

# (n.b. I have not quite worked out the correct code for displaying
# data from both test stages in a single graph)

#-library(graphics)

library(ggplot2)

#- barplot(descrip$mean, names = descrip$group1, legend = descrip$group2,
#-         xlab = "Stimulus", ylab = "Mean Rating (0-10)",
#-         main = 'Experiment 3 Results', ylim = c(0, 10))

## Similar plot using ggplot
## ggplot syntax is very similar to other tidy packages, but using + instead of %>%
my.plot <- ggplot(descrip) +                           # create an (empty) plot with descrip as data
  geom_bar(                                 # do a bar-plot
    aes(group1, mean),                      # aesthetics with group1 as X, mean as Y
    stat = "identity") +                    # plot actual values
  facet_grid(. ~ group2) +                  # make separate plots for each group2
  labs(x = "Stimulus",                      # legend for x-axis
       y = "Mean Rating (0-10)",            # legend for y-axis
       title = "Experiment 3 Results") +    # title
  ylim(0, 10) +                             # y-axis limits
  theme_bw()                                # apply a "printing" theme

print(my.plot)
#####################################################################

# Now it's time to perform the analysis our data set.

# We will need to aggregate the data to do this:
#-aggdata <- aggregate(testdata$resp,list(testdata$partic, testdata$stg,
#                                        testdata$stim),mean)
#-colnames(aggdata) <- c('partic','stage','stim','resp')

## Aggregate using dplyr
aggdata <- testdata %>%
  group_by(partic, stage = stg, stim) %>%
  summarise(resp = mean(resp)) %>% ungroup()

# Also we will need to split out the two test stages:
#-aggdata1 <- subset(aggdata,stage == 'Test1')
#-aggdata2 <- subset(aggdata,stage == 'Test2')
## filter observations by stage
aggdata1 <- aggdata %>%
  filter(stage == 'Test1')
aggdata2 <- aggdata %>%
  filter(stage == 'Test2')

# This loads up the ez package within R, which we need for the
# analysis (needs to be downloaded if you don't have it):

library(ez)

# First step is to see whether there is a redundancy effect
# at both test stage 1 and test stage 2.

# 1. Testing X and Y for the Redundancy Effect (stage 1):
#-RE.aggdata1 <- subset(aggdata1,stim == 'X' | stim == 'Y')
## using filter
RE.aggdata1 <- aggdata1 %>% 
  filter(stim %in% c('X','Y'))
t.test(resp ~ stim, data = RE.aggdata1, paired = TRUE)

# 2. Testing X and Y for the Redundancy Effect (stage 2):
#-RE.aggdata2 <- subset(aggdata2,stim == 'X' | stim == 'Y')
## using filter
RE.aggdata2 <- aggdata2 %>% 
  filter(stim %in% c('X','Y'))
t.test(resp ~ stim, data = RE.aggdata2, paired = TRUE)

# There is a significant redundancy effect at both stages.
# Next is to see whether stage1 X is significantly different
# from stage2 X, and whether stage1 Y is significantly
# different from stage2 Y.

# 3. Testing for significant difference between stage 1
# and stage 2 ratings for X:
#-X.aggdata <- subset(aggdata,stim == 'X')
## filter observations
X.aggdata <- aggdata %>%
  filter(stim == 'X')
t.test(resp ~ stage, data = X.aggdata, paired = TRUE)

# 4. Testing for significant difference between stage 1
# and stage 2 ratings for Y:
#-Y.aggdata <- subset(aggdata,stim == 'Y')
## filter observations
Y.aggdata <- aggdata %>% 
  filter(stim == 'Y')
t.test(resp ~ stage, data = Y.aggdata, paired = TRUE)

# The results show that the scores are different at each
# stage. The key question now is whether there is a significant
# difference between the differences (i.e. an interaction
# effect) - there isn't:

#XY.aggdata <- subset(aggdata,stim == 'Y' | stim == 'X')
## filter observations
XY.aggdata <- aggdata %>%
  filter(stim %in% c('Y', 'X')) %>%
  mutate(
    partic = factor(partic),
    stim = factor(stim),
    stage = factor(stage))

output <- ezANOVA(data = XY.aggdata
                   , dv = resp
                   , wid = partic
                   , within = c('stim','stage')
                   , type = 3)

print(output)

#####################################################################

#solongandthanksforallthefish
