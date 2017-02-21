# DAU: PLY107
# Analysis of experimental data.

# Let's start by importing the data that we want to analyze:
testdata <- read.csv("coldata/ply107testdata.csv")

# First of all we need the descriptive statistics. We need
# to load up the psych package for this:

library(psych)

# The following function will give us our descriptive statistics:
descrip <- describeBy(testdata$resp, testdata$stim, mat = TRUE)
print(descrip)
write.csv(descrip, 'analysis/Exp3Descriptives.csv', row.names = FALSE)

# This function produces graphs. A bar chart of the mean
# ratings for each stimuli is a good way of visualising the
# data. First graph has a title, while the second is a direct
# replication with no title:

library(graphics)

barplot(descrip$mean, names = descrip$group1, xlab = "Stimulus",
        ylab = "Mean Rating (0-10)", main = 'Experiment 3 Results',
        ylim = c(0, 10))

barplot(descrip$mean, names = descrip$group1, xlab = "Stimulus",
        ylab = "Mean Rating (0-10)", ylim = c(0, 10))

# We will need to aggregate the data for the next steps:
aggdata <- aggregate(testdata$resp, list(testdata$partic,
                     testdata$stim), mean)

colnames(aggdata) <- c('partic', 'stim', 'resp')

# We will need to run a series of t.tests based on specific
# experimental hypotheses. For each example we will need to extract 
# a subset of the data to analyse, after which we can run the t.test:

# 1. Testing X and Y for the Redundancy Effect:
RE.aggdata <- subset(aggdata, stim == 'X' | stim == 'Y')
t.test(resp ~ stim, data = RE.aggdata,paired = TRUE)

# 2. Testing Z and Y for the Relative Validity Effect:
RVE.aggdata <- subset(aggdata, stim == 'Z' | stim == 'Y')
t.test(resp ~ stim, data = RVE.aggdata,paired = TRUE)

# 3. Testing X and Z for differences:
DIFF.aggdata <- subset(aggdata, stim == 'X' | stim == 'Z')
t.test(resp ~ stim, data = DIFF.aggdata, paired = TRUE)

# The results show significant differences between the means
# for all three tests.

# We also need to test for blocking. In other words, is there a
# significant difference between the overshadowing cues and the
# blocked cue at test:

# 4. Testing P and X for differences:
BL.aggdata <- subset(aggdata, stim == 'P' | stim == 'X')
t.test(resp ~ stim, data = BL.aggdata, paired = TRUE)

# 5 Testing Q and X for differences:
BL2.aggdata <- subset(aggdata, stim == 'Q' | stim == 'X')
t.test(resp ~ stim, data = BL2.aggdata, paired = TRUE)

# These differences are not significant, which means the data
# does not provide evidence of blocking compared to the 
# overshadowing control.

# Now it's time to perform an ANOVA our data set (this is for
# example purposes only).

# This loads up the ez package within R (please note that you will
# need to download this package if you have not done so already):

library(ez)

# This is an ANOVA looking at whether there is a significant
# difference between the mean scores for each of the stimuli:

output <- ezANOVA(data = aggdata
                   , dv = resp
                   , wid = partic
                   , within = stim
                   , type = 3)

print(output)

################################################################

#theansweris42
