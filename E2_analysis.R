# DAU: PLY101
# Analysis of experimental data.

# The first step is to import the data we want to analyse:
testdata <- read.csv("coldata/ply101testdata.csv")

# We need to represent the experimental stage in a non-numeric format
# for analysis:
testdata$stg <- NULL
testdata$stg[testdata$stage == '2'] <- 'Test1'
testdata$stg[testdata$stage == '4'] <- 'Test2'

# First of all we need the descriptive statistics (we need
# to load up the psych package for this):

library(psych)

# Descriptives are generated using the following function:
descrip <- describeBy(testdata$resp,list(testdata$stim,
                                         testdata$stg),mat=TRUE)
print(descrip)
write.csv(descrip,'analysis/Exp2Descriptives.csv',row.names=FALSE)

# This next function produces graphs. A bar chart of the mean
# ratings for each stimuli is a good way of visualising the
# data. First graph has a title, while the second is a direct
# replication with no title:

# (n.b. I have not quite worked out the correct code for displaying
# data from both test stages in a single graph)

library(graphics)

barplot(descrip$mean, names = descrip$group1, legend = descrip$group2,
        xlab = "Stimulus", ylab = "Mean Rating (0-10)",
        main = 'Experiment 3 Results', ylim = c(0, 10))

barplot(descrip$mean, names = descrip$group1, legend = descrip$group2,
        xlab = "Stimulus",ylab = "Mean Rating (0-10)", 
        ylim = c(0, 10))

#####################################################################

# Now it's time to perform the analysis our data set.

# We will need to aggregate the data to do this:
aggdata <- aggregate(testdata$resp,list(testdata$partic, testdata$stg,
                                        testdata$stim),mean)
colnames(aggdata) <- c('partic','stage','stim','resp')

# Also we will need to split out the two test stages:
aggdata1 <- subset(aggdata,stage == 'Test1')
aggdata2 <- subset(aggdata,stage == 'Test2')

# This loads up the ez package within R, which we need for the
# analysis (needs to be downloaded if you don't have it):

library(ez)

# First step is to see whether there is a redundancy effect
# at both test stage 1 and test stage 2.

# 1. Testing X and Y for the Redundancy Effect (stage 1):
RE.aggdata1 <- subset(aggdata1,stim == 'X' | stim == 'Y')
t.test(resp ~ stim, data = RE.aggdata1,paired=TRUE)

# 2. Testing X and Y for the Redundancy Effect (stage 2):
RE.aggdata2 <- subset(aggdata2,stim == 'X' | stim == 'Y')
t.test(resp ~ stim, data = RE.aggdata2,paired=TRUE)

# There is a significant redundancy effect at both stages.
# Next is to see whether stage1 X is significantly different
# from stage2 X, and whether stage1 Y is significantly 
# different from stage2 Y.

# 3. Testing for significant difference between stage 1
# and stage 2 ratings for X:
X.aggdata <- subset(aggdata,stim == 'X')
t.test(resp ~ stage, data = X.aggdata, paired=TRUE)

# 4. Testing for significant difference between stage 1
# and stage 2 ratings for Y:
Y.aggdata <- subset(aggdata,stim == 'Y')
t.test(resp ~ stage, data = Y.aggdata, paired=TRUE)

# The results show that the scores are different at each
# stage. The key question now is whether there is a significant
# difference between the differences (i.e. an interaction
# effect) - there isn't:

XY.aggdata <- subset(aggdata,stim == 'Y' | stim == 'X')

output <- ezANOVA(data = XY.aggdata
                   , dv = resp
                   , wid = partic
                   , within = c('stim','stage')
                   , type = 3)

print(output)

#####################################################################

#solongandthanksforallthefish
