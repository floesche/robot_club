# DAU: PLY132
# Blocked cues & uncorrelated (mk2)
# Analysis of experimental data.

# Import the data we want to analyse:
testdata <- read.csv("Exp13testdata.csv")

# Install psych package (if required):
#install.packages("psych")

# Load psych package:
library(psych)

# Descriptives are generated using this function from psych:
# (2nd example shows how to list multiple grouping variables if required)
descrip <- describeBy(testdata$resp, group = testdata$stim, mat = TRUE)
descrip2 <- describeBy(testdata$resp, list(testdata$stim), mat = TRUE)
write.csv(descrip, 'Exp13Descriptives.csv',row.names = FALSE)

# n.b. typing ? in console before a functions brings up help & info:
?describeBy

# Load graphics package:
library(graphics)

# Produce bar chart:
barplot(descrip$mean, names = descrip$group1,
        xlab = "Stimulus", ylab = "Mean Rating (0-10)", 
        ylim = c(0, 10))

# How to create scatterplot (basic example):
x <- c(1, 2, 3, 4, 5)
y <- c(2, 7, 4, 10, 9)
plot(x, y)

# For futher info:
?plot

# Alternatively see ggplot2 package for elegant graphs with flexible options:
install.packages("ggplot2")
library(ggplot2)
?ggplot2

# Aggregate data for analysis:
aggdata <- aggregate(testdata$resp, list(testdata$partic, testdata$stim), mean)

# Fix column names for aggdata:
colnames(aggdata) <- c('partic','stim','resp')

# Example of sub-setting data and performing t-test:
sub.agg <- subset(aggdata, stim == 'WC' | stim == 'XF')
t.test(resp ~ stim, data = sub.agg, paired = TRUE)

# Install ez package (if required):
#install.packages("ez")

# Load ez package (for ANOVA):
library(ez)

# Perform ANOVA (for sig diff btwn mean scores for stimuli):
output <- ezANOVA(data = aggdata
                  , dv = resp
                  , wid = partic
                  , within = stim
                  , type = 3)
# Results:
print(output)

# n.b for post-hoc tests you can use t-tests. Remember to make the Bonferroni
# correction to account for the number of t-tests you do.
?t.test

# Another way of doing ANOVA (aov function):
library(stats)
output2 <- aov(resp ~ stim, data = aggdata)
summary(output2)

# Post-hoc test (example purposes only: I would not analyse the data this way!):
TukeyHSD(output2)


# Some other useful functions...

# This creates a table:
table(testdata$resp)

# Create a copy of object with new name (for example only)
gubbin <- testdata$resp

# If you have two objects you want to compare this provides true v false:
testdata$resp == gubbin

# And this puts the above into a handy table:
table(testdata$resp == gubbin)

# Example of a binomial test (weird coin that falls on 31 tails v 9 heads):
binom.test(c(31, 9), p = 0.5)

# A quick Bayes taster:
#install.packages("BayesFactor")

# Load package:
library(BayesFactor)

# Read info:
?BayesFactor
BFManual()

# Do Bayesian t-test (for example):
ttestBF(x = aggdata$resp[aggdata$stim == "X"], 
        y = aggdata$resp[aggdata$stim == "W"], paired = TRUE)

# n.b. we are discussing a proper session on Bayesian analysis next term...

# Tidy environment:
rm(testdata, descrip2, gubbin, x, y)

# The end :-)
