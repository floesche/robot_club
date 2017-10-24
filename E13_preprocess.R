# DAU: PLY132
# Blocked cues & uncorrelated (mk2)
# Harvesting experimental data.

# Make list of raw data files (i.e. psychopy outputs for each participant):
rawfiles <- list.files(path = 'E13_rawdata', full.names = TRUE)

# Collate individual participant data (indivdata) into a single dataset of all 
# experimental data (alldata):
alldata <- NULL
for(file in rawfiles) {
  indivdata <- read.csv(file, stringsAsFactors = FALSE)
  alldata <- rbind(alldata, indivdata)
}

# Tidy up the info in alldata to make it suitable for analysis in R (rdata). 
# Select columns we need and address column order:
rdata <- alldata[ ,c('date','participant','train_trials.thisRepN',
                    'train_trials2.thisRepN','test_trials.thisRepN',
                    'train_trials.thisN','train_trials2.thisN',
                    'test_trials.thisN','stim1','stim2','stim.A','stim.B',
                    'key_ans.keys','key_answ.keys','rating.response','key_ans.rt',
                    'key_answ.rt','rating.rt','outcome')]

# Fix column names:
colnames(rdata) <- c('date','partic','block1','block2','block3','trial1','trial2',
                     'trial3','stim1','stim2','fruit1','fruit2','resp1','resp2',
                     'resp3','rt1','rt2','rt3','outcome')

# This adds a value of one to each block and trial number, to overcome the fact 
# that Psychopy starts it's numbering at zero rather than one:
rdata$block1 <- rdata$block1 + 1
rdata$block2 <- rdata$block2 + 1
rdata$block3 <- rdata$block3 + 1
rdata$trial1 <- rdata$trial1 + 1
rdata$trial2 <- rdata$trial2 + 1
rdata$trial3 <- rdata$trial3 + 1

# This replaces all the block and trial columns with an NA in them, 
# with a zero instead:
rdata$block1[is.na(rdata$block1)] <- 0
rdata$block2[is.na(rdata$block2)] <- 0
rdata$block3[is.na(rdata$block3)] <- 0
rdata$trial1[is.na(rdata$trial1)] <- 0
rdata$trial2[is.na(rdata$trial2)] <- 0
rdata$trial3[is.na(rdata$trial3)] <- 0

# Combine block & trial number columns into a single block column and 
# a single trial column:
rdata$block <- rdata$block1 + rdata$block2 + rdata$block3
rdata$trial <- rdata$trial1 + rdata$trial2 + rdata$trial3

# Add in an extra column detailing which stage the of experiment data is from. 
# This is done by by reading which cells in each respective response time (RT) 
# column DO NOT have an N/A in them:
rdata$stage <- NULL
rdata$stage[!is.na(rdata$rt1)] <- 1
rdata$stage[!is.na(rdata$rt2)] <- 2
rdata$stage[!is.na(rdata$rt3)] <- 3

# Convert all the response time (RT) cells containing an NA into zeros, for ease 
# of data management. The final line of code in this section combines all of the 
# RT data into a single column:
rdata$rt1[is.na(rdata$rt1)] <- 0
rdata$rt2[is.na(rdata$rt2)] <- 0
rdata$rt3[is.na(rdata$rt3)] <- 0
rdata$rt <- rdata$rt1 + rdata$rt2 + rdata$rt3

# Remove all the unneccesary rows. Done by identifying every cell in the stage 
# column which does not contain an N/A and retaining only these rows:
rdata <- rdata[!is.na(rdata$stage), ]

# This combines the response data (resp) from each stage of
# the experiment into a single response column:
rdata$resp <- NULL
rdata$resp[rdata$stage == 1] <- rdata$resp1[rdata$stage == 1]
rdata$resp[rdata$stage == 2] <- rdata$resp2[rdata$stage == 2]
rdata$resp[rdata$stage == 3] <- rdata$resp3[rdata$stage == 3]

# In the PsychoPy script, different letters were used to represent the cues (than 
# the ones we need), because of how psychopy reads the syntax. This will fix that 
# and also merge the info into a single column.

# Starting with the first single cues:
rdata$stim <- NULL

rdata$stim[rdata$stim1 == 'A' & rdata$stim2 == ' '] <- 'A'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'A'] <- 'A'

rdata$stim[rdata$stim1 == 'B' & rdata$stim2 == ' '] <- 'B'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'B'] <- 'B'

rdata$stim[rdata$stim1 == 'C' & rdata$stim2 == ' '] <- 'C'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'C'] <- 'C'

rdata$stim[rdata$stim1 == 'F' & rdata$stim2 == ' '] <- 'D'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'F'] <- 'D'

rdata$stim[rdata$stim1 == 'G' & rdata$stim2 == ' '] <- 'E'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'G'] <- 'E'

rdata$stim[rdata$stim1 == 'H' & rdata$stim2 == ' '] <- 'F'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'H'] <- 'F'

rdata$stim[rdata$stim1 == 'D' & rdata$stim2 == ' '] <- 'X'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'D'] <- 'X'

rdata$stim[rdata$stim1 == 'E' & rdata$stim2 == ' '] <- 'Y'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'E'] <- 'Y'

rdata$stim[rdata$stim1 == 'I' & rdata$stim2 == ' '] <- 'W'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'I'] <- 'W'

rdata$stim[rdata$stim1 == 'J' & rdata$stim2 == ' '] <- 'Z'
rdata$stim[rdata$stim1 == ' ' & rdata$stim2 == 'J'] <- 'Z'

# And now moving onto the compounds:
rdata$stim[rdata$stim1 == 'A' & rdata$stim2 == 'D'] <- 'AX'
rdata$stim[rdata$stim1 == 'D' & rdata$stim2 == 'A'] <- 'AX'

rdata$stim[rdata$stim1 == 'B' & rdata$stim2 == 'E'] <- 'BY'
rdata$stim[rdata$stim1 == 'E' & rdata$stim2 == 'B'] <- 'BY'

rdata$stim[rdata$stim1 == 'C' & rdata$stim2 == 'E'] <- 'CY'
rdata$stim[rdata$stim1 == 'E' & rdata$stim2 == 'C'] <- 'CY'

rdata$stim[rdata$stim1 == 'F' & rdata$stim2 == 'I'] <- 'DW'
rdata$stim[rdata$stim1 == 'I' & rdata$stim2 == 'F'] <- 'DW'

rdata$stim[rdata$stim1 == 'G' & rdata$stim2 == 'J'] <- 'EZ'
rdata$stim[rdata$stim1 == 'J' & rdata$stim2 == 'G'] <- 'EZ'

rdata$stim[rdata$stim1 == 'H' & rdata$stim2 == 'J'] <- 'FZ'
rdata$stim[rdata$stim1 == 'J' & rdata$stim2 == 'H'] <- 'FZ'

rdata$stim[rdata$stim1 == 'D' & rdata$stim2 == 'C'] <- 'XC'
rdata$stim[rdata$stim1 == 'C' & rdata$stim2 == 'D'] <- 'XC'

rdata$stim[rdata$stim1 == 'I' & rdata$stim2 == 'C'] <- 'WC'
rdata$stim[rdata$stim1 == 'C' & rdata$stim2 == 'I'] <- 'WC'

rdata$stim[rdata$stim1 == 'D' & rdata$stim2 == 'H'] <- 'XF'
rdata$stim[rdata$stim1 == 'H' & rdata$stim2 == 'D'] <- 'XF'

# Select which columns are required for data table (and column ordering):
rdata <- rdata[,c('partic','stage','block','trial','stim','resp','rt',
                  'fruit1','fruit2','outcome','date')]

#####################################################################

# Output test data to a csv.

# Remove the data from training so that we just have the test data:
rtestdata <- rdata[rdata$stage == 3, ]

# This function creates the exported csv file:
write.csv(rtestdata,'Exp13testdata.csv', row.names = FALSE)

# crossingat430
