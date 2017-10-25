# DAU: PLY132
# Blocked cues & uncorrelated (mk2)
# Harvesting experimental data.

library(tidyverse)

# Make list of raw data files (i.e. psychopy outputs for each participant):
rawfiles <- list.files(path = 'E13_rawdata', full.names = TRUE)

# Collate individual participant data (indivdata) into a single dataset of all 
# experimental data (alldata):
alldata <- NULL
for (file in rawfiles) {
  indivdata <- read_csv(file)
  alldata <- alldata %>%
    bind_rows(indivdata)
}

# Tidy up the info in alldata to make it suitable for analysis in R (rdata). 
# Select columns we need and address column order and fix column names:
rdata <- alldata %>%
  select(
    'date', 
    'partic' = 'participant',
    'block1' = 'train_trials.thisRepN',
    'block2' = 'train_trials2.thisRepN',
    'block3' = 'test_trials.thisRepN',
    'trial1' = 'train_trials.thisN',
    'trial2' = 'train_trials2.thisN',
    'trial3' = 'test_trials.thisN',
    'stim1',
    'stim2',
    'fruit1' = 'stim.A',
    'fruit2' = 'stim.B',
    'resp1'  = 'key_ans.keys',
    'resp2'  = 'key_answ.keys',
    'resp3'  = 'rating.response',
    'rt1'    = 'key_ans.rt',
    'rt2'    = 'key_answ.rt',
    'rt3'    = 'rating.rt',
    'outcome'
  )

# This adds a value of one to each block and trial number, to overcome the fact 
# that Psychopy starts it's numbering at zero rather than one:
rdata <- rdata %>%
  mutate(block1 = block1 + 1, 
         block2 = block2 + 1,
         block3 = block3 + 1,
         trial1 = trial1 + 1,
         trial2 = trial2 + 1,
         trial3 = trial3 + 1)

# This replaces all the block and trial columns with an NA in them, 
# with a zero instead:
rdata <- rdata %>%
  replace_na(list(
    block1 = 0,
    block2 = 0, 
    block3 = 0,
    trial1 = 0,
    trial2 = 0,
    trial3 = 0
  ))

# Combine block & trial number columns into a single block column and 
# a single trial column:
rdata <- rdata %>%
  mutate(block = block1 + block2 + block3,
         trial = trial1 + trial2 + trial3)


# Add in an extra column detailing which stage the of experiment data is from. 
# This is done by by reading which cells in each respective response time (RT) 
# column DO NOT have an N/A in them:
rdata <- rdata %>%
  mutate(stage = case_when(
    !is.na(rt1) ~ 1,
    !is.na(rt2) ~ 2,
    !is.na(rt3) ~ 3
  ))


# Convert all the response time (RT) cells containing an NA into zeros, for ease 
# of data management. The final line of code in this section combines all of the 
# RT data into a single column:
rdata <- rdata %>%
  replace_na(list(
    rt1 = 0,
    rt2 = 0,
    rt3 = 0)) %>%
  mutate(rt = rt1 + rt2 + rt3)

# Remove all the unneccesary rows. Done by identifying every cell in the stage 
# column which does not contain an N/A and retaining only these rows:
rdata <- rdata %>%
  filter(!is.na(stage))


# This combines the response data (resp) from each stage of
# the experiment into a single response column:
rdata <- rdata %>%
  mutate(resp = case_when(
    stage == 1 ~ resp1,
    stage == 2 ~ resp2,
    stage == 3 ~ as.character(resp3)
  ))


# In the PsychoPy script, different letters were used to represent the cues (than 
# the ones we need), because of how psychopy reads the syntax. This will fix that 
# and also merge the info into a single column.

rdata <- rdata %>%
  replace_na(list(
    stim1 = ' ',
    stim2 = ' '
  )) %>%
  mutate(stim = case_when(
    
    # Starting with the first single cues:
    stim1 == 'A' & stim2 == ' ' ~ 'A',
    stim1 == ' ' & stim2 == 'A' ~ 'A',
    
    stim1 == 'B' & stim2 == ' ' ~ 'B',
    stim1 == ' ' & stim2 == 'B' ~ 'B',
    
    stim1 == 'C' & stim2 == ' ' ~ 'C',
    stim1 == ' ' & stim2 == 'C' ~ 'C',
    
    stim1 == 'F' & stim2 == ' ' ~ 'D',
    stim1 == ' ' & stim2 == 'F' ~ 'D',
    
    stim1 == 'G' & stim2 == ' ' ~ 'E',
    stim1 == ' ' & stim2 == 'G' ~ 'E',
    
    stim1 == 'H' & stim2 == ' ' ~ 'F',
    stim1 == ' ' & stim2 == 'H' ~ 'F',
    
    stim1 == 'D' & stim2 == ' ' ~ 'X',
    stim1 == ' ' & stim2 == 'D' ~ 'X',
    
    stim1 == 'E' & stim2 == ' ' ~ 'Y',
    stim1 == ' ' & stim2 == 'E' ~ 'Y',
    
    stim1 == 'I' & stim2 == ' ' ~ 'W',
    stim1 == ' ' & stim2 == 'I' ~ 'W',
    
    stim1 == 'J' & stim2 == ' ' ~ 'Z',
    stim1 == ' ' & stim2 == 'J' ~ 'Z',
    
    # And now moving onto the compounds:    
    stim1 == 'A' & stim2 == 'D' ~ 'AX',
    stim1 == 'D' & stim2 == 'A' ~ 'AX',
    
    stim1 == 'B' & stim2 == 'E' ~ 'BY',
    stim1 == 'E' & stim2 == 'B' ~ 'BY',
    
    stim1 == 'C' & stim2 == 'E' ~ 'CY',
    stim1 == 'E' & stim2 == 'C' ~ 'CY',
    
    stim1 == 'F' & stim2 == 'I' ~ 'DW',
    stim1 == 'I' & stim2 == 'F' ~ 'DW',
    
    stim1 == 'G' & stim2 == 'J' ~ 'EZ',
    stim1 == 'J' & stim2 == 'G' ~ 'EZ',
    
    stim1 == 'H' & stim2 == 'J' ~ 'FZ',
    stim1 == 'J' & stim2 == 'H' ~ 'FZ',
    
    stim1 == 'D' & stim2 == 'C' ~ 'XC',
    stim1 == 'C' & stim2 == 'D' ~ 'XC',
    
    stim1 == 'I' & stim2 == 'C' ~ 'WC',
    stim1 == 'C' & stim2 == 'I' ~ 'WC',
    
    stim1 == 'D' & stim2 == 'H' ~ 'XF',
    stim1 == 'H' & stim2 == 'D' ~ 'XF'
  )) 


# Select which columns are required for data table (and column ordering):
rdata %>% select(
  'partic','stage','block','trial','stim','resp','rt',
  'fruit1','fruit2','outcome','date'
)


#####################################################################

# Output test data to a csv.

# Remove the data from training so that we just have the test data:
rtestdata <- rdata %>%
  filter(stage == 3)

# This function creates the exported csv file:
write_csv(rtestdata, 'Exp13testdata.csv')

# crossingat430
