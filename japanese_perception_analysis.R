## Bodo Winter
## September 14, 2016
## Japanese & Korean production analysis

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load in libraries:

library(dplyr)		# for data processing
library(lme4)		# for mixed models
library(party)		# for random forests
library(stringr)	# for string processing
options(stringsAsFactors = F)
options(dplyr.width = Inf)

## Load in data:

setwd('/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/teeniematlock/Desktop/research/politeness/japanese_perception/analysis/')
per <- read.csv('japanese_perception_experiment.csv') %>% tbl_df()

## Select subset of relevant columns:

per <- per %>% select(Subject, Age, Decision.RESP.Trial.,
	Decision.RT.Trial., SenarioNumber:SoundFile2)

## Rename columns:

per <- per %>% rename(Choice = Decision.RESP.Trial.,
	RT = Decision.RT.Trial.,
	File1 = SoundFile,
	File2 = SoundFile2,
	Item = SenarioNumber)

## Create trial variable:

per$Trial <- NA
for (i in 1:length(unique(per$Subject))) {
	this_sub <- unique(per$Subject)[i]
	this_length <- nrow(filter(per, Subject == this_sub))
	per[per$Subject == this_sub, ]$Trial <- 1:this_length
	}

## Get rid of those that are NA:

sum(is.na(per$Choice))	# 46 NA's
sum(is.na(per$Choice)) / nrow(per)	# 2.9 % of overall data
per <- per[!is.na(per$Choice), ]

## Get the chosen file:

per$ChosenFile <- c('File1', 'File2')[per$Choice]

## Get the chosen politeness level:

per$ChosenPol <- NA
for (i in 1:nrow(per)) {
	per[i, ]$ChosenPol <- unlist(per[i, per[i, ]$ChosenFile])
	}

## Extract the politeness information out of the file name using regex:

per$ChosenPol <- str_extract(per$ChosenPol, 'pol|cas')

## Create an accuracy variable:

per$ACC <- ifelse(per$ChosenPol == 'pol', 1, 0)


##------------------------------------------------------------------
## Mixed model analysis:
##------------------------------------------------------------------

## For mixed model analysis log transform RTs and center RTs and trial variable:

per <- per %>% mutate(LogRT = log10(RT + 1),
	LogRT_c = LogRT - mean(LogRT),
	Trial_c = Trial - mean(Trial))

## Make 'ChosenPol' column into factor:

per <- per %>% mutate(ChosenPol = as.factor(ChosenPol))

## A mixed model where we look at the intercept:

xmdl <- lmer(ChosenPol ~ 1 + LogRT_c + Trial_c + 
	(1|Subject) + (1|Item), data = per, family = 'binomial', REML = F)
# convergence issues (probably not enough data)

## Investigate results:

summary(xmdl)
# most importantly: intercept significant
# longer responses less likely correct

## Check intercept (whether the predictions make sense):

plogis(summary(xmdl)$coefficients['(Intercept)', 'Estimate'])	# 59%

## Compare to descriptive average:

mean(per$ACC)	# yep

## Compare to descriptive averages across participants:

aggregate(ACC ~ Subject, per, mean)	# not one below 50%




