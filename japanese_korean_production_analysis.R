## Bodo Winter
## September 14, 2016
## Japanese & Korean production analysis

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load in libraries:

library(dplyr)	# for data processing
library(lme4)	# for mixed models
library(party)	# for random forests
options(stringsAsFactors = F)
options(dplyr.width = Inf)

## Load in data:

setwd('/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/teeniematlock/Desktop/research/politeness/japanese_perception/analysis/')
prod <- read.csv('japanese_korean_production.csv') %>% tbl_df()

## Create a 'unique speaker variable' by pasting subject column with language column:

prod$SubID <- paste(prod$speaker, prod$language, sep = ':')

## Create a 'unique item variable' by pasting item column with langtask column:

prod$Item <- paste(prod$scenario, prod$langtask, sep = ':')

## Create Korean and Japanese subsets:

kor <- filter(prod, language == 'Korean')
jap <- filter(prod, language == 'Japanese')



##------------------------------------------------------------------
## Mixed model analysis:
##------------------------------------------------------------------

## Define variables to perform mixed model analysis on:

variables <- c('dur', 'f0mnhz', 'f0sdhz',
	'inmn', 'inrange', 'jitloc',
	'shimloc', 'h1mh2mn', 'mnHNR')

## Create empty tables to be filled with relevant statistics:
## (gender main effect, attitude main effect and gender * attitude interaction)
## (for each one we store t-value and slope)
## (... later to be replaced with likelihood ratio test results)
## (can amend this to include SEs for publication)

kor.res <- data.frame(variables,
	gen.t = 0, gen.slope = 0,
	pol.t = 0, pol.slope = 0,
	int.t = 0, int.slope = 0)
jap.res <- kor.res	# copy for Japanese

## Get formula snippets that will be used later:

main_effects <- 'gender + condition2'
int_effects <- paste(main_effects, 'gender:condition2', sep = ' + ')
rand_effects <- '(1 + condition2|SubID) + (1|Item)'
main_effects <- paste(main_effects, rand_effects, sep = ' + ')
int_effects <- paste(int_effects, rand_effects, sep = ' + ')
# explanation: attitude is within-subjects, so there's a random effect for that
# and gender is within-items so there's a random effect for that

## Loop through variables and perform the following analyses:

for (i in 1:length(variables)) {
	## Select variable:
	
	this_var <- variables[i]
	
	## Create formulas:
	
	var_form <- paste0(this_var, ' ~ ')
	main_form <- paste0(var_form, main_effects)
	int_form <- paste0(var_form, int_effects)
	
	## Run models for Korean:
	
	xmdl.main <- lmer(as.formula(main_form),
		data = kor, REML = F)
	xmdl.main <- summary(xmdl.main)$coefficients
	
	xmdl.int <- lmer(as.formula(int_form),
		data = kor, REML = F)
	xmdl.int <- summary(xmdl.int)$coefficients	
	
	## Store relevant data:
	
	kor.res[i, ]$gen.t <- xmdl.main['genderM', 't value']
	kor.res[i, ]$gen.slope <- xmdl.main['genderM', 'Estimate']

	kor.res[i, ]$pol.t <- xmdl.main['condition2polite', 't value']
	kor.res[i, ]$pol.slope <- xmdl.main['condition2polite', 'Estimate']
	
	kor.res[i, ]$int.t <- xmdl.int['genderM:condition2polite', 't value']
	kor.res[i, ]$int.slope <- xmdl.int['genderM:condition2polite', 'Estimate']

	## Run models for japean:
	
	xmdl.main <- lmer(as.formula(main_form),
		data = jap, REML = F)
	xmdl.main <- summary(xmdl.main)$coefficients
	
	xmdl.int <- lmer(as.formula(int_form),
		data = jap, REML = F)
	xmdl.int <- summary(xmdl.int)$coefficients	
	
	## Store relevant data:
	
	jap.res[i, ]$gen.t <- xmdl.main['genderM', 't value']
	jap.res[i, ]$gen.slope <- xmdl.main['genderM', 'Estimate']

	jap.res[i, ]$pol.t <- xmdl.main['condition2polite', 't value']
	jap.res[i, ]$pol.slope <- xmdl.main['condition2polite', 'Estimate']
	
	jap.res[i, ]$int.t <- xmdl.int['genderM:condition2polite', 't value']
	jap.res[i, ]$int.slope <- xmdl.int['genderM:condition2polite', 'Estimate']
	}
# some convergence issues that we'll ignore for now

## Store results:

write.table(kor.res, file = 'kor_production_LMER_results.csv',
	sep = ',', row.names = F)
write.table(jap.res, file = 'jap_production_LMER_results.csv',
	sep = ',', row.names = F)



##------------------------------------------------------------------
## Random forests (for now without conditional = T):
##------------------------------------------------------------------

## Create uber formulas:

all_vars <- prod %>% select(dur:shimapq3vr) %>% colnames
uber_form <- paste(all_vars, collapse = ' + ')
uber_form <- paste('condition2', uber_form, sep = ' ~ ')

## Set random forest parameters:

my_settings <- cforest_unbiased(ntree = 2000,
	mtry = round(sqrt(length(all_vars))))

## For this analysis the response needs to be a factor:

kor <- kor %>% mutate(condition2 = as.factor(condition2))
jap <- jap %>% mutate(condition2 = as.factor(condition2))

## Run forest on Korean and Japanese data separately:

kor.forest <- cforest(as.formula(uber_form),
	data = kor, controls = my_settings)
jap.forest <- cforest(as.formula(uber_form),
	data = jap, controls = my_settings)

## Get variable importances:

kor.varimp <- varimp(kor.forest, conditional = F)
jap.varimp <- varimp(jap.forest, conditional = F)

## Plot this:

library(lattice)
dotplot(sort(kor.varimp))
dotplot(sort(jap.varimp))

## Get predictive accuracies:

kor.pred <- predict(kor.forest)
jap.pred <- predict(jap.forest)

kor.tab <- table(kor$condition2, kor.pred)
jap.tab <- table(jap$condition2, jap.pred)
sum(diag(kor.tab)) / sum(kor.tab)	# 93%
sum(diag(jap.tab)) / sum(jap.tab)	# 88%




