# read in data that is prepped for analysis
temp.filename <- paste0(site, '_mod_dat.csv')
dat <- read.csv(file.path('data_cached', temp.filename))

# define predictors
load('data_cached/modvars.Rdata')
dat.mod <- dat[,predictors]
dat.mod <- complete.cases(dat.mod)

n.lost <- length(dat.mod[dat.mod == FALSE])
message(paste0(n.lost, ' observations dropped due to missing predictor data.'))

dat.mod <- dat[dat.mod, ]

# get rid of non-frozen-rainless-runoff events
no.rain <- dat.mod[(dat.mod$frozen=="FALSE" & dat.mod$weq<= 0.2),]
message(paste0(nrow(no.rain), ' observations dropped due to less than 0.2 weq during non-frozen events'))
write.csv(no.rain, file.path('data_cached', paste0(site, '_dropped_noRain.csv')))

dat.mod <- dat.mod[!(dat.mod$frozen=="FALSE" & dat.mod$weq<= 0.2),]

# get rid of highly correlated variables
which.frozen <- which(predictors %in% 'frozen')
predictors.cor <- cor(dat[,predictors[-which.frozen]], use = 'complete.obs') # drop var "crop" from correlation since it's a categorical var
names.cor <- row.names(predictors.cor)
drop.predictors <- caret::findCorrelation(predictors.cor, cutoff = 0.95, verbose = FALSE, exact = TRUE)

predictors.keep <- c(names.cor[-drop.predictors], 'frozen')

# log transform response vars
dat.mod[,responses] <- log10(dat.mod[,responses])
sums <- colSums(dat.mod[,responses])
if(any(is.infinite(sums))) {
  stop('Zeros in the response variables caused values to be infinite when log transformed. Please see code in scripts/data_analysis/0_before_after_datmod_prep.R to debug.', call. = F)
}

#for now, drop Inf values created by logging - TO DO THIS COMMENT OUT LINES 26, 27 AND UNCOMMENT THE FOLLOWING
dat.mod <- dat.mod[!is.infinite(rowSums(dat.mod[,responses])),]
