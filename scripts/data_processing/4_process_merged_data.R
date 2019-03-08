# read in merged data
eof <- read.csv(file.path('data_cached', paste0(site, '_merged_dat.csv')), header = TRUE, stringsAsFactors = FALSE,
                colClasses = c(storm_start = 'POSIXct'))

# create a water equivalent var that sums rain and 
# water equivalent of snow, estimated as 1:10
eof$weq <- ifelse(eof$snwd_diff >= 0, eof$rain, eof$rain + (abs(eof$snwd_diff)/10))

# set responses and predictors

# start with all predictors - 32 in total
# have grouped them from each processing step (e.g., rain vars, then discharge vars), 
# I think this will standardize predictors since each step has standard output.
# rain, weather, field predictors, discharge, frozen
predictors <- names(select(eof, weq, duration:ARFdays14, sin_sdate:tmin, days_since_planting:days_since_disturbance, 
                           ant_dis_1day_max:ant_dis_14day_max, frozen))

# drop site-specific predictors that shouldn't be in mod
if (!any(is.na(predictors_drop))) {
  predictors <- predictors[-which(predictors %in% predictors_drop)]
}

                    
# set responses and set cleaner name to plot for responses

conc_names <- wq_env$concvars

load_names <- wq_env$loadvars

responses <- c(conc_names, load_names, other_responses)
responses <- responses[!is.na(responses)]

########################
# turn frozen into logical column
# create a period variable that is before/after

site_dat <- eof %>%
  mutate(frozen = as.logical(eof$frozen)) %>%
  mutate(period = ifelse(storm_start >= bmp_date, 'after', 'before'))

temp_filename <- file.path('data_cached', paste0(site, '_mod_dat.csv'))
write.csv(site_dat, temp_filename, row.names = F)

############
# save predictors and responses for later use

save(predictors, responses, file = 'data_cached/modvars.Rdata')

if (nrow(site_dat) > 1) {
  message(paste("The data have now been merged and processed. Please see", temp_filename, "to ensure processing."))
} else {
  stop("Something went wrong during processing of merged data. To debug, see code in 'scripts/data_processing/4_process_merged_data.R'.")
}

