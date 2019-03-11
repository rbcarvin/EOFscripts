# calculate days since field action variables
# e.g., days since planing corn, alfalfa
# days since last fertilizer/manure application
# use the field activity sheet and storm start dates to calculate
storm_filename <- file.path('data_cached', paste0(site, '_prepped_WQbystorm.csv'))
storms <- read.csv(storm_filename, stringsAsFactors = FALSE,
                   colClasses = c(storm_start = 'POSIXct', storm_end = 'POSIXct'))

timeline_filename <- file.path('data_raw', activity_file)
timeline <- read.csv(timeline_filename,
                     stringsAsFactors = FALSE, strip.white = TRUE)
timeline$date <- as.Date(timeline$date, format = date_format)

# days since planting
# for corn, after cutting, value assumes zero
# for alfalfa, resets after cutting but goes up until next cutting

field_events <- arrange(storms, storm_start) %>%
  select(unique_storm_number, storm_start)

# calculate days since manure/fertilizer
# calculate days since planting, set back to zero after
# cutting
field_events <- field_events %>%
  mutate(days_since_planting = NA) %>%
  mutate(days_since_fertilizer = NA) %>%
  mutate(days_since_cultivation = NA) %>%
  mutate(days_since_disturbance = NA)

for (i in 1:nrow(field_events)) {
  
  temp_date <- as.Date(format(field_events$storm_start[i], "%Y-%m-%d"))
  
  #fert/manure
  man_fert_filter <- grep(paste0(nut_additions_keywords, collapse = '|'), timeline$activity_group, ignore.case = T)
  temp_timeline <- timeline[man_fert_filter, ]
  temp_timeline <- filter(temp_timeline, date <= temp_date)
  
  if (nrow(temp_timeline) == 0) {
    field_events$days_since_fertilizer[i] <- NA
  } else {
    fert_diff <- temp_date - temp_timeline$date
    field_events$days_since_fertilizer[i] <- as.numeric(min(fert_diff))
  }
  
  
  # cultivation
  cultivation_filter <- grep(paste0(cultivation_keywords, collapse = '|'), timeline$activity_group, ignore.case = T)
  temp_timeline <- timeline[cultivation_filter, ]
  temp_cultivation <- filter(temp_timeline, date <= temp_date)
  
  if (nrow(temp_cultivation) == 0) {
    field_events$days_since_cultivation[i] <- NA
  } else {
    cultivation_diff <- temp_date - temp_cultivation$date
    field_events$days_since_cultivation[i] <- as.numeric(min(cultivation_diff))
  }
  
  
  
  # disturbance
  # basically count days since any field disturbance 
  
  date_last_disturb <- filter(timeline, date <= temp_date) %>% slice(which.max(date)) %>%
    pull(date)
  
  if (length(date_last_disturb) == 0) {
    field_events$days_since_disturbance[i] <- NA
  } else {
    days_since_disturb <- as.numeric(temp_date - date_last_disturb)
    field_events$days_since_disturbance[i] <- ifelse(days_since_disturb > 10, 10, days_since_disturb)
  }
  
 
  #plantings, cutting, & harvest to estimate plant cover
  if (all(!is.na(cutting_keywords))) {
    temp_timeline <- filter(timeline, date <= temp_date)
    
    if (nrow(temp_timeline)==0) {
      field_events$days_since_planting[i] <- NA
    } else {
    planting_filter <- grep(paste0(planting_keywords, collapse = '|'), temp_timeline$activity_group, ignore.case = T)
    harvest_filter <- grep(paste0(harvest_keywords, collapse = '|'), temp_timeline$activity_group, ignore.case = T)
    cutting_filter <- grep(paste0(cutting_keywords, collapse = '|'), temp_timeline$activity_group, ignore.case = T)
    
    temp_plantings <- temp_timeline[planting_filter, ]
    temp_harvest <- temp_timeline[harvest_filter, ]
    temp_cutting <- temp_timeline[cutting_filter, ]
    
    temp_all <- bind_rows(temp_plantings, temp_harvest, temp_cutting) %>%
      arrange(date)
    
    # now, filter out everything since the last harvest
    last_harvest <- filter(temp_all, activity_group %in% c(harvest_keywords, cutting_keywords)) %>%
      slice(which.max(date)) %>% pull(date)
    
    if (length(last_harvest) == 0) {
      if (nrow(temp_all) == 1) {
        temp_diff <- as.numeric(temp_date - temp_all$date)
        field_events$days_since_planting[i] <- ifelse(temp_diff > 30, 30, temp_diff)
        
      } else if (any(temp_all$activity_group[which.max(temp_all$date)] %in% cutting_keywords)) {
        last_date <- max(temp_all$date)
        temp_diff <- as.numeric(temp_date - last_date)
        field_events$days_since_planting[i] <- ifelse(temp_diff > 30, 30, temp_diff)
      } else {
        last_date <- max(temp_all$date)
        temp_diff <- as.numeric(temp_date - last_date)
        
        # check if there was a planting date before this. Do not want to reset if planting
        # on top of a cover crop, for example.
        
        if (temp_diff < 30 & any(temp_all$activity_group[nrow(temp_all)-1] %in% planting_keywords)) {
          temp_diff <- as.numeric(temp_date - temp_all$date[nrow(temp_all)-1])
        }
        
        field_events$days_since_planting[i] <- ifelse(temp_diff > 30, 30, temp_diff)
      }
    } else {
    
    temp_all <- filter(temp_all, date >= last_harvest)
    
    # if harvest was the last activity, then set to 0
    
    if (any(temp_all$activity_group[which.max(temp_all$date)] %in% harvest_keywords)) {
      field_events$days_since_planting[i] <- 0
    
      } else if (any(temp_all$activity_group[which.max(temp_all$date)] %in% cutting_keywords)) {
      last_date <- max(temp_all$date)
      temp_diff <- as.numeric(temp_date - last_date)
      field_events$days_since_planting[i] <- ifelse(temp_diff > 30, 30, temp_diff)
    } else {
      last_date <- max(temp_all$date)
      temp_diff <- as.numeric(temp_date - last_date)
      
      # check if there was a planting date before this. Do not want to reset if planting
      # on top of a cover crop, for example.
      
      if (temp_diff < 30 & any(temp_all$activity_group[nrow(temp_all)-1] %in% planting_keywords)) {
        temp_diff <- as.numeric(temp_date - temp_all$date[nrow(temp_all)-1])
      }
      
      field_events$days_since_planting[i] <- ifelse(temp_diff > 30, 30, temp_diff)
    }
    }
    }
    
    
  }
  # temp_timeline <- filter(timeline, date <= temp_date)
  # planting_filter <- grep(paste0(planting_keywords, collapse = '|'), temp_timeline$activity_group, ignore.case = T)
  # harvest_filter <- grep(paste0(harvest_keywords, collapse = '|'), temp_timeline$activity_group, ignore.case = T)
  # temp_plantings <- temp_timeline[planting_filter, ]
  # temp_harvest <- temp_timeline[harvest_filter, ]
  # 
  # # now decide which planting/harvest date to use. 
  # # e.g., if a cutting happened between now and planting, use days since cutting
  # temp_diff <- c((temp_date - temp_plantings$date), (temp_date - temp_harvest$date))
  # 
  # field_events$days_since_planting[i] <- as.numeric(min(temp_diff))
}

field_events <- select(field_events, -storm_start)

temp_filename <- file.path('data_cached', paste0(site, '_field_predictors.csv'))
write.csv(field_events, temp_filename, row.names = FALSE)

if(nrow(field_events) == nrow(storms) & nrow(field_events) > 0) {
  message(paste("Field events processing is complete. Please see", temp_filename, "to ensure correct processing."))
} else {
  stop("Something went wrong with processing the field events data. To debug, see code in 'scripts/data_processing/2_calc_field_predictors.R'")
}
