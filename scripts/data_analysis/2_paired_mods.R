# loop through each response var
# uses vars set up in 1_mds_paired.R
dat <- wq
for (i in 1:length(trt_vars)) {
  
  temp_trt <-  trt_vars[i]
  temp_con <- gsub(test_site, control_site, temp_trt)
  
  # log transform responses
  dat[, temp_trt] <- log10(dat[,temp_trt])
  dat[,temp_con] <- log10(dat[,temp_con])
  
  # create linear model
  mod_temp = lm(dat[,temp_trt] ~ dat[,temp_con]*dat[,period_num], data=dat)
  
  
}