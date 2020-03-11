# Post_run

# Send results to a results folder within the site folder on the P: drive

# build the results path
results_path <- file.path('P:/0301/field_analysis/results',nick)

nick <- nick.1
# create folders for the most recent run
Rundate <- format(Sys.time(),"%Y-%m-%d-%H%M")
#Rundate <- "2019-10"
dir.create(file.path(results_path,Rundate))
dir.create(file.path(results_path,Rundate,'figures'))
dir.create(file.path(results_path,Rundate,'figures','diagnostic'))
dir.create(file.path(results_path,Rundate,'tables'))
dir.create(file.path(results_path,Rundate,'cache'))
dir.create(file.path(results_path,Rundate,'raw'))

# copy site-specific results files to the site 'results' folder

# figures
# tables (useful .csv table(s))
# cache (less useful)
# raw (to retrace which data input was used)

run_folder <- file.path('D:/EOF/EOFscripts')
#run_folder <- file.path('H:/Projects/GLRI EdgeOfField','OH-FM1')

# figures
copy.figures_1 <- list.files(file.path(run_folder, 'figures','diagnostic'), pattern = nick, full.names=FALSE, recursive = TRUE)
file.copy(from = file.path(run_folder,'figures','diagnostic',copy.figures_1), to = file.path(results_path,Rundate,'figures','diagnostic'))
copy.figures_2 <- list.files(file.path(run_folder, 'figures'), pattern = c(nick,'pdf|png'), full.names=FALSE, recursive = FALSE)
file.copy(from = file.path(run_folder,'figures',copy.figures_2), to = file.path(results_path,Rundate,'figures'))

# tables
copy.mod <- list.files(file.path(run_folder, 'data_cached'), pattern = 'mod_dat|mdc|percent|residual', full.names=FALSE, recursive = TRUE)
copy.mod <- grep(pattern = nick, x=copy.mod, value = TRUE)
file.copy(from = file.path(run_folder,'data_cached',copy.mod), to = file.path(results_path,Rundate,'tables'))

# cache
copy.cache <- grep(list.files(file.path(run_folder, 'data_cached')), pattern = 'mod_dat|mdc|percent|residual', inv = TRUE, value = TRUE)
copy.cache <- grep(pattern = nick, x = copy.cache, value = TRUE)
file.copy(from = file.path(run_folder,'data_cached',copy.cache), to = file.path(results_path,Rundate,'cache'))

# raw
copy.raw <- grep(list.files(file.path(run_folder, 'data_raw')), pattern = nick, inv = FALSE, value = TRUE)
file.copy(from = file.path(run_folder,'data_raw',copy.raw), to = file.path(results_path,Rundate,'raw'))
#----------------------
# end
#----------------------



