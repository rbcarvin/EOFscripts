#----------------------
# FILE PULL AND PUSH - BEFORE/AFTER ANALYSIS
#----------------------
    #----------------------
    # PULL pre-run
    #----------------------
    # Site Nickname
    nick <- "WI-SW4"
    #nick.1 <- ""
    
    # site_folder - point to the folder with site-specific data needed for this run
    site_folder <- file.path('P:/0301/field_analysis/approved_site_data')
    
    # Temporarily copy raw data to data_raw
    move.me <- list.files(site_folder, pattern = nick, full.names=FALSE, recursive = FALSE)
    move.this <- file.path(site_folder,move.me)
    move.here <- file.path("D:/EOF/EOFscripts/data_raw")
    
    # remove any old files in the local 'data_raw' folder with the same name
    file.remove(file.path(move.here,move.me))
    
    # copy site-specific data_raw files to the local 'data_raw' folder
    file.copy(from = move.this, to = move.here)
    #----------------------
    # end
    #----------------------
    #----------------------
    # PUSH Post_run
    #----------------------
      # Send results to a results folder within the site folder on the P: drive
      
      # build the results path
      results_path <- file.path('P:/0301/field_analysis/results',nick)
      #nick <- nick.1
      
      # create folders for the most recent run
      Rundate <- format(Sys.time(),"%Y-%m-%d-%H%M")
      
      dir.create(file.path(results_path,Rundate))
      dir.create(file.path(results_path,Rundate,'figures'))
      dir.create(file.path(results_path,Rundate,'figures','diagnostic'))
      dir.create(file.path(results_path,Rundate,'tables'))
      dir.create(file.path(results_path,Rundate,'cache'))
      dir.create(file.path(results_path,Rundate,'raw'))
      
      # copy site-specific results files to the site 'results' folder:
        # figures
        # tables (useful .csv table(s))
        # cache (less useful)
        # raw (to retrace which data input was used)
      
      run_folder <- file.path('D:/EOF/EOFscripts')
      
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

      
      
######################################################################################################################################  
# Paired Sites Only - Pull
  
  control_site <- file.path('P:/0301/analysis/WI/WI-SW4')
  treat_site <- file.path('P:/0301/analysis/WI/WI-SW5')
  paired_site <- file.path('P:/0301/analysis/WI/WI-SW4-SW5-pair')
  site_folder <- paired_site
  
  # Temporarily copy raw data to D:EOF/EOFscripts/data_raw
  
  move.me.c <- list.files(file.path(control_site, 'approved_site_data'), pattern = '.csv', full.names=FALSE, recursive = FALSE)
  move.this.c <- file.path(control_site,'approved_site_data',move.me.c)
  
  move.me.t <- list.files(file.path(treat_site, 'approved_site_data'), pattern = '.csv', full.names=FALSE, recursive = FALSE)
  move.this.t <- file.path(treat_site,'approved_site_data',move.me.t)
  
  move.me.p <- list.files(file.path(paired_site, 'approved_site_data'), pattern = '', full.names=FALSE, recursive = FALSE)
  move.this.p <- file.path(paired_site,'approved_site_data',move.me.p)
  
  move.me <- c(move.me.c,move.me.t,move.me.p)
  move.this <- c(move.this.c,move.this.t,move.this.p)
  
  move.here <- file.path("D:/EOF/EOFscripts/data_raw")
  
  # remove any old files in the local 'data_raw' folder with the same name
  file.remove(file.path(move.here,move.me))
  
  # copy site-specific data_raw files to the local 'data_raw' folder
  file.copy(from = move.this, to = move.here)
  