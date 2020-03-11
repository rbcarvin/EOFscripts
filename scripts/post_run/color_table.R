# Summarize Results across sites

# Increase/Decrease/NoChange Table

  # bring in before_after_residual_analysis tables
  # combine them
  # rotate them?
  # create color plot showing increase/decrease/NoChange based on p-value
  # save color plot
  # save table used to generate plot

###

main_path <- file.path('P:/0301/field_analysis/results')
#states <- list.files(main_path,full.names=TRUE,recursive=FALSE)
sites <- list.files(main_path,full.names=TRUE, recursive = FALSE)
sites <- grep(pattern = 'pair', x=sites, inv=TRUE, value=TRUE)
# for each site, put the most recent residual analysis table file path into a vector
tables_recent <- NULL
for (i in 1:length(sites)){
  temp_tables <- file.info(list.files(sites[i],pattern = 'before_after_residual_analysis',full.names = TRUE, recursive = TRUE))
  most_recent_residual_table <- rownames(temp_tables)[which.max(temp_tables$mtime)]
  tables_recent <- c(tables_recent,most_recent_residual_table)
  }

# make list of site names that match recent tables
siteID<-gsub(main_path,"",x = tables_recent)
siteID<-gsub("/20.*","",x=siteID)
siteID<-gsub("/","",x=siteID)

# Read that list of recent tables, add a site ID, stitch them together
residual_table <- data.frame()
for (j in 1:length(tables_recent)){
  table_temp <- read.csv(file=tables_recent[j],as.is = TRUE)
  table_temp$site <- siteID[j]
  residual_table <- rbind(residual_table, table_temp)
}

#
require(reshape2)
temp_change_table <- residual_table[,c("variable", "pvals_reduction","site")]
cast <-dcast(temp_change_table,site~variable,value.var = "pvals_reduction")

# create color plot
library(ggplot2)
library(tidyverse)

#colorTable <- readRDS("D:/LADData/colorTable.RDS")
colorTable <- cast

colorTable_long <- colorTable %>% 
  pivot_longer(-site, names_to = "param", values_to = "p_val") %>% 
  mutate(cat_orig = cut(p_val, include.lowest = TRUE,
                        breaks = c(0,0.05, 0.075, 0.1, 0.9, 0.925, 0.95, 1)))


category_df <- data.frame(
  cat_orig = factor(levels(colorTable_long$cat_orig), levels = levels(colorTable_long$cat_orig)),
  label = c("0-0.05", "0.05-0.75", "0.75-0.1", "middle", "0.75-0.1", "0.05-0.75", "0-0.05"),
  category = c("green_1", "green_2", "green_3", "middle", "red_3", "red_2", "red_1")
)



shortNames <- data.frame(
  param = c("SS load (pounds)","Chloride load (pounds)","NO2 + NO3 load (pounds)",     
            "Ammonium load (pounds)","TKN load (pounds)","Orthophosphate load (pounds)",
            "TP load (pounds)","TN load (pounds)","Org N load (pounds)",         
            "TOC load (pounds)","DOC load (pounds)","Peak discharge (cfs)"),
  short_name = c("Suspended Sed","Chloride","Nitrate + Nitrite",
                 "Ammonium","TKN","Ortho P",
                 "Total P","Total N","Org N","TOC","DOC","Peak flow (cfs)"),
  stringsAsFactors = FALSE
)

colorTable_long_clean <- colorTable_long %>% 
  left_join(shortNames, by = "param") %>% 
  left_join(category_df, by = "cat_orig") %>% 
  mutate(short_name = ifelse(is.na(short_name), param, short_name))

colorTable_long_clean$short_name <- factor(colorTable_long_clean$short_name,
                                           levels = c("Volume (cf)","Peak flow (cfs)","Suspended Sed",
                                                      "Chloride","Nitrate + Nitrite","Ammonium","TKN","Ortho P",
                                                      "Total P","Total N","Org N","TOC","DOC"))

color_scale <- setNames(c("green4", "green3", "green", "white", "red", "red3", "red4"),
                        category_df$cat_orig)

p <- ggplot(data = colorTable_long_clean) +
  geom_tile(aes(x = short_name, y = site, fill = cat_orig), colour = "grey25") +
  scale_fill_manual(na.value = "grey",  
                    values = color_scale,
                    name = "p-value", 
                    labels = c("0-0.05 decrease", "0.05-0.075", "0.075-0.1", "no significant change",
                               "0.075-0.1","0.05-0.075","0-0.05 increase"))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text( angle = 90,vjust=0.5,hjust = 0.975),
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid',colour = "black")) +
  guides(fill=guide_legend(reverse = TRUE))+
  ggtitle("Before and After - Change in Load ") + xlab("") + ylab("Site")
p

Rundate <- format(Sys.time(),"%Y-%m-%d-%H%M")
temp_filename <- paste0('P:/0301/field_analysis/meta-analysis/RedGreen_',Rundate,'.png')
ggsave(filename=temp_filename, p, height = 4, width = 6)
