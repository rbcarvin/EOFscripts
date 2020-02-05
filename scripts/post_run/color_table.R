# Summarize Results across sites

# Increase/Decrease/NoChange Table

  # bring in before_after_residual_analysis tables
  # combine them
  # rotate them?
  # create color plot showing increase/decrease/NoChange based on p-value
  # save color plot
  # save table used to generate plot

###

main_path <- file.path('P:/0301/analysis/')
states <- list.files(main_path,full.names=TRUE,recursive=FALSE)
sites <- list.files(states,full.names=TRUE, recursive = FALSE)

# for each site, put the most recent residual analysis table file path into a vector
tables_recent <- NULL
for (i in 1:length(sites)){
  temp_tables <- file.info(list.files(sites[i],pattern = 'before_after_residual_analysis',full.names = TRUE, recursive = TRUE))
  most_recent_residual_table <- rownames(temp_tables)[which.max(temp_tables$mtime)]
  tables_recent <- c(tables_recent,most_recent_residual_table)
  }

# make list of site names that match recent tables
siteID<-gsub(main_path,"",x = tables_recent)
siteID<-gsub("/results.*","",siteID)
siteID<-gsub(pattern = "\\/([a-zA-Z0-9]{0,})/","",x=siteID)

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
# https://labrtorian.com/2016/11/07/conditional-formatting-of-a-table-in-r/

x=1:ncol(detected_change_table)
y=1:nrow(detected_change_table)
centers <- expand.grid(y,x)
par(mar=c(2,7,4,2))
image(x, y, t(detected_change_table),
      col = c(rgb(0,0,1,0.3),rgb(1,0,0,0.3), rgb(1,1,0,0.3)),
      breaks = c(0, 25, 50, 100),
      xaxt = 'n', 
      yaxt = 'n', 
      xlab = '', 
      ylab = '',
      ylim = c(max(y) + 0.5, min(y) - 0.5)
)

# goal: \\gs.doi.net\UpperMidwest-W\Legacy Projects\NonPoint Evaluation\GLRI Edge-of-field\Presentations\NRCS-GLRI Update meeting 11-13-19

#https://stackoverflow.com/questions/18663159/conditional-coloring-of-cells-in-table
palette(c(RColorBrewer::brewer.pal(8, "Pastel1"),
          RColorBrewer::brewer.pal(8, "Pastel2")))


require(gtable)
require(grid)
gtable_add_grobs <- gtable_add_grob # alias

d <- head(cast, 3)
nc <- ncol(d)
nr <- nrow(d)

extended_matrix <- cbind(c("", rownames(d)), rbind(colnames(d), as.matrix(d))) 

## text for each cell
all_grobs <- matrix(lapply(extended_matrix, textGrob), ncol=ncol(d) + 1)

## define the fill background of cells
fill <- lapply(seq_len(nc*nr), function(ii) 
  rectGrob(gp=gpar(fill=ii)))

## some calculations of cell sizes
row_heights <- function(m){
  do.call(unit.c, apply(m, 1, function(l)
    max(do.call(unit.c, lapply(l, grobHeight)))))
}

col_widths <- function(m){
  do.call(unit.c, apply(m, 2, function(l)
    max(do.call(unit.c, lapply(l, grobWidth)))))
}

## place labels in a gtable
g <- gtable_matrix("table", grobs=all_grobs, 
                   widths=col_widths(all_grobs) + unit(4,"mm"), 
                   heights=row_heights(all_grobs) + unit(4,"mm"))

## add the background
g <- gtable_add_grobs(g, fill, t=rep(seq(2, nr+1), each=nc), 
                      l=rep(seq(2, nc+1), nr), z=0,name="fill")

## draw
grid.newpage()
grid.draw(g)
