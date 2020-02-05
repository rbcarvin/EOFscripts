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
  param = c("Ammonium load (pounds)",
            "Chloride load (pounds)",     
            "NO2 + NO3 load (pounds)"),
  short_name = c("Ammonium",
                 "Chloride",
                 "TKN"),
  stringsAsFactors = FALSE
)

colorTable_long_clean <- colorTable_long %>% 
  left_join(shortNames, by = "param") %>% 
  left_join(category_df, by = "cat_orig") %>% 
  mutate(short_name = ifelse(is.na(short_name), param, short_name))

color_scale <- setNames(c("green", "green3", "green4", "white", "red", "red3", "red4"),
                        category_df$cat_orig)

ggplot(data = colorTable_long_clean) +
  geom_tile(aes(x = short_name, y = site, fill = cat_orig)) +
  theme_bw() +
  theme(axis.text.x = element_text( angle = 90,vjust=0.5,hjust = 0.975)) +
  scale_fill_manual(na.value = "grey",  
                    values = color_scale)
