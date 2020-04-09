
rm(list=ls())
library(tidyverse)
library(readxl)

maize_harv_Amunis_Data <- read_excel("ProductionData/maize_brazil.xlsx", 
  sheet = "Harvested area (hectares)", skip = 1, na = c("", "-", "..."), 
  col_types = c("numeric", "guess","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))  

data1718 <- read_excel("ProductionData/table839_maize_1718.xlsx", sheet ="Area colhida (Hectares)", skip=2,
  col_types = c("numeric", "guess","numeric","numeric"), na = c("", "-", "..."))  


combined <- left_join(maize_harv_Amunis_Data, data1718, by = c("IBGE CODE" = "CODE"))

write.csv(combined, "maize1718_harvArea_combined.csv")


maize_plant_Amunis_Data <- read_excel("ProductionData/maize_brazil.xlsx", 
  sheet = "Planted area (hectares)", skip = 1, na = c("", "-", "..."), 
  col_types = c("numeric", "guess","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))  

data161718 <- read_excel("ProductionData/table839_maize_161718.xlsx", sheet ="Area plantada (Hectares)", skip=2,
  col_types = c("numeric", "guess","numeric","numeric","numeric","numeric","numeric","numeric"), na = c("", "-", "..."))  


combined <- left_join(maize_plant_Amunis_Data, data161718, by = c("IBGE CODE" = "CODE"))

write.csv(combined, "maize161718_plantArea_combined.csv")




maize2_plant_Amunis_Data <- read_excel("ProductionData/maize2_brazil.xlsx", 
  sheet = "Planted area (hectares)", skip = 1, na = c("", "-", "..."), 
  col_types = c("numeric", "guess","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))  

data1718 <- read_excel("ProductionData/table839_maize2_1718.xlsx", sheet ="Area plantada (Hectares)", skip=2,
  col_types = c("numeric", "guess","numeric","numeric","numeric","numeric"), na = c("", "-", "..."))  


combined <- left_join(maize2_plant_Amunis_Data, data1718, by = c("IBGE CODE" = "CODE"))

write.csv(combined, "maize1718_plantArea_combined.csv")
