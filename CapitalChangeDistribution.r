
#plots the distribution of Capital values for cells each year in which EMPIRICALLY agriculture is established

#read change locations from raster
#read capital maps from either cell CSV or raster
#create table for each year with columns by capital and a change/no change column
#paired boxplots or dual histograms for distro of capitals on change vs no change cells

#some capital maps can come from cell CSV:
#moisture - KEY
#growing season - KEY
#Human
#Development
#Port Access- KEY
#Other [this can come from here because update maps are from observed LC]
#Soy Protection
#Land Price- KEY
#Maize Protection
#Pasture Protection
#OAgri Proection

#some are dependent on land cover
#agriInfrastructure [THIS TAKES ~2.5 HOURS TO CALCULATE IN accessMap.r - I CURRENTLY HAVE FOR 2001, 2016 AND 2018]
#OAgriInfrastructure [IF I AM TO USE THIS I WILL NEED TO DISAGGREGATE AGRI VS OAGRI IN ALL OBSERVED LC MAPS... START WITH JUST AGRI 
#Nature [CALCULATED WITHIN CRAFTY?]
#NatureAccess [THIS TAKES ~8 HOURS TO CALCULATE IN accessMap.r - I CURRENTLY HAVE FOR 2001, 2016 AND 2018]
#OtherAgriculture [NOT CURRENTLY USED]

#so initially run for 2001-2002 and 2016-2017
#read 2001 cells csv, 2001 input rasters, 2001-2002 diffc map


rm(list=ls())

#required packages
library(tidyverse)
library(raster)

######
#FUNCTIONS
#raster to xyz  (with help from https://stackoverflow.com/a/19847419)
#sepcify input raster, whether nodata cells should be output, whether a unique cell ID should be added
#return is a matrix. note format is row (Y) then col (X)
extractXYZ <- function(raster, nodata = FALSE, addCellID = TRUE){
  
  vals <- raster::extract(raster, 1:ncell(raster))   #specify raster otherwise dplyr used
  xys <- rowColFromCell(raster,1:ncell(raster))
  combine <- cbind(xys,vals)
  
  if(addCellID){
    combine <- cbind(1:length(combine[,1]), combine)
  }
  
  if(!nodata){
    combine <- combine[!rowSums(!is.finite(combine)),]  #from https://stackoverflow.com/a/15773560
  }
  
  return(combine)
}

#read raster data function
readMapXYZ <- function(mapz)
{
  #map <- raster(path)     #read raster
  map <-flip(mapz, 'y')    #flip maps as CRAFTY read values from the bottom of the map
  map <- extractXYZ(map)  #convert from map to xyz (as tibble)
  return(as_tibble(map))  #return xyz as tibble
}

createEmpiricalData <- function(year, scenario, runID, LC){
  
  #CRAFTY output cell file
  cell_dir <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/CRAFTY_testing/CRAFTYOutput/Data/"
  cellData <- read.csv(paste0(cell_dir,scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",year,".csv"))
  
  #land cover difference raster
  lc_dir <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/LandCover/MapBiomas4/BrazilInputMaps/Data/Classified/"
  LCdiffc <- raster(paste0(lc_dir,"Diffc_PastureB_Disagg.asc_",year,"-",year+1,"_toLC",LC,".asc"))  #land cover from LandCoverMap.r (or ClassifyDisaggregateMap.r)
  LCdiffc.xy <- readMapXYZ(LCdiffc) 
  
  #access files
  ac_dir <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/CRAFTYInput/Data/"
  AgriAccess <- raster(paste0(ac_dir,"AgriAccess_",year,"_PastureB_Disagg.asc"))
  AgriAccess.xy <- readMapXYZ(AgriAccess) 
  OAgriAccess <- raster(paste0(ac_dir,"OAgriAccess_",year,"_PastureB_Disagg.asc"))
  OAgriAccess.xy <- readMapXYZ(OAgriAccess) 
  NatAccess <- raster(paste0(ac_dir,"NatureAccess_",year,"_PastureB_Disagg.asc"))
  NatAccess.xy <- readMapXYZ(NatAccess) 
  
  
  cellData <- dplyr::select(cellData, -Tick, -RegionSet, -CellRegion, -starts_with("Service"))
  
  join.xy <- left_join(cellData, LCdiffc.xy, by = c("Y" = "row", "X" = "col")) %>%
    dplyr::select(-V1) %>%
    rename(LCdiffc = vals) %>%
    left_join(., AgriAccess.xy, by = c("Y" = "row", "X" = "col")) %>%
    dplyr::select(-V1) %>%
    rename("AgriAccess" = vals) %>% 
    left_join(., NatAccess.xy, by = c("Y" = "row", "X" = "col")) %>%
    dplyr::select(-V1) %>%
    rename("NatAccess" = vals) %>%
    left_join(., OAgriAccess.xy, by = c("Y" = "row", "X" = "col")) %>%
    dplyr::select(-V1) %>%
    rename("OAgriAccess" = vals) %>%
    dplyr::rename_at(vars(starts_with('Capital')), list(~ str_remove(.,"Capital."))) %>%
    dplyr::select(X, Y, LCdiffc, AgriAccess, Growing.Season, Land.Price, Moisture, NatAccess, OAgriAccess, Port.Access) #%>%
 
  join.xy <- join.xy %>%
    mutate(LCdiffc = replace(LCdiffc , LCdiffc == 0, "No Diffc")) %>%
    mutate(LCdiffc = replace(LCdiffc , LCdiffc == 1, "Diffc")) %>%
    mutate(LCdiffc = factor(LCdiffc, levels=c("No Diffc", "Diffc")))
    
  join.long <- join.xy %>%
    gather(key = CapitalName, value = Capital, -X, -Y, -LCdiffc) %>%
    group_by(LCdiffc, CapitalName) 

  return(join.long)
}


createModelData <- function(year, scenario, runID, LC){
  
  #CRAFTY output cell file
  cell_dir <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/CRAFTY_testing/CRAFTYOutput/Data/"
  cellData <- read.csv(paste0(cell_dir,scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",year,".csv"))
  
  tocellData <- read.csv(paste0(cell_dir,scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",year+1,".csv"))

  tocellData <- tocellData %>%
    dplyr::select(X, Y, Agent)
  
  cellData <- cellData %>%
    left_join(tocellData, by = c("X", "Y")) %>%
    rename(OldAgent = Agent.x, NewAgent=Agent.y)
  
  #if Agriculture
  if(LC == 3){
    cellData <- cellData %>%
      mutate(LCdiffc = 
          if_else(NewAgent == "FR1" & OldAgent != "FR1" & OldAgent != "FR2" & OldAgent != "FR3" & OldAgent != "FR6", "Diffc",
          if_else(NewAgent == "FR2" & OldAgent != "FR1" & OldAgent != "FR2" & OldAgent != "FR3" & OldAgent != "FR6", "Diffc",
          if_else(NewAgent == "FR3" & OldAgent != "FR1" & OldAgent != "FR2" & OldAgent != "FR3" & OldAgent != "FR6", "Diffc",
          if_else(NewAgent == "FR6" & OldAgent != "FR1" & OldAgent != "FR2" & OldAgent != "FR3" & OldAgent != "FR6", "Diffc", "No Diffc"
              ))))
      )
  }
  
  #if Pasture
  if(LC == 5){
    cellData <- cellData %>%
      mutate(LCdiffc = 
          if_else(NewAgent == "FR8" & OldAgent != "FR8", "Diffc", "No Diffc")
      )
  }
  
  cellData <- cellData %>%
    dplyr::select(-Tick, -RegionSet, -CellRegion, -starts_with("Service")) %>%
    dplyr::rename_at(vars(starts_with('Capital')), list(~ str_remove(.,"Capital."))) %>%
    dplyr::select(X, Y, LCdiffc, Agri.Infrastructure, Growing.Season, Land.Price, Moisture, Nature.Access, OAgri.Infrastructure, Port.Access) %>%
    mutate(LCdiffc = factor(LCdiffc, levels=c("No Diffc", "Diffc")))
    
  join.long <- cellData %>%
    gather(key = CapitalName, value = Capital, -X, -Y, -LCdiffc) %>%
    group_by(LCdiffc, CapitalName) 

  return(join.long)
}
#############

scenario <- "testing_demand_smoother3"
runID <- "0-0"
targetLC <- 5


#2001-2002

dat0102 <- createEmpiricalData(year=2001,scenario,runID,targetLC)

summarise(dat0102, count = n(), med = median(Capital, na.rm=T), ave = mean(Capital, na.rm=T))

# #proportion of cells with specified Capital value for different Capitals and LCdiffc
# AA_ND_25p <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'No Diffc' & Capital == 0.25)$Capital ) / length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'No Diffc')$Capital )
# AA_D_25p <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'Diffc' & Capital == 0.25)$Capital ) / length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'Diffc')$Capital )
# 
# AA_ND_75p <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'No Diffc' & Capital == 0.75)$Capital ) / length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'No Diffc')$Capital )
# AA_D_75p <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'Diffc' & Capital == 0.75)$Capital ) / length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'Diffc')$Capital )
# 
# 
# #count  of cells with specified Capital value for different Capitals and LCdiffc
# AA_ND_25 <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'No Diffc' & Capital == 0.25)$Capital )
# AA_D_25 <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'Diffc' & Capital == 0.25)$Capital )
# 
# AA_ND_75 <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'No Diffc' & Capital == 0.75)$Capital )
# AA_D_75 <- length( filter(dat0102, CapitalName == "AgriAccess" & LCdiffc == 'Diffc' & Capital == 0.75)$Capital )


dat0102 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2001-2002 Empirical")
ggsave("Box_To2002_Empirical.pdf")


dat0102 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2001-2002 Empirical")
ggsave("Violin_To2002_Empirical.pdf")
  


mod0102 <- createModelData(year=2001,scenario,runID,targetLC)
summarise(mod0102, count = n(), med = median(Capital, na.rm=T), ave = mean(Capital, na.rm=T))


mod0102 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2001-2002 Modelled")
ggsave("Box_To2002_Modelled.pdf")


mod0102 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2001-2002 Modelled")
ggsave("Violin_To2002_Modelled.pdf")


#2016-2017

dat1617 <- createEmpiricalData(year=2016,scenario,runID,targetLC)

summarise(dat1617, count = n(), med = median(Capital, na.rm=T), ave = mean(Capital, na.rm=T))

dat1617 %>%
  #sample_frac(0.5) %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2016-2017 Empirical")
ggsave("Box_To2017_Empirical.pdf")


dat1617 %>%
  #sample_frac(0.5) %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2016-2017")
ggsave("Violin_To2017_Empirical.pdf")
  

mod1617 <- createModelData(year=2016,scenario,runID,targetLC)
summarise(mod1617, count = n(), med = median(Capital, na.rm=T), ave = mean(Capital, na.rm=T))


mod1617 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2016-2017 Modelled")
ggsave("Box_To2017_Modelled.pdf")


mod1617 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2016-2017 Modelled")
ggsave("Violin_To2017_Modelled.pdf")




dat0506 <- createEmpiricalData(year=2005,scenario,runID,targetLC)

summarise(dat0506, count = n(), med = median(Capital, na.rm=T), ave = mean(Capital, na.rm=T))

dat0506 %>%
  #sample_frac(0.5) %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2005-2006 Empirical")
ggsave("Box_To2006_Empirical.pdf")


dat0506 %>%
  #sample_frac(0.5) %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2005-2006")
ggsave("Violin_To2006_Empirical.pdf")


mod0506 <- createModelData(year=2005,scenario,runID,targetLC)
summarise(mod0506, count = n(), med = median(Capital, na.rm=T), ave = mean(Capital, na.rm=T))


mod0506 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2005-2006 Modelled")
ggsave("Box_To2006_Modelled.pdf")


mod0506 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2005-2006 Modelled")
ggsave("Violin_To2006_Modelled.pdf")



mod1011 <- createModelData(year=2010,scenario,runID,targetLC)
summarise(mod1011, count = n(), med = median(Capital, na.rm=T), ave = mean(Capital, na.rm=T))


mod1011 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2010-2011 Modelled")
ggsave("Box_To2011_Modelled.pdf")


mod1011 %>%
  ggplot(aes(x=CapitalName, y=Capital, fill=LCdiffc)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("2010-2011 Modelled")
ggsave("Violin_To2011_Modelled.pdf")


