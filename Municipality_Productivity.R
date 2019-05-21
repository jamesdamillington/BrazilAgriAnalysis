#script to compare agriculture capital (maps) to municipality production data)

rm(list = ls())
#library(raster)
library(tidyverse)
library(sf)
#library(RColorBrewer)
#library(viridisLite)

years <- seq(2003,2016,1)

className <- "class-A-SH"
outputDir <- "Output"
#munis.r <- raster("SpatialData/sim10_BRmunis_latlon_5km_2018-04-27.asc")
BRmunis <- st_read("SpatialData/BRmunis_sim10_simple2.shp")

soy <- readr::read_csv("ProductionData/soybean_Crop.csv",col_types=cols(.default=col_integer()), na="NA")
maize1 <- readr::read_csv("ProductionData/maize_first.csv",col_types=cols(.default=col_integer()), na="NA")
maize2 <- readr::read_csv("ProductionData/maize_second.csv",col_types=cols(.default=col_integer()), na="NA")


clean_and_long <- function(data) {
  
  cdata <- data %>% 
    rename(muni = CODE) %>%
    mutate(state = (muni %/% 100000)) %>%
    mutate(stateNM = if_else(state == 17, "TO", 
       if_else(state == 29, "BA",
       if_else(state == 31, "MG",
       if_else(state == 35, "SP",
       if_else(state == 41, "PR",
       if_else(state == 42, "SC",
       if_else(state == 43, "RS", 
       if_else(state == 50, "MS",
       if_else(state == 51, "MT",
       if_else(state == 52, "GO", NULL
       ))))))))))
     ) %>%
    
    #regions as discussed with Ramon 2019-05-07
    mutate(regionNM = if_else(state == 17, "N", 
       if_else(state == 29, "N",
       if_else(state == 31, "E",
       if_else(state == 35, "E",
       if_else(state == 41, "S",
       if_else(state == 42, "S",
       if_else(state == 43, "S", 
       if_else(state == 50, "W",
       if_else(state == 51, "W",
       if_else(state == 52, "W", NULL
       ))))))))))
     ) %>%
    
    gather(key = year, value = prod, -muni, -state, -stateNM, -regionNM) %>%
    filter(!is.na(stateNM)) %>%
    filter(!is.na(prod))

    return(cdata)
  
}

#make data long
soyl <- clean_and_long(soy)
mz1l <- clean_and_long(maize1)
mz2l <- clean_and_long(maize2)

soy_sum <- soyl %>%
  group_by(year, stateNM, regionNM) %>%
  summarize(mean = mean(prod),
            sd   = sd(prod),
            mean_p2sd = mean + 2 * sd,
            mean_m2sd = mean - 2 * sd) %>%
  ungroup()

soy_sum %>%
  ggplot(aes(x=year, y=mean, group=stateNM, colour=stateNM)) +
  geom_line(size=1) + 
  facet_grid(regionNM~.) + 
  ggtitle("Soy") +
  ylab("Mean Municipality Productivity (k/ha)")

mz1_sum <- mz1l %>%
  group_by(year, stateNM, regionNM) %>%
  summarize(mean = mean(prod),
            sd   = sd(prod),
            mean_p2sd = mean + 2 * sd,
            mean_m2sd = mean - 2 * sd) %>%
  ungroup()

mz1_sum %>%
  ggplot(aes(x=year, y=mean, group=stateNM, colour=stateNM)) +
  geom_line(size=1) + 
  facet_grid(regionNM~.) + 
  ggtitle("Maize 1") +
  ylab("Mean Municipality Productivity (k/ha)")


mz2_sum <- mz2l %>%
  group_by(year, stateNM, regionNM) %>%
  summarize(mean = mean(prod),
            sd   = sd(prod),
            mean_p2sd = mean + 2 * sd,
            mean_m2sd = mean - 2 * sd) %>%
  ungroup()

mz2_sum %>%
  ggplot(aes(x=year, y=mean, group=stateNM, colour=stateNM)) +
  geom_line(size=1) + 
  facet_grid(regionNM~.) + 
  ggtitle("Maize 2") +
  ylab("Mean Municipality Productivity (k/ha)")


soyl %>%
  ggplot(aes(x=year, y=prod, group = muni)) +
  facet_grid(regionNM~.) +
  geom_line(na.rm=T) 

soyl %>%
  filter(stateNM == "TO") %>%
  ggplot(aes(x=year, group = muni, colour=muni)) +
  geom_bar() 
