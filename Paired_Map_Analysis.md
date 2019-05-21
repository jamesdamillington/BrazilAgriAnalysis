---
title: "Paired Map Analysis - BrazilAgriAnalysis"
author: "James D.A. Millington"
date: "September 2018"
output: 
  html_document: 
    keep_md: yes
    code_folding: hide
    df_print: paged
---

## Examine differences between pairs of raster maps for given years (e.g. between classifications)



```r
library(tidyverse)
library(caret)   #for confusionMatrix
library(diffeR)  #for map comparison
library(knitr)
library(raster)
library(rasterVis)
library(gridExtra)


####FUNCTIONS
nat2pas <- function(x,y) { x == 1 & y == 5 }
pas2nat <- function(x,y) { x == 5 & y == 1 }

pas2oag <- function(x,y) { x == 5 & y == 2 }
oag2pas <- function(x,y) { x == 2 & y == 5 }

nat2oag <- function(x,y) { x == 1 & y == 2 }
oag2nat <- function(x,y) { x == 2 & y == 1 }

ag2oag <- function(x,y) { x == 3 & y == 2 }
oag2ag <- function(x,y) { x == 2 & y == 3 }



binRatify <- function(ras){
  
  ras <- ratify(ras)
  rat <- levels(ras)[[1]]
  rat$code <- c("No Change","Change")
  #if(length(rat) == 1) rat$code <- "unknown"
  #if(length(rat) == 2) rat$code <- c("No Change","Change")
  
  levels(ras) <- rat

  return(ras)
  
}



makeObsLUmap <- function(LU, year) {
  
  
  #add categories for later plotting etc. (see https://stackoverflow.com/a/37214431)
  LU <- ratify(LU)     #tell R that the map raster is categorical 
  rat <- levels(LU)[[1]]    #apply the levels (i.e. categories) 

  uLU <- unique(LU) 

  LUcols <- c()
  LUlabs <- c()
  
  if(1 %in% uLU) { 
    LUcols <- c(LUcols, 'forestgreen') 
    LUlabs <- c(LUlabs, 'Nat')  }
  if(2 %in% uLU) { 
    LUcols <- c(LUcols, 'darkcyan') 
    LUlabs <- c(LUlabs, 'OAg') }
  if(3 %in% uLU) { 
    LUcols <- c(LUcols, 'wheat2') 
    LUlabs <- c(LUlabs, 'Ag') }
  if(4 %in% uLU) { 
    LUcols <- c(LUcols, 'black') 
    LUlabs <- c(LUlabs, 'Oth') }
  if(5 %in% uLU) { 
    LUcols <- c(LUcols, 'orange2') 
    LUlabs <- c(LUlabs, 'Pas') }
  
  rat$LandUse <- LUlabs  
  levels(LU) <- rat 
  
  p <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = paste0(year),
      par.settings = list(
        layout.heights = list(top.padding = 0, bottom.padding = 0),
        layout.widths = list(left.padding = 0, right.padding = 0) 
        )
      )  

  return(p)
}
```



```r
pathA <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/LandCover/MapBiomas23/ClassificationComparison/ASCII/"

pathB <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/CRAFTYInput/Data/ObservedLCmaps/"


Aname <- "Original"
Bname <- "Planted Data"

data_yrs <- seq(2001, 2015, 1)

for(i in seq_along(data_yrs)){

  print(data_yrs[i])
  #create a stack of rasters for first set of maps
  innameA <- paste0(pathA,"brazillc_",data_yrs[i],"_PastureB.asc")
  lcA <- raster(innameA)
  
  if(i == 1) sA <- stack(lcA)  else  sA <- stack(sA, lcA)
  
  #create a stack of rasters for second set of maps
  innameB <- paste0(pathB,"NewAgri_brazillc_",data_yrs[i],"_PastureB.asc")
  lcB <- raster(innameB)
  
  if(i == 1) sB <- stack(lcB)  else sB <- stack(sB, lcB) 
  
}

#mask to study area
munis.r <- raster("C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/CRAFTYInput/Data/sim10_BRmunis_latlon_5km_2018-04-27.asc")

sA <- mask(x=sA, mask=munis.r)   
sA <- trim(sA, padding=2)

sB <- mask(x=sB, mask=munis.r)   
sB <- trim(sB, padding=2)
```


```r
LCnames <- c("Nat", "OtherAgri", "Agri", "Other", "Pasture")  #used to label error matrix in loop below

comparisons <- c(nat2pas, pas2nat, pas2oag, oag2pas, nat2oag, oag2nat, ag2oag, oag2ag) 
comparisons_n <- c("nat2pas", "pas2nat", "pas2oag", "oag2pas", "nat2oag", "oag2nat", "ag2oag", "oag2ag")

comparisons <- c(nat2pas, pas2nat, pas2oag, oag2pas, nat2oag, oag2nat, ag2oag, oag2ag) 
comparisons_n <- c("nat2pas", "pas2nat", "pas2oag", "oag2pas", "nat2oag", "oag2nat", "ag2oag", "oag2ag")


for(i in seq_along(data_yrs)){
  
  #i <- 2 #for testing
  
  lul <- list()  #this will hold the plots for the LU map for this year
  lul[[1]] <- makeObsLUmap(sA[[i]], Aname)
  lul[[2]] <- makeObsLUmap(sB[[i]], Bname)
  

  #output the crosstab  
  cat("  \n","  \n","Crosstab ",data_yrs[i],"  \n") 
  xtab <- crosstabm(sA[[i]], sB[[i]])
  colnames(xtab) <- LCnames
  rownames(xtab) <- LCnames
  cat("  \n")
  print(kable(xtab))
  cat("  \n")
  
  pl <- list()  #this will hold the plots for the all map for this year
  ts <- stack(sA[[i]], sB[[i]]) #stack used when creating difference maps

  for(j in seq_along(comparisons)){
    
    #j <- 2 #for testing
    np <- raster::overlay(x=ts, fun=comparisons[[j]])
    
    if(cellStats(np, sum) > 0) #if there are any differences
    {
      np <- binRatify(np) 
      mycols <- c("lightgray", "red")
    } else {
      mycols <- c("lightgray") 
    }
    
        #create the plot
        p <- levelplot(np,
        contour=F, 
        margin=F,
        colorkey=F,
        scales=list(draw=FALSE),
        col.regions= mycols,
        main = comparisons_n[j],
        par.settings = list(
          layout.heights = list(top.padding = 0, bottom.padding = 0),
          layout.widths = list(left.padding = 0, right.padding = 0) 
          )
        )
    
      pl[[j]] <- p   
    
  }
  
  
  print(marrangeGrob(lul, nrow = 1, ncol = 2, top = paste0("Observed Land Use")))
  print(marrangeGrob(pl, nrow = 2, ncol = 4, top = paste0(data_yrs[i])))

}
```

  
   
 Crosstab  2001   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          61618           0      0       0         0
OtherAgri      372        2147   1926       0     21466
Agri             0        2010   6542       0         0
Other            0           0      0    1918         0
Pasture       2148           0      0       0     61983
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-1.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-2.png)<!-- -->  
   
 Crosstab  2002   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          60922           0      0       0         0
OtherAgri      370        2365   1940       0     20916
Agri             0        2133   6994       0         0
Other            0           0      0    2037         0
Pasture       2151           0      0       0     62292
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-3.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-4.png)<!-- -->  
   
 Crosstab  2003   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          60258           0      0       0         0
OtherAgri      319        2574   2022       0     20124
Agri             0        2237   7361       0         0
Other            0           0      0    2038         0
Pasture       2176           0      0       0     63011
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-5.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-6.png)<!-- -->  
   
 Crosstab  2004   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          59473           0      0       0         0
OtherAgri      257        2670   2084       0     19204
Agri             0        2526   7470       0         0
Other            0           0      0    2000         0
Pasture       2217           0      0       0     63524
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-7.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-8.png)<!-- -->  
   
 Crosstab  2005   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58886           0      0       0         0
OtherAgri      249        2470   2010       0     18648
Agri             0        3047   7621       0         0
Other            0           0      0    2021         0
Pasture       2235           0      0       0     64238
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-9.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-10.png)<!-- -->  
   
 Crosstab  2006   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58718           0      0       0         0
OtherAgri      255        2328   1765       0     17173
Agri             0        2673   8521       0         0
Other            0           0      0    2052         0
Pasture       2229           0      0       0     65711
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-11.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-12.png)<!-- -->  
   
 Crosstab  2007   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58990           0      0       0         0
OtherAgri      282        2428   1609       0     17757
Agri             0        2922   8607       0         0
Other            0           0      0    2072         0
Pasture       2232           0      0       0     64526
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-13.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-14.png)<!-- -->  
   
 Crosstab  2008   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58810           0      0       0         0
OtherAgri      289        2864   1693       0     18301
Agri             0        2939   8567       0         0
Other            0           0      0    2075         0
Pasture       2212           0      0       0     63675
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-15.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-16.png)<!-- -->  
   
 Crosstab  2009   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58732           0      0       0         0
OtherAgri      328        3175   1553       0     19420
Agri             0        2924   8807       0         0
Other            0           0      0    2093         0
Pasture       2244           0      0       0     62149
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-17.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-18.png)<!-- -->  
   
 Crosstab  2010   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58852           0      0       0         0
OtherAgri      325        3081   1453       0     19768
Agri             0        3350   8576       0         0
Other            0           0      0    2120         0
Pasture       2227           0      0       0     61673
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-19.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-20.png)<!-- -->  
   
 Crosstab  2011   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58830           0      0       0         0
OtherAgri      313        3537   1595       0     20347
Agri             0        2745   9032       0         0
Other            0           0      0    2173         0
Pasture       2229           0      0       0     60624
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-21.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-22.png)<!-- -->  
   
 Crosstab  2012   
  

               Nat   OtherAgri   Agri   Other   Pasture
----------  ------  ----------  -----  ------  --------
Nat          58709           0      0       0         0
OtherAgri      292        3375   1475       0     19583
Agri             0        2957   9546       0         0
Other            0           0      0    2239         0
Pasture       2200           0      0       0     61049
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-23.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-24.png)<!-- -->  
   
 Crosstab  2013   
  

               Nat   OtherAgri    Agri   Other   Pasture
----------  ------  ----------  ------  ------  --------
Nat          59271           0       0       0         0
OtherAgri      288        3164    1279       0     15406
Agri             0        4034   10493       0         0
Other            0           0       0    2269         0
Pasture       2223           0       0       0     62998
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-25.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-26.png)<!-- -->  
   
 Crosstab  2014   
  

               Nat   OtherAgri    Agri   Other   Pasture
----------  ------  ----------  ------  ------  --------
Nat          59699           0       0       0         0
OtherAgri      270        3165    1484       0     12766
Agri             0        4776   10001       0         0
Other            0           0       0    2293         0
Pasture       2223           0       0       0     64748
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-27.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-28.png)<!-- -->  
   
 Crosstab  2015   
  

               Nat   OtherAgri    Agri   Other   Pasture
----------  ------  ----------  ------  ------  --------
Nat          59500           0       0       0         0
OtherAgri      294        2666    1316       0     11618
Agri             0        5513   10998       0         0
Other            0           0       0    2249         0
Pasture       2215           0       0       0     65056
  
![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-29.png)<!-- -->![](Paired_Map_Analysis_files/figure-html/unnamed-chunk-3-30.png)<!-- -->
