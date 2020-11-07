'''
Script developed for quantitative analysis of digital (RGB) images regarding pigment percentages (CMYK colorspace) and luminance values.
Written by Luciano de Sampaio Soares <luciano.soares(a)unir.br/https://www.lucsampaio.me> as a helper script for the doctoral thesis by Carolina Vigna Prado, PhD <https://carolina.vigna.com.br/>.
'''
library(tidyverse)
library(lubridate)
library(raster)

colorchart <- read_csv('colorchart.csv', sep = ";", dec = ",",
  progress = show_progress()) #this is a 1G csv file that can be found @ https://www.dropbox.com/s/juenc8447i8xl57/colorlist.csv?dl=0


dirList <- list.dirs(getwd(),full.names = T)

#function "throws" the image against sieve and keeps only color matches
colorSieve <- function(dirNum){
  StartTime <<- now() #marks start to keep time of processing
  fileList <<- list.files(dirList[dirNum],pattern = "\\.jpg$",ignore.case = T)
  df <<- tibble("title" = as.character(), "cyanPerc" = as.numeric(),
                "magentaPerc"=as.numeric(),"yellowPerc"=as.numeric(),
                "blackPerc" = as.numeric(), "lum_max" = as.numeric(),
                "lum_med" = as.numeric(), "lum_min"=as.numeric())
  for(i in 1:length(fileList)){
    print(i)
    fileLink <- paste0(dirList[dirNum],"/",fileList[i],sep = "") #finds file in chosen dir
    print(fileLink) #prints filename for checking purposes
    img <- image_read(fileLink) #reads image from fileLink
    data <- as.raster(img) #rasterizes image to ensure pixels can be sieved
    cmyk <- filter(colorchart, colorchart$hex  %in% data) #"throws" image against color list to filter
    df <<- add_row(vg, "title" = fileList[i],
                        "cyanPerc" = sum(cmyk$C)/nrow(cmyk), 
                        "magentaPerc" = sum(cmyk$M)/nrow(cmyk),
                        "yellowPerc" = sum(cmyk$Y)/nrow(cmyk),
                        "blackPerc" = sum(cmyk$K)/nrow(cmyk),
                        "lum_max" = max(cmyk$L),
                        "lum_med" = median(cmyk$L),
                        "lum_min"= min(cmyk$L))
    print(now()-StartTime) #prints out time elapsed since process started
  }

  tablename <- paste0(dirList[dirNum],".csv",sep = "") #output filename
  fwrite(vg, file = tablename,sep = ";",dec = ",",row.names = F, col.names = T) #writes output csv file

}

colorSieve(x) #x = desired folder number in dirList