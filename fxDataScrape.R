
### Load Library
library(baseballr)
library(dplyr)
library(mgcv)
library(pitchRx)



# Set working directory
setwd("C:/Users/Noah/Downloads/fxDataScrape")

# List of date ranges to be scraped.  Range is from May through July 2016
dateList <- list(c('2016-05-01','2016-05-05'),
                 c('2016-05-06','2016-05-10'),
                 c('2016-05-11','2016-05-15'),
                 c('2016-05-16','2016-05-20'),
                 c('2016-05-21','2016-05-25'),
                 c('2016-05-26','2016-05-31'),
                 c('2016-06-01','2016-06-05'),
                 c('2016-06-06','2016-06-10'),
                 c('2016-06-11','2016-06-15'),
                 c('2016-06-16','2016-06-20'),
                 c('2016-06-21','2016-06-25'),
                 c('2016-06-26','2016-06-30'),
                 c('2016-07-01','2016-07-05'),
                 c('2016-07-06','2016-07-10'),
                 c('2016-07-11','2016-07-15'),
                 c('2016-07-16','2016-07-20'),
                 c('2016-07-21','2016-07-25'),
                 c('2016-07-26','2016-07-31'))

dataList <- list()

for (i in 1:12) {
  dataList[[i]] <- scrape_statcast_savant_pitcher_all(start_date = dateList[[i]][1],
                                     end_date = dateList[[i]][2])
}

### bind dataframe in each element of datalist together
big_data <- do.call(rbind, dataList)

### write data
write.csv(big_data, "pitchfx_may-july-2016.csv")


big_data <- read.csv("pitchfx_may-july-2016.csv")

### clean data
big_data$description <- as.character(big_data$description)
big_data$pitch_type <- as.character(big_data$pitch_type)


cPitch <- big_data %>% filter(description == "ball" | description == "called_strike") %>% 
       select(player_name,pitcher,batter,pitch_type,plate_x,plate_z,pfx_x,pfx_z,
              balls,strikes,stand,p_throws,sz_top,sz_bot,
              release_spin_rate,release_extension,release_speed,description)

cPitch <- cPitch %>% filter(!is.na(pitch_type) & !is.na(plate_x) & !is.na(plate_z) &
                                          !is.na(pfx_x) & !is.na(pfx_z))

cPitch$plate_x <- as.numeric(as.character(cPitch$plate_x))
cPitch$plate_z <- as.numeric(as.character(cPitch$plate_z))
cPitch$pfx_x <- as.numeric(as.character(cPitch$pfx_x))
cPitch$pfx_z <- as.numeric(as.character(cPitch$pfx_z))
cPitch$player_name <- as.character(cPitch$player_name)
cPitch$stand <- as.character(cPitch$stand)
cPitch$p_throws <- as.character(cPitch$p_throws)
cPitch$called_strike <- as.numeric(cPitch$description %in% "called_strike")
colnames(cPitch)[5] = 'px' 
colnames(cPitch)[6] = 'pz'

### fit model
start.time <- Sys.time()

m1 <- bam(called_strike ~ s(px, pz),
          data=subset(cPitch, pitch_type == "FA" | 
                        pitch_type == "FT" | 
                        pitch_type == "FF" |
                        pitch_type == "FC"), 
          family = binomial(link='logit'))

strikeFX(cPitch, model=m1) + ##, layer = facet_grid( . ~ stand)) +
  ggtitle("Fastball Strike Zone")

m2 <- bam(called_strike ~ s(px, pz),
          data=subset(cPitch, pitch_type == "CU"), 
          family = binomial(link='logit'))

strikeFX(cPitch, model=m2) + ##, layer = facet_grid( . ~ stand)) +
  ggtitle("Curveball Strike Zone")


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

