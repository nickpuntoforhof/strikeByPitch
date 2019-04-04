
### Load Library
library(baseballr)
library(dplyr)
library(mgcv)
library(pitchRx)
library(xgboost)
library(ash)
library(parallel)

# Set working directory
setwd("C:/Users/Noah/Downloads/fxDataScrape")

##################################################################################
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


#####################################################################################
### read data in. Start here if you have already scraped the data
big_data <- read.csv("pitchfx_may-july-2016.csv")

### change some data types
big_data$description <- as.character(big_data$description)
big_data$pitch_type <- as.character(big_data$pitch_type)

### get only balls/called strikes 
cPitch <- big_data %>% filter(description == "ball" | description == "called_strike") %>% 
       select(player_name,pitcher,batter,pitch_type,plate_x,plate_z,pfx_x,pfx_z,
              balls,strikes,stand,p_throws,sz_top,sz_bot,
              release_spin_rate,release_extension,release_speed,description)

### filter out missing observations
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


### bin categories of pitches to create more general pitch type
pitch_categorizer <- function(data_row){
  fastball <- c("FF", "FT", "SI")
  curveball <- c("CU", "KC", "EP")
  slider <- c("FC", "SL")
  offspeed <- c("CH", "SC", "FS", "FO")
  knuckle <- c("KC")
  omit <- c("UN")
  p = data_row[4]
  if(p %in% fastball){
    return("FB")
  } else if (p %in% curveball) {
    return("CU")
  } else if (p %in% slider) {
    return("SL")
  } else if (p %in% offspeed) {
    return("OF")
  } else if (p %in% knuckle) {
    return("KN")
  } else {
    return("UN")
  }
}
cPitch$pitch_bin <- factor(apply(cPitch, 1, pitch_categorizer))

### create a categorical variable for count
### make sure to convert all categorical variables to factors
### before adding them to the model
cPitch$count <- factor(paste0("(",cPitch$balls,",",cPitch$strikes,")"))
cPitch$stand <- factor(cPitch$stand)


### new model
require(parallel)  
nc <- 2   ## cluster size, set for example portability
if (detectCores()>1) { ## no point otherwise
  cl <- makeCluster(nc) 
  ## could also use makeForkCluster, but read warnings first!
} else cl <- NULL

system.time(m1 <- bam(called_strike ~ interaction(stand, count) + s(px, pz, by = interaction(stand, count)),
                      data= cPitch, 
                      family = binomial(link='logit'),
                      chunk.size = 5000, cluster = cl))

### save m1 object
save(m1, file = "bam_stand_count_interaction")
### 


### create subsets of data to train different models on ###
cPitch_fastballs <- subset(cPitch, pitch_type == "FA" | 
         pitch_type == "FT" | 
         pitch_type == "FF" |
         pitch_type == "FC")
cPitch_curveballs <- subset(cPitch, pitch_type == "CU")
cPitch_sliders <- subset(cPitch, pitch_type == "SL")

cPitch_filtered <- cPitch[,c(5,6,19)]
cPitch_fastballs <- cPitch_fastballs[,c(5,6,19)]
cPitch_curveballs <- cPitch_curveballs[,c(5,6,19)]
cPitch_sliders <- cPitch_sliders[,c(5,6,19)]

### create grid of values to test model on ###
x.grid <- seq(-1.5, 1.5, 0.005)
y.grid <- seq(0, 4.5, 0.005)
grid <- expand.grid(px=x.grid, pz=y.grid)
grid <- as.matrix(grid)

### bucket pitches ###
### 72 buckets horizontal ###
### 108 buckets vertical ###
cut(cPitch_filtered$px, 72, dig.lab = )


## xboost model
fast_matrix_data <- as.matrix(cPitch_filtered[,1:2])
fast_matrix_label <- as.vector(cPitch_filtered[,3])

xgb_fast <- xgboost(data = fast_matrix_data, label = fast_matrix_label, 
                    objective = "binary:logistic", nrounds = 2)

pred <- predict(xgb_fast, grid)
hist(pred)

pred_fast <- as.data.frame(cbind(grid,pred))

ggplot(pred_fast, aes(x=px,y=pz,fill=pred)) + geom_tile()
ggplot(pred_fast, aes(x=px,y=pred)) + geom_point() + geom_smooth()


### bam model
start.time <- Sys.time()

m1 <- bam(called_strike ~ s(px, pz, by = ),
          data= cPitch_fastballs, 
          family = binomial(link='logit'))

pred_bam <- predict(m1, as.data.frame(grid))


hist(pred_bam)

ggplot(grid, aes(x = px, y = pz)) + geom_point()



#strikeFX(cPitch, model=m1) + ##, layer = facet_grid( . ~ stand)) +
 # ggtitle("Fastball Strike Zone")

m2 <- bam(called_strike ~ s(px, pz),
          data=subset(cPitch, pitch_type == "CU"), 
          family = binomial(link='logit'))

strikeFX(cPitch, model=m2) + ##, layer = facet_grid( . ~ stand)) +
  ggtitle("Curveball Strike Zone")


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

