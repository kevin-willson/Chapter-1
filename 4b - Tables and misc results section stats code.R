# The first part of this code calculates data for supplementary tables.
# The second part of this code quantifies the misc. values that are referenced in the results section

library(readxl)
library(dplyr)
library(bimixt)
library(car)
library(MASS)
library(tidyr)
library(data.table)
library(xlsx)
library(forecast)
library(lmtest)
library(MuMIn)
library(caret)
library(broom)
library(rstatix)
library(gridExtra)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(statmod)
library(sjPlot)

## Prepare data for analysis
setwd("D:/Dryad_submission/Data/")

setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Writing/Question 1/Dryad submission/Question 1/Data")

seedling_distance <- read_excel("Transect_Data_5-25-21.xlsx", sheet = 1)

### Remove transects that had nearby overstory trees at the far end of the transect
seedling_distance <- seedling_distance[!(seedling_distance$Fire == "Shelly" & seedling_distance$Patch == "NB" & seedling_distance$Transect == "2"),]
seedling_distance <- seedling_distance[!(seedling_distance$Fire == "Shelly" & seedling_distance$Patch == "1" & seedling_distance$Transect == "2"),]
seedling_distance <- seedling_distance[!(seedling_distance$Fire == "Glass" & seedling_distance$Patch == "2" & seedling_distance$Transect == "7"),]
seedling_distance <- seedling_distance[!(seedling_distance$Fire == "Shelly" & seedling_distance$Patch == "NB" & seedling_distance$Transect == "1"),]
seedling_distance <- seedling_distance[!(seedling_distance$Fire == "Divide" & seedling_distance$Patch == "6" & seedling_distance$Transect == "10"),]

Plotdata_HA <- read_excel("Plot_data.xlsx", sheet = 1)
Plotdata_HA$Direction <- ifelse(Plotdata_HA$Cont_Wind_direc<=0, "No", "Yes")

### Remove transects that had nearby overstory trees at the far end of the transect
Plotdata_HA <- Plotdata_HA[!(Plotdata_HA$Fire == "Shelly" & Plotdata_HA$Patch == "NB" & Plotdata_HA$Transect == "2"),]
Plotdata_HA <- Plotdata_HA[!(Plotdata_HA$Fire == "Shelly" & Plotdata_HA$Patch == "1" & Plotdata_HA$Transect == "2"),]
Plotdata_HA <- Plotdata_HA[!(Plotdata_HA$Fire == "Glass" & Plotdata_HA$Patch == "2" & Plotdata_HA$Transect == "7"),]
Plotdata_HA <- Plotdata_HA[!(Plotdata_HA$Fire == "Shelly" & Plotdata_HA$Patch == "NB" & Plotdata_HA$Transect == "1"),]
Plotdata_HA <- Plotdata_HA[!(Plotdata_HA$Fire == "Divide" & Plotdata_HA$Patch == "6" & Plotdata_HA$Transect == "10"),]

### And have to remove the plot without overstory tree heights
Plotdata_HA <- Plotdata_HA[!is.na(Plotdata_HA$AvgHeight),]


seedling_distance$FPT <- paste(seedling_distance$Fire, "-", seedling_distance$Patch, "-", seedling_distance$Transect)
seedling_distance <- seedling_distance[!is.na(seedling_distance$Vertical_Distance),]

## Get equal densities across all transects (just shelly 1-1 that was run at 1 meter width instead of 10 meters)
x <- 1
additional_shelly_1_1 <- seedling_distance[FALSE,]
d <- seedling_distance[FALSE,]

while(x <= length(seedling_distance$Fire)){
  if(seedling_distance$Fire[x] == "Shelly" & seedling_distance$Patch[x] == 1 & seedling_distance$Transect[x] == 1){
    d <- rbind(seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],
               seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],seedling_distance[x,])
    additional_shelly_1_1 <- rbind(additional_shelly_1_1, d)
    x <- x + 1
  }
  else{
    x <- x + 1
  }
}

seedling_distance <- rbind(seedling_distance, additional_shelly_1_1)


## Convert data to densities
### Starting with 16 10x10 meter blocks for ease of analysis. Can change if needed
x <- 1
seedling_distance$Distance <- NA
while(x <= length(seedling_distance$Fire)){
  if(seedling_distance$Vertical_Distance[x] <= 10){
    seedling_distance$Distance[x] <- 5
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 10 & seedling_distance$Vertical_Distance[x] <= 20){
    seedling_distance$Distance[x] <- 15
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 20 & seedling_distance$Vertical_Distance[x] <= 30){
    seedling_distance$Distance[x] <- 25
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 30 & seedling_distance$Vertical_Distance[x] <= 40){
    seedling_distance$Distance[x] <- 35
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 40 & seedling_distance$Vertical_Distance[x] <= 50){
    seedling_distance$Distance[x] <- 45
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 50 & seedling_distance$Vertical_Distance[x] <= 60){
    seedling_distance$Distance[x] <- 55
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 60 & seedling_distance$Vertical_Distance[x] <= 70){
    seedling_distance$Distance[x] <- 65
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 70 & seedling_distance$Vertical_Distance[x] <= 80){
    seedling_distance$Distance[x] <- 75
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 80 & seedling_distance$Vertical_Distance[x] <= 90){
    seedling_distance$Distance[x] <- 85
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 90 & seedling_distance$Vertical_Distance[x] <= 100){
    seedling_distance$Distance[x] <- 95
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 100 & seedling_distance$Vertical_Distance[x] <= 110){
    seedling_distance$Distance[x] <- 105
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 110 & seedling_distance$Vertical_Distance[x] <= 120){
    seedling_distance$Distance[x] <- 115
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 120 & seedling_distance$Vertical_Distance[x] <= 130){
    seedling_distance$Distance[x] <- 125
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 130 & seedling_distance$Vertical_Distance[x] <= 140){
    seedling_distance$Distance[x] <- 135
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 140 & seedling_distance$Vertical_Distance[x] <= 150){
    seedling_distance$Distance[x] <- 145
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 150 & seedling_distance$Vertical_Distance[x] <= 160){
    seedling_distance$Distance[x] <- 155
    x <- x+1
  }
}

## Group these seedlings by transect and distance to count the number of each
seed_density_distance <- seedling_distance %>% 
  group_by(Fire, Patch, Transect, Distance) %>% 
  tally()

unique_transects <- Plotdata_HA[,c(1:3)]

all_bins <- unique_transects[rep(seq_len(nrow(unique_transects)), each = 16), ]

all_bins$Distance <- rep(seq(from = 5, to = 155, by = 10), 46)

all_bins$n_zeros <- 0

seed_density_distance <- merge(seed_density_distance, all_bins, all.y = T)

seed_density_distance[is.na(seed_density_distance)] <- 0

seed_density_distance$n_zeros <- NULL

## Convert density to ha (100 sq m to 10,000 sq m)
seed_density_distance$Density_ha <- seed_density_distance$n*100

## Bind the plot data with it in preparation for analysis
merging <- Plotdata_HA[, c(1,2,3, 11, 10, 6, 12, 13, 4, 5, 9, 8)]

seed_density_distance <- merge(seed_density_distance, merging, by = c("Fire", "Patch", "Transect"), all.x = T)

colnames(seed_density_distance)[8] <- "Aspect"
colnames(seed_density_distance)[14] <- "Combined_slope"
colnames(seed_density_distance)[9] <- "Tree_height"
colnames(seed_density_distance)[11] <- "Prevailing_wind"
colnames(seed_density_distance)[13] <- "Ponderosa_overstory_density" ## This was chosen using a dredge of all PIPO stand structure variables and this was the most important to seed density (between this, PIPO BA, PIPO density, and PIPO QMD)

seed_density_distance$Prevailing_wind <- ifelse(seed_density_distance$Prevailing_wind>0, seed_density_distance$Prevailing_wind,0)

seed_density_distance$Seedling_density_log <- log(seed_density_distance$Density_ha)

seed_density_distance$binom <- ifelse(seed_density_distance$Seedling_density > 0,1,0)

# Make the binomial models table (Supplemental table S1), but it doesn't contain crossvalidation data
comb_slope <- glm(binom ~ log(Distance) + factor(Aspect) + Combined_slope, data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
comb_slope_treeht <- glm(binom ~ Combined_slope + Tree_height + log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
comb_slope_wind<- glm(binom ~  Combined_slope + Prevailing_wind+ log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
comb_slope_treeht_wind<- glm(binom ~ Combined_slope + Tree_height + Prevailing_wind + log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
treeht_wind<- glm(binom ~  Tree_height + Prevailing_wind+ log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
treeht<- glm(binom ~ Tree_height+ log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
wind<- glm(binom ~ Prevailing_wind+ log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
comb_slope_pipodensity25 <- glm(binom ~  Combined_slope + Ponderosa_overstory_density+ log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
comb_slope_wind_pipodensity25 <- glm(binom ~  Combined_slope + Ponderosa_overstory_density + Prevailing_wind+ log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
pipodensity25 <- glm(binom ~ Ponderosa_overstory_density + log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
pipodensity25_wind <- glm(binom ~ Ponderosa_overstory_density + Prevailing_wind + log(Distance) + factor(Aspect), data=seed_density_distance, family = binomial(),  na.action = "na.fail" )
distance <- glm(binom ~ log(Distance), data=seed_density_distance, family = binomial())

tab_model(comb_slope, treeht, wind, pipodensity25, comb_slope_treeht, comb_slope_wind, comb_slope_pipodensity25, treeht_wind, pipodensity25_wind, 
          comb_slope_treeht_wind, comb_slope_wind_pipodensity25, distance, show.ci = F, show.aicc = T, show.se = T, p.style = "stars",digits = 4,
          dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Logistic Model", "distance"))

summary(distance)



### Now lets crossvalidate the best model:
ctrl <- trainControl(method = "LOOCV")

#### Create training and testing data splits
Train <- createDataPartition(seed_density_distance$binom, p=0.8, list=FALSE)
training <- seed_density_distance[ Train, ]
testing <- seed_density_distance[ -Train, ]

#fit a regression model and use LOOCV to evaluate performance
comb_slope_model <- train(as.factor(binom) ~ log(Distance) + factor(Aspect) + Combined_slope, data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

treeht_model <- train(as.factor(binom) ~  Tree_height
                      + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

wind_model <- train(as.factor(binom) ~  Prevailing_wind
                    + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

pipodensity25_model <- train(as.factor(binom) ~ Ponderosa_overstory_density 
                             + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

comb_slope_treeht_model <- train(as.factor(binom) ~  Combined_slope + Tree_height
                                 + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

comb_slope_wind_model <- train(as.factor(binom) ~  Combined_slope + Prevailing_wind
                               + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

comb_slope_pipodensity25_model <- train(as.factor(binom) ~  Combined_slope + Ponderosa_overstory_density
                                        + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

treeht_wind_model <- train(as.factor(binom) ~  Tree_height + Ponderosa_overstory_density
                           + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

pipodensity25_wind_model <- train(as.factor(binom) ~  Ponderosa_overstory_density + Prevailing_wind
                                  + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

comb_slope_treeht_wind_model <- train(as.factor(binom) ~  Combined_slope + Tree_height + Prevailing_wind
                                      + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

comb_slope_wind_pipodensity25_model <- train(as.factor(binom) ~  Combined_slope + Ponderosa_overstory_density + Prevailing_wind
                                             + log(Distance) + factor(Aspect), data=training, method = 'glmnet', trControl = ctrl, family = 'binomial')

distance_model <- train(as.factor(binom) ~ log(Distance), data=training, method = 'glm', trControl = ctrl, family = 'binomial')


#view summary of LOOCV for Cross validation accuracy               
comb_slope_model_pred <- predict(comb_slope_model, newdata=testing) 
confusionMatrix(data=comb_slope_model_pred, as.factor(testing$binom)) #accuracy = 0.6111

treeht_model_pred <- predict(treeht_model, newdata=testing) 
confusionMatrix(data=treeht_model_pred, as.factor(testing$binom)) #accuracy = 0.6181

wind_model_pred <- predict(wind_model, newdata=testing) 
confusionMatrix(data=wind_model_pred, as.factor(testing$binom)) #accuracy = 0.6111

pipodensity25_model_pred <- predict(pipodensity25_model, newdata=testing) 
confusionMatrix(data=pipodensity25_model_pred, as.factor(testing$binom)) #accuracy = 0.6250

comb_slope_treeht_model_pred <- predict(comb_slope_treeht_model, newdata=testing) 
confusionMatrix(data=comb_slope_treeht_model_pred, as.factor(testing$binom)) #accuracy = 0.6181

comb_slope_wind_model_pred <- predict(comb_slope_wind_model, newdata=testing) 
confusionMatrix(data=comb_slope_wind_model_pred, as.factor(testing$binom)) #accuracy = 0.6250

comb_slope_pipodensity25_model_pred <- predict(comb_slope_pipodensity25_model, newdata=testing) 
confusionMatrix(data=comb_slope_pipodensity25_model_pred, as.factor(testing$binom)) #accuracy = 0.6389

treeht_wind_model_pred <- predict(treeht_wind_model, newdata=testing) 
confusionMatrix(data=treeht_wind_model_pred, as.factor(testing$binom)) #accuracy = 0.6111

pipodensity25_wind_model_pred <- predict(pipodensity25_wind_model, newdata=testing) 
confusionMatrix(data=pipodensity25_wind_model_pred, as.factor(testing$binom)) #accuracy = 0.6528

comb_slope_treeht_wind_model_pred <- predict(comb_slope_treeht_wind_model, newdata=testing) 
confusionMatrix(data=comb_slope_treeht_wind_model_pred, as.factor(testing$binom)) #accuracy = 0.6181

comb_slope_wind_pipodensity25_model_pred <- predict(comb_slope_wind_pipodensity25_model, newdata=testing) 
confusionMatrix(data=comb_slope_wind_pipodensity25_model_pred, as.factor(testing$binom)) #accuracy = 0.6528

dist_model_pred <- predict(distance_model, newdata=testing) 
confusionMatrix(data=dist_model_pred, as.factor(testing$binom)) #accuracy = 0.6667

# Make the GLM table (Supplemental table S2), but it doesn't contain crossvalidation data
seed_density_distance <- seed_density_distance[seed_density_distance$binom == 1,]

comb_slope <- lm(Seedling_density_log ~ Distance + factor(Aspect) + Combined_slope, data=seed_density_distance,  na.action = "na.fail" )
comb_slope_treeht <- lm(Seedling_density_log ~ Combined_slope + Tree_height + Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
comb_slope_wind<- lm(Seedling_density_log ~  Combined_slope + poly(Prevailing_wind,2)+ Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
comb_slope_treeht_wind<- lm(Seedling_density_log ~ Combined_slope + Tree_height + poly(Prevailing_wind,2) + Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
treeht_wind<- lm(Seedling_density_log ~  Tree_height + poly(Prevailing_wind,2)+ Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
treeht<- lm(Seedling_density_log ~ Tree_height+ Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
wind<- lm(Seedling_density_log ~ poly(Prevailing_wind,2)+ Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
comb_slope_pipodensity25 <- lm(Seedling_density_log ~  Combined_slope + poly(Ponderosa_overstory_density,2)+ Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
comb_slope_wind_pipodensity25 <- lm(Seedling_density_log ~  Combined_slope + poly(Ponderosa_overstory_density,2) + poly(Prevailing_wind,2)+ Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
pipodensity25 <- lm(Seedling_density_log ~ poly(Ponderosa_overstory_density,2) + Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )
pipodensity25_wind <- lm(Seedling_density_log ~ poly(Ponderosa_overstory_density,2) + poly(Prevailing_wind,2) + Distance + factor(Aspect), data=seed_density_distance,  na.action = "na.fail" )

tab_model(comb_slope, treeht, wind, pipodensity25, comb_slope_treeht, comb_slope_wind, comb_slope_pipodensity25, treeht_wind, pipodensity25_wind, 
          comb_slope_treeht_wind, comb_slope_wind_pipodensity25, show.ci = F, show.aicc = T, show.se = T, p.style = "stars",digits = 4,
          dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 9", "Model 10", "Abiotic Features Model"))

### Now lets crossvalidate the best model:
ctrl <- trainControl(method = "LOOCV")

#fit a regression model and use LOOCV to evaluate performance
comb_slope_model <- train(Density_ha ~  Combined_slope
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

treeht_model <- train(Density_ha ~  Tree_height
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

wind_model <- train(Density_ha ~  poly(Prevailing_wind,2)
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

pipodensity25_model <- train(Density_ha ~ poly(Ponderosa_overstory_density,2) 
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

comb_slope_treeht_model <- train(Density_ha ~  Combined_slope + Tree_height
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

comb_slope_wind_model <- train(Density_ha ~  Combined_slope + poly(Prevailing_wind,2)
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

comb_slope_pipodensity25_model <- train(Density_ha ~  Combined_slope + poly(Ponderosa_overstory_density,2)
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

treeht_wind_model <- train(Density_ha ~  Tree_height + poly(Ponderosa_overstory_density,2)
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

pipodensity25_wind_model <- train(Density_ha ~  poly(Ponderosa_overstory_density,2) + poly(Prevailing_wind,2)
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

comb_slope_treeht_wind_model <- train(Density_ha ~  Combined_slope + Tree_height + poly(Prevailing_wind,2)
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

comb_slope_wind_pipodensity25_model <- train(Density_ha ~  Combined_slope + poly(Ponderosa_overstory_density,2) + poly(Prevailing_wind,2)
                                           + Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

distance_model <- train(Density_ha ~  Distance + factor(Aspect), data=seed_density_distance, method = "lm", trControl = ctrl)

#view summary of LOOCV for Cross validation RMSE               
print(comb_slope_model) #1852.643	
print(treeht_model) #1846.711
print(wind_model) #1812.049
print(pipodensity25_model) #1602.531
print(comb_slope_treeht_model) #1836.647
print(comb_slope_wind_model) #1794.884
print(comb_slope_pipodensity25_model) #1600.137
print(treeht_wind_model) #1601.876
print(pipodensity25_wind_model) #1586.978
print(comb_slope_treeht_wind_model) #1781.992
print(comb_slope_wind_pipodensity25_model) #1582.588
print(distance_model) #1856.556

# Part 2 
## quantification of values in table 1 
### Overall density and transect counts (bottom right values)
Plotdata_HA %>% summarise(mean(TransDensity),sd(TransDensity)/(sqrt(n())), max(TransDensity), min(TransDensity))
Plotdata_HA %>% tally()

### Overall by aspect (right values)
Plotdata_HA %>% group_by(CardDirec) %>% summarise(mean(TransDensity),sd(TransDensity)/(sqrt(n())))
Plotdata_HA %>% group_by(CardDirec) %>% tally()

### Overall by slope (bottom values)
Plotdata_HA %>% group_by(Slope_position) %>% summarise(mean(TransDensity),sd(TransDensity)/(sqrt(n())))
Plotdata_HA %>% group_by(Slope_position) %>% tally()

### By slope and aspect (middle of the table)
Plotdata_HA %>% group_by(CardDirec, Slope_position) %>% summarise(mean(TransDensity),sd(TransDensity)/(sqrt(n())))
Plotdata_HA %>% group_by(CardDirec, Slope_position) %>% tally()

## supplemental table s2 - stand structure. Height was calculated from a different spreadsheet specific for all tree heights 
Plotdata_HA %>% summarise(mean(NestedBA),sd(NestedBA)/(sqrt(n())), max(NestedBA), min(NestedBA))
Plotdata_HA %>% summarise(mean(NestedDensity),sd(NestedDensity)/(sqrt(n())), max(NestedDensity), min(NestedDensity))
Plotdata_HA %>% summarise(mean(OverstoryDensity_25),sd(OverstoryDensity_25)/(sqrt(n())), max(OverstoryDensity_25), min(OverstoryDensity_25))
Plotdata_HA %>% summarise(mean(PIPOBA),sd(PIPOBA)/(sqrt(n())), max(PIPOBA), min(PIPOBA))
Plotdata_HA %>% summarise(mean(PIPODensity),sd(PIPODensity)/(sqrt(n())), max(PIPODensity), min(PIPODensity))
Plotdata_HA %>% summarise(mean(OverstoryPIPODensity_25),sd(OverstoryPIPODensity_25)/(sqrt(n())), max(OverstoryPIPODensity_25), min(OverstoryPIPODensity_25))

## Results 3.1 text values
### Slope degrees
Plotdata_HA %>% group_by(Slope_position) %>% summarise(mean(Slope), sd(Slope)/(sqrt(n())))

### Transect density by wind position
Plotdata_HA %>% group_by(Direction) %>% summarise(mean(TransDensity), sd(TransDensity)/(sqrt(n())))

## Results 3.2 text values
### Just to note, this dataframe is already subsetted for pixels within patches > 1ha in size
seedling_density_df <- read.csv("seedling_presence_density_fullmodel_distancemodel_1-8-24.csv")

names(seedling_density_df)[names(seedling_density_df) == 'Aspct'] <- 'Aspect'

seedling_density_df$aspect_card <- NA
seedling_density_df$aspect_card[seedling_density_df$Aspect <= 45 | seedling_density_df$Aspect > 315] <- "North"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 45 & seedling_density_df$Aspect <= 135] <- "East"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 135 & seedling_density_df$Aspect <= 225] <- "South"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 225 & seedling_density_df$Aspect <= 315] <- "West"

seedling_density_df$Distance_only_density <- ifelse(seedling_density_df$seedling_presence_distance > 0.4, seedling_density_df$seedling_density_distance, 0)
seedling_density_df$Full_model_density <- ifelse(seedling_density_df$seedling_presence_windslope > 0.4, seedling_density_df$seedling_density_windslope, 0)


seedling_density_df$density_diff <- seedling_density_df$Full_model_density - seedling_density_df$Distance_only_density

seedling_density_df$slope_pos <- ifelse(seedling_density_df$slope<0, "downslope", "upslope")
seedling_density_df$wind_pos <- ifelse(seedling_density_df$wnd_0>0, "downwind", "upwind")

### Calculate the proportion of area where the full model > distance only model estimates
#### all pixels on northfacing aspects 
seedling_density_df_north <- seedling_density_df[seedling_density_df$aspect_card == "North",]

tally(seedling_density_df_north[seedling_density_df_north$density_diff > 0,])/length(seedling_density_df_north$Full_model_density)

### mean difference between full and distance only estimates
mean(seedling_density_df_north$density_diff)

#### maximum increase in regen for areas downwind of the primary seed source 
seedling_density_df_north_down <- seedling_density_df_north %>%  filter(wind_pos == "downwind") 

max(seedling_density_df_north_down$density_diff)

#### Proportion of areas with more regen that were downwind of intact forest
tally(seedling_density_df_north[seedling_density_df_north$density_diff > 0 & seedling_density_df_north$wind_pos == "downwind",])/tally(seedling_density_df_north[seedling_density_df_north$density_diff > 0,])

tally(seedling_density_df_north[seedling_density_df_north$density_diff > 0 & seedling_density_df_north$slope_pos == "downslope",])/tally(seedling_density_df_north[seedling_density_df_north$density_diff > 0,])

### average regen difference and SE in areas where the abiotic features model had less regeneration 
mean(seedling_density_df_north[seedling_density_df_north$density_diff < 0,]$density_diff)
sd(seedling_density_df_north[seedling_density_df_north$density_diff < 0,]$density_diff)/
  sqrt(tally(seedling_density_df_north[seedling_density_df_north$density_diff < 0,]))

## counts for each panel in figure 4
seedling_density_df %>% group_by(wind_slope) %>% tally




