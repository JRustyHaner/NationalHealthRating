#A Comparison of Mental Health Statistics in the United States regarding Overall Health

#Install Libraries
install.packages("corrplot")
install.packages("gridExtra")

#Load Library for Maps
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(dplyr)
library(corrplot)
library(gridExtra)

#Functions


#Set Working Directory
setwd("~/MIS7390/Project/")

#load data
df <- read.csv("source.csv")

#view summary
summary(df)

#list names of dataframe
names(df)

#define kept columns
colKeep <- c(1,2,3,4,11,12,13,16,17,19)

#remove duplicates
df <- unique(df[,])

#prune columns
df <- df[colKeep]

#summary of new dataframe
summary(df)

#omit all rows containing missing data
df <- na.omit(df)

#add columns for new data
df$HealthRank <- NA
df$X..Excessive.Drinking.Rank <-NA
df$X..Unemployed.Rank <- NA
df$Median.Household.Income.Rank <- NA
df$X..Mental.Health.Providers.Rank <- NA
df$MHP.Rate.Rank <- NA
df$X..Insufficient.Sleep.Rank <- NA

#Programmatically Add Ranks
df$HealthRank[order(df$X..Fair.Poor)] <- 1:nrow(df)
df$X..Excessive.Drinking.Rank[order(df$X..Excessive.Drinking)] <- 1:nrow(df)
df$X..Unemployed.Rank[order(df$X..Unemployed)] <- 1:nrow(df)
df$Median.Household.Income.Rank[order(df$Median.Household.Income)] <- 1:nrow(df)
df$X..Mental.Health.Providers.Rank[order(df$X..Mental.Health.Providers)] <- 1:nrow(df)
df$MHP.Rate.Rank[order(df$MHP.Rate)] <- 1:nrow(df)
df$X..Insufficient.Sleep.Rank[order(df$X..Insufficient.Sleep)] <- 1:nrow(df)

colnames(df)
#aggregate state stats
stateAgg <- aggregate(df[1+3:16],by=list(df$State), FUN=mean)
colnames(stateAgg)[1] <- "State"


#add county ranks columns
df$HealthRank <- NA
df$X..Excessive.Drinking.Rank <- NA
df$X..Unemployed.Rank <- NA
df$Median.Household.Income.Rank <- NA
df$X..Mental.Health.Providers.Rank <- NA
df$MHP.Rate.Rank <- NA

#add state ranks columns
stateAgg$HealthRank <- NA
stateAgg$X..Excessive.Drinking.Rank <- NA
stateAgg$X..Unemployed.Rank <- NA
stateAgg$Median.Household.Income.Rank <- NA
stateAgg$X..Mental.Health.Providers.Rank <- NA
stateAgg$MHP.Rate.Rank <- NA
stateAgg$X..Insufficient.Sleep.Rank <-NA

#populate county ranks
df$HealthRank[order(df$X..Fair.Poor)] <- 1:nrow(df)
df$X..Excessive.Drinking.Rank[order(df$X..Excessive.Drinking)] <- 1:nrow(df)
df$X..Unemployed.Rank[order(df$X..Unemployed)] <- 1:nrow(df)
df$Median.Household.Income.Rank[order(df$Median.Household.Income)] <- 1:nrow(df)
df$X..Mental.Health.Providers.Rank[order(df$X..Mental.Health.Providers)] <- 1:nrow(df)
df$MHP.Rate.Rank[order(df$MHP.Rate)] <- 1:nrow(df)
df$X..Insufficient.Sleep.Rank[order(df$X..Insufficient.Sleep)]  <- 1:nrow(df)

#populate state ranks
stateAgg$HealthRank[order(stateAgg$X..Fair.Poor)] <- 1:nrow(stateAgg)
stateAgg$X..Excessive.Drinking.Rank[order(stateAgg$X..Excessive.Drinking)] <- 1:nrow(stateAgg)
stateAgg$X..Unemployed.Rank[order(stateAgg$X..Unemployed)] <- 1:nrow(stateAgg)
stateAgg$Median.Household.Income.Rank[order(stateAgg$Median.Household.Income)] <- 1:nrow(stateAgg)
stateAgg$X..Mental.Health.Providers.Rank[order(stateAgg$X..Mental.Health.Providers)] <- 1:nrow(stateAgg)
stateAgg$MHP.Rate.Rank[order(stateAgg$MHP.Rate)] <- 1:nrow(stateAgg)
stateAgg$X..Insufficient.Sleep.Rank[order(stateAgg$X..Insufficient.Sleep)] <- 1:nrow(stateAgg)

#Set Up DataFrame for Correlation Data
corData <- data.frame("Index" = 1:1)
corData$Health.Drinking <- NA
corData$Health.Income <- NA
corData$Health.Unemployed <- NA
corData$Health.MentalHealtProviders <- NA
corData$Health.RatioMHP <- NA
corData$MHP.Income <- NA
corData$MHP.Unemployed <- NA
corData$MHP.Drinking <- NA
corData$InsuffecientSleep <- NA

#Calculate Correlation of Poor Health to Excessive Drinking (-0.70)
plot(stateAgg$X..Fair.Poor,stateAgg$X..Excessive.Drinking)
corData$Health.Drinking <-  cor(stateAgg$X..Fair.Poor,stateAgg$X..Excessive.Drinking)

#calculate Correlation of Poor Health to Median Income (-0.74)
plot(stateAgg$X..Fair.Poor,stateAgg$Median.Household.Income) 
corData$Health.Income <- cor(stateAgg$X..Fair.Poor,stateAgg$Median.Household.Income)

#calculate Correlation of Poor Health to Unemployment (0.625)
plot(stateAgg$X..Fair.Poor,stateAgg$X..Unemployed) 
corData$Health.Unemployed <- cor(stateAgg$X..Fair.Poor,stateAgg$X..Unemployed)

#calculate Correlation of Poor Health to Available Mental Health Providers (-0.31)
plot(stateAgg$X..Fair.Poor,stateAgg$X..Mental.Health.Providers)
corData$Health.MentalHealtProviders <- cor(stateAgg$X..Fair.Poor,stateAgg$X..Mental.Health.Providers)

#calculate Correlation of Poor Health to Ratio of Mental Health Providers (-0.35)
plot(stateAgg$X..Fair.Poor,stateAgg$MHP.Rate)
corData$Health.RatioMHP <- cor(stateAgg$X..Fair.Poor,stateAgg$MHP.Rate)

#calculate Correlation of Mental Health Providers to Median Income (0.59)
plot(stateAgg$X..Mental.Health.Providers,stateAgg$Median.Household.Income)
corData$MHP.Income <- cor(stateAgg$X..Mental.Health.Providers,stateAgg$Median.Household.Income)

#calculate Correlation of Mental Health Providers to Unemployment (-0.02)
plot(stateAgg$X..Mental.Health.Providers,stateAgg$X..Unemployed)
corData$MHP.Unemployed <- cor(stateAgg$X..Mental.Health.Providers,stateAgg$X..Unemployed)

#calculate Correlation of Mental Health Providers to Excessive Drinking(0.25)
plot(stateAgg$X..Mental.Health.Providers,stateAgg$X..Excessive.Drinking)
corData$MHP.Drinking <- cor(stateAgg$X..Mental.Health.Providers,stateAgg$X..Excessive.Drinking)

#calculate Correlation of Mental Health Providers to Excessive Drinking(0.25)
plot(stateAgg$X..Mental.Health.Providers,stateAgg$X..Excessive.Drinking)
corData$MHP.Drinking <- cor(stateAgg$X..Mental.Health.Providers,stateAgg$X..Excessive.Drinking)

#calculate Correlation of Mental Health Providers to Insuffiecient Sleep
plot(stateAgg$X..Mental.Health.Providers,stateAgg$X..Insufficient.Sleep)
corData$MHP.Drinking <- cor(stateAgg$X..Mental.Health.Providers,stateAgg$X..Insufficient.Sleep)



colnames(df)
#Correlation Plots (All Variables)
dfStripped <- df[,4:10]
corDf <- cor(na.omit(dfStripped))
corrplot(as.matrix(corDf), method="square")

#Correlation Plots (My Variables)
stateAggStripped <- stateAgg[,2:ncol(stateAgg)]
corStateAgg <- cor(stateAggStripped)
corrplot(as.matrix(corStateAgg), method="square")

#Calculate Means from County Data
meanHealth <- mean(df$X..Fair.Poor)
meanMHP <- mean(df$X..Mental.Health.Providers)
meanExcessiveDrinking <- mean(df$X..Excessive.Drinking)
meanIncome <- mean(df$Median.Household.Income)
meanUnemployed <- mean(df$X..Unemployed)
meanMHPRate <- mean(df$MHP.Rate)

#Calculate Maximums from County Data
maxHealth <- max(df$X..Fair.Poor)
maxMHP <- max(df$X..Mental.Health.Providers)
maxExcessiveDrinking <- max(df$X..Excessive.Drinking)
maxIncome <- max(df$Median.Household.Income)
maxUnemployed <- max(df$X..Unemployed)
maxMHPRate <- max(df$MHP.Rate)

#Plot Table of Correlations
grid.table(corData)

#Plot states with greater than mean Mental Health Providers


#Add catagorical and percentage fields

df$healthCatagory <- NA
df$healthPCT <- df$X..Fair.Poor / maxHealth
for(i in 1:nrow(df)){
  if(df$X..Fair.Poor[i] > meanHealth){
    df[i,which(colnames(df)=="healthCatagory")] <- "High"
  } else {
    df[i,which(colnames(df)=="healthCatagory")] <- "Low"
  }
}

  
  
df$excessiveDrinkingCatagory <- NA
df$excessiveDrinkingPCT <- df$X..Excessive.Drinking / maxExcessiveDrinking
for(i in 1:nrow(df)){
  if(df$X..Excessive.Drinking[i] > meanExcessiveDrinking){
    df[i,which(colnames(df)=="excessiveDrinkingCatagory")] <- "High"
  } else {
    df[i,which(colnames(df)=="excessiveDrinkingCatagory")] <- "Low"
  }
}

df$UnemployedCatagory <- NA
df$UnemployedPCT <- df$X..Unemployed / maxUnemployed
for(i in 1:nrow(df)){
  if(df$X..Unemployed[i] > meanUnemployed){
    df[i,which(colnames(df)=="UnemployedCatagory")] <- "High"
  } else {
    df[i,which(colnames(df)=="UnemployedCatagory")] <- "Low"
  }
}

df$medianIncomeCatagory <- NA
df$Median.Household.IncomePCT <- df$Median.Household.Income / maxIncome
for(i in 1:nrow(df)){
  if(df$Median.Household.Income[i] > meanIncome){
    df[i,which(colnames(df)=="medianIncomeCatagory")] <- "High"
  } else {
    df[i,which(colnames(df)=="medianIncomeCatagory")] <- "Low"
  }
}
 
df$meanMHPCatagory <- NA
df$X..Mental.Health.ProvidersPCT <- df$X..Mental.Health.Providers / maxMHP
for(i in 1:nrow(df)){
  if(df$X..Mental.Health.Providers[i] > meanMHP){
    df[i,which(colnames(df)=="meanMHPCatagory")] <- "High"
  } else {
    df[i,which(colnames(df)=="meanMHPCatagory")] <- "Low"
  }
}

#Combination Catagories

#Counties that report lower than Average Health and Excessive Drinking
dfCatagorize <- subset(df, df$X..Insufficient.Sleep > mean(df$X..Insufficient.Sleep) & df$X..Mental.Health.Providers > newMeanMHP)
df$MapCat <- "False"
for(i in 1:nrow(df)){
  if(df$X..Mental.Health.Providers > newMeanMHP & df$X..Excessive.Drinking > mean(df$X..Excessive.Drinking)){
    df$MapCat[i] <- "True"
  }  
}

dfCatagorize[,1:3]
str(dfCatagorize)
mean(dfCatagorize$X..Fair.Poor)

#Prune Mental Health Providers
dfPruned <- subset(df, df$subregion == "El Paso")
newMeanMHP <- mean(dfPruned$X..Mental.Health.Providers)
dfPruned <- subset(df, df$X..Mental.Health.Providers <  newMeanMHP)

# Plot the counties
# Load the county data from the maps package
data(county.fips)
cnty <- map_data("county")
cnty2 <- cnty %>%
  mutate(polyname = paste(region,subregion,sep=",")) %>%
  left_join(county.fips, by="polyname")


colnames(df)[1] <- "fips"
colnames(df)[2] <- "region"
colnames(df)[3] <- "subregion"
lapply(df$subregion, FUN = tolower)
dfMap <- inner_join(df,cnty2, by="fips")
ggplot(dfMap, aes(long, lat,group = group)) + 
  geom_polygon(aes(fill = healthCatagory), colour = rgb(0,0,0,0.2))  +
  coord_quickmap()

mean(dfCatagorize$X..Fair.Poor)
