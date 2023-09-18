
# Bus Stop Amenities

## Set up workspace

rm(list = ls()) ### clear environment

library(tidyverse)
library(here)
library(vtable)


readRenviron("~/.Renviron")


# Import 2022 bus stop amenity data -----

BusStopAmenityData22 <- read_csv(here("Data", "BusStopAmenityData2022-pax.csv"))

summary(BusStopAmenityData22)


# Prepare data -----

## filter out excluded data 

CleanBusStopData22 <- filter(BusStopAmenityData22, Exclude == "No")


## Change data to numeric as needed
CleanBusStopData22$RoutesCount <- as.numeric(CleanBusStopData22$RoutesCount)
CleanBusStopData22$WeeklyTrips <- as.numeric(CleanBusStopData22$WeeklyTrips)

## code categorical variables 
CleanBusStopData22$Nearside <-ifelse(CleanBusStopData22$Location=="Near side",1,0)
CleanBusStopData22$Farside <-ifelse(CleanBusStopData22$Location=="Far side",1,0)
CleanBusStopData22$SharedStop <-ifelse(CleanBusStopData22$Shared=="Yes",1,0)
CleanBusStopData22$City <-ifelse(CleanBusStopData22$CityCounty=="City",1,0)
CleanBusStopData22$StateRoad <-ifelse(CleanBusStopData22$RoadOwner=="state",1,0)
CleanBusStopData22$LocalRoad <-ifelse(CleanBusStopData22$RoadOwner=="local",1,0)

summary(CleanBusStopData22)

### Get rid of NAs
CleanBusStopData22$Nearside[is.na(CleanBusStopData22$Nearside)] <- 0
CleanBusStopData22$Farside[is.na(CleanBusStopData22$Farside)] <- 0

## Test categorical variable combinations
xtabs(~ Seating + Nearside, data=CleanBusStopData22)
xtabs(~ Seating + Farside, data=CleanBusStopData22)
xtabs(~ Seating + SharedStop, data=CleanBusStopData22)
xtabs(~ Seating + City, data=CleanBusStopData22)
xtabs(~ Seating + StateRoad, data=CleanBusStopData22)
xtabs(~ Seating + LocalRoad, data=CleanBusStopData22)
xtabs(~ Seating + Residential, data=CleanBusStopData22)
xtabs(~ Seating + Retail, data=CleanBusStopData22)
xtabs(~ Seating + Office, data=CleanBusStopData22) ### Sample size might be too small
xtabs(~ Seating + School, data=CleanBusStopData) ### Sample size might be too small

xtabs(~ Shelter + Nearside, data=CleanBusStopData22)
xtabs(~ Shelter + Farside, data=CleanBusStopData22)
xtabs(~ Shelter + SharedStop, data=CleanBusStopData22)
xtabs(~ Shelter + City, data=CleanBusStopData22)
xtabs(~ Shelter + StateRoad, data=CleanBusStopData22)
xtabs(~ Shelter + LocalRoad, data=CleanBusStopData22)
xtabs(~ Shelter + Residential, data=CleanBusStopData22)
xtabs(~ Shelter + Retail, data=CleanBusStopData22)
xtabs(~ Shelter + Office, data=CleanBusStopData22) ### Sample size might be too small
xtabs(~ Shelter + School, data=CleanBusStopData22) ### Sample size might be too small

## Check to see if log transformations are needed
hist(CleanBusStopData22$TotalRidership) #### Looks like a log-normal distribution
CleanBusStopData22$LogRidership <- log(CleanBusStopData22$TotalRidership)
hist(CleanBusStopData22$LogRidership)

hist(CleanBusStopData22$RoutesCount)
CleanBusStopData22$LogRoutes <- log(CleanBusStopData22$RoutesCount)
hist(CleanBusStopData22$LogRoutes) ## Inclusion of routes might not be viable

hist(CleanBusStopData22$WeeklyTrips)
CleanBusStopData22$LogTrips <- log(CleanBusStopData22$WeeklyTrips)
hist(CleanBusStopData22$LogTrips)

hist(CleanBusStopData22$Population)
CleanBusStopData22$LogPop <- log(CleanBusStopData22$Population)
hist(CleanBusStopData22$LogPop)

hist(CleanBusStopData22$PctZeroCar)
CleanBusStopData22$LogZeroCar <- log(0.01 + CleanBusStopData22$PctZeroCar) ## Percent zero cars has a zero value
hist(CleanBusStopData22$LogZeroCar)


hist(CleanBusStopData22$PctTransit)
CleanBusStopData22$LogTransitMode <- log(CleanBusStopData22$PctTransit)
hist(CleanBusStopData22$LogTransitMode)

hist(CleanBusStopData$PctNonWhite)

hist(CleanBusStopData22$PctLowIncome)
CleanBusStopData22$LogLowIncome <- log(CleanBusStopData22$PctLowIncome)
hist(CleanBusStopData22$LogLowIncome)

hist(CleanBusStopData22$EMP)
CleanBusStopData22$LogJobs <- log(CleanBusStopData22$EMP)
hist(CleanBusStopData22$LogJobs)


# Correlation Matrix -----
library(reshape2)

## Keep variables of interest

#### Remove PctOwnerOccupied because of correlations with transport metrics
#### Remove NOVA because of correlations with density and vehicle ownership

CorrelationData <- CleanBusStopData22 %>%
  dplyr::select(Shelter,
                Seating,
                Nearside,
                Farside,
                SharedStop,
                City,
                StateRoad,
                LocalRoad,
                Residential,
                Retail,
                Office,
                School,
                TotalRidership,
                RoutesCount,
                WeeklyTrips,
                Population,
                PctZeroCar,
                PctTransit,
                PctNonWhite,
                PctLowIncome,
                EMP)


#Correlation Matrix
CorMatrix <- round(cor(CorrelationData),2)
head(CorMatrix) #### Returns the first or last parts of a vector, matrix, table, data frame or function

#### Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(CorMatrix) #### Save data for upper triangle
upper_tri

### Convert an object into a molten data frame.
melted_Cor <- melt(upper_tri, na.rm=TRUE)

## Heatmap
ggheatmap <- ggplot(melted_Cor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

### Print the heatmap
print(ggheatmap)

### Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


### Local road is moderately correlated with cities and state road; routes has moderate-high correlation with trips


## Convert relevant columns into factors
CleanBusStopData22$Shelter <-as.factor(CleanBusStopData22$Shelter)
CleanBusStopData22$Seating <-as.factor(CleanBusStopData22$Seating)
CleanBusStopData22$Nearside <-as.factor(CleanBusStopData22$Nearside)
CleanBusStopData22$Farside <-as.factor(CleanBusStopData22$Farside)
CleanBusStopData22$SharedStop <-as.factor(CleanBusStopData22$SharedStop)
CleanBusStopData22$City <-as.factor(CleanBusStopData22$City)
CleanBusStopData22$StateRoad <-as.factor(CleanBusStopData22$StateRoad)
CleanBusStopData22$LocalRoad <-as.factor(CleanBusStopData22$LocalRoad)
CleanBusStopData22$Residential <-as.factor(CleanBusStopData22$Residential)
CleanBusStopData22$Retail <-as.factor(CleanBusStopData22$Retail)
CleanBusStopData22$Office <-as.factor(CleanBusStopData22$Office)
CleanBusStopData22$School <-as.factor(CleanBusStopData22$School)



# Logit Regression Model 1 - Likelihood bus stop has seating  -----
library(car)

SeatingGLM <- glm(Seating ~ LogRidership +
                    LogTrips +
                    LogPop +
                    LogZeroCar + 
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    Nearside +
                    Farside +
                    SharedStop +
                    City +
                    StateRoad +
                    LocalRoad +
                    Residential +
                    Retail + 
                    Office,
                          family = "binomial",
                          data = CleanBusStopData22)

SeatingGLM <- glm(Seating ~ LogRidership +
                    LogTrips +
                    LogPop +
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    SharedStop +
                    City +
                    LocalRoad +
                    Residential +
                    Retail,
                 family = "binomial",
                 data = CleanBusStopData22)

summary(SeatingGLM)


vif(SeatingGLM)


## Logit model results
### Relative risk ratios
logit.or = exp(coef(SeatingGLM))

library(stargazer)

stargazer(SeatingGLM, type="text", coef=list(logit.or), p.auto=FALSE, out="SeatingGLM2022.htm")


## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(SeatingGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- SeatingGLM$null.deviance/-2
Transport.ll.proposed <- SeatingGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.3132161 

## P-value
1 - pchisq(2*(Transport.ll.proposed - Transport.ll.null), df=(length(SeatingGLM$coefficients)-1)) 
### P = 7.233311e-09




# Logit Regression Model 2 - Likelihood bus stop has shelter  -----
library(car)

ShelterGLM <- glm(Shelter ~ LogRidership +
                    LogTrips +
                    LogPop +
                    LogZeroCar + 
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    Nearside +
                    Farside +
                    SharedStop +
                    City +
                    StateRoad +
                    LocalRoad +
                    Residential +
                    Retail + 
                    Office,
                  family = "binomial",
                  data = CleanBusStopData22)

ShelterGLM <- glm(Shelter ~ LogRidership +
                    LogTrips +
                    LogPop +
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    SharedStop +
                    City +
                    LocalRoad +
                    Residential +
                    Retail,
                  family = "binomial",
                  data = CleanBusStopData22)

summary(ShelterGLM)


vif(ShelterGLM)


## Logit model results
### Relative risk ratios
logit.or = exp(coef(ShelterGLM))

library(stargazer)

stargazer(ShelterGLM, type="text", coef=list(logit.or), p.auto=FALSE, out="ShelterGLM2022.htm")


## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(ShelterGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- ShelterGLM$null.deviance/-2
Transport.ll.proposed <- ShelterGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.2795514 

## P-value
1 - pchisq(2*(Transport.ll.proposed - Transport.ll.null), df=(length(ShelterGLM$coefficients)-1)) 
### P = 1.396044e-06





# Import 2018 bus stop amenity data -----

BusStopAmenityData18 <- read_csv(here("Data", "BusStopAmenityData2018.csv"))

summary(BusStopAmenityData18)


# Prepare data -----

## filter out excluded data 

CleanBusStopData18 <- filter(BusStopAmenityData18, Exclude == "No")


## Change data to numeric as needed
CleanBusStopData18$RoutesCount <- as.numeric(CleanBusStopData18$RoutesCount)
CleanBusStopData18$WeeklyTrips <- as.numeric(CleanBusStopData18$WeeklyTrips)

## code categorical variables 
CleanBusStopData18$Nearside <-ifelse(CleanBusStopData18$Location=="Near side",1,0)
CleanBusStopData18$Farside <-ifelse(CleanBusStopData18$Location=="Far side",1,0)
CleanBusStopData18$SharedStop <-ifelse(CleanBusStopData18$Shared=="Yes",1,0)
CleanBusStopData18$City <-ifelse(CleanBusStopData18$CityCounty=="City",1,0)
CleanBusStopData18$StateRoad <-ifelse(CleanBusStopData18$RoadOwner=="state",1,0)
CleanBusStopData18$LocalRoad <-ifelse(CleanBusStopData18$RoadOwner=="local",1,0)

summary(CleanBusStopData18)

### Get rid of NAs
CleanBusStopData18$Nearside[is.na(CleanBusStopData18$Nearside)] <- 0
CleanBusStopData18$Farside[is.na(CleanBusStopData18$Farside)] <- 0

## Test categorical variable combinations
xtabs(~ Seating + Nearside, data=CleanBusStopData18)
xtabs(~ Seating + Farside, data=CleanBusStopData18)
xtabs(~ Seating + SharedStop, data=CleanBusStopData18)
xtabs(~ Seating + City, data=CleanBusStopData18)
xtabs(~ Seating + StateRoad, data=CleanBusStopData18)
xtabs(~ Seating + LocalRoad, data=CleanBusStopData18)
xtabs(~ Seating + Residential, data=CleanBusStopData18)
xtabs(~ Seating + Retail, data=CleanBusStopData18)
xtabs(~ Seating + Office, data=CleanBusStopData18) 
xtabs(~ Seating + School, data=CleanBusStopData18) ### Sample size might be too small

xtabs(~ Shelter + Nearside, data=CleanBusStopData18)
xtabs(~ Shelter + Farside, data=CleanBusStopData18)
xtabs(~ Shelter + SharedStop, data=CleanBusStopData18)
xtabs(~ Shelter + City, data=CleanBusStopData18)
xtabs(~ Shelter + StateRoad, data=CleanBusStopData18)
xtabs(~ Shelter + LocalRoad, data=CleanBusStopData18)
xtabs(~ Shelter + Residential, data=CleanBusStopData18)
xtabs(~ Shelter + Retail, data=CleanBusStopData18)
xtabs(~ Shelter + Office, data=CleanBusStopData18) ### Sample size might be too small
xtabs(~ Shelter + School, data=CleanBusStopData18) ### Sample size might be too small

## Check to see if log transformations are needed
hist(CleanBusStopData18$TotalRidership) #### Looks like a log-normal distribution
CleanBusStopData18$LogRidership <- log(CleanBusStopData18$TotalRidership)
hist(CleanBusStopData18$LogRidership)

hist(CleanBusStopData18$RoutesCount)
CleanBusStopData18$LogRoutes <- log(CleanBusStopData18$RoutesCount)
hist(CleanBusStopData18$LogRoutes) ## Inclusion of routes might not be viable

hist(CleanBusStopData18$WeeklyTrips)
CleanBusStopData18$LogTrips <- log(CleanBusStopData18$WeeklyTrips)
hist(CleanBusStopData18$LogTrips)

hist(CleanBusStopData18$Population)
CleanBusStopData18$LogPop <- log(CleanBusStopData18$Population)
hist(CleanBusStopData18$LogPop)

hist(CleanBusStopData18$PctZeroCar)
CleanBusStopData18$LogZeroCar <- log(0.01 + CleanBusStopData18$PctZeroCar) ## Percent zero cars has a zero value
hist(CleanBusStopData18$LogZeroCar)

hist(CleanBusStopData18$PctTransit)
CleanBusStopData18$LogTransitMode <- log(CleanBusStopData18$PctTransit + 0.01)
hist(CleanBusStopData18$LogTransitMode)

hist(CleanBusStopData18$PctNonWhite)

hist(CleanBusStopData18$PctLowIncome)
CleanBusStopData18$LogLowIncome <- log(CleanBusStopData18$PctLowIncome + 0.01)
hist(CleanBusStopData18$LogLowIncome)

hist(CleanBusStopData18$EMP)
CleanBusStopData18$LogJobs <- log(CleanBusStopData18$EMP)
hist(CleanBusStopData18$LogJobs)

summary(CleanBusStopData18)


# Correlation Matrix -----
library(reshape2)

## Keep variables of interest

#### Remove PctOwnerOccupied because of correlations with transport metrics
#### Remove NOVA because of correlations with density and vehicle ownership

CorrelationData <- CleanBusStopData18 %>%
  dplyr::select(Shelter,
                Seating,
                Nearside,
                Farside,
                SharedStop,
                City,
                StateRoad,
                LocalRoad,
                Residential,
                Retail,
                Office,
                School,
                TotalRidership,
                RoutesCount,
                WeeklyTrips,
                Population,
                PctZeroCar,
                PctTransit,
                PctNonWhite,
                PctLowIncome,
                EMP)


#Correlation Matrix
CorMatrix <- round(cor(CorrelationData),2)
head(CorMatrix) #### Returns the first or last parts of a vector, matrix, table, data frame or function

#### Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(CorMatrix) #### Save data for upper triangle
upper_tri

### Convert an object into a molten data frame.
melted_Cor <- melt(upper_tri, na.rm=TRUE)

## Heatmap
ggheatmap <- ggplot(melted_Cor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

### Print the heatmap
print(ggheatmap)

### Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


### Local road is moderately correlated with cities and state road; routes has moderate-high correlation with trips


## Convert relevant columns into factors
CleanBusStopData18$Shelter <-as.factor(CleanBusStopData18$Shelter)
CleanBusStopData18$Seating <-as.factor(CleanBusStopData18$Seating)
CleanBusStopData18$Nearside <-as.factor(CleanBusStopData18$Nearside)
CleanBusStopData18$Farside <-as.factor(CleanBusStopData18$Farside)
CleanBusStopData18$SharedStop <-as.factor(CleanBusStopData18$SharedStop)
CleanBusStopData18$City <-as.factor(CleanBusStopData18$City)
CleanBusStopData18$StateRoad <-as.factor(CleanBusStopData18$StateRoad)
CleanBusStopData18$LocalRoad <-as.factor(CleanBusStopData18$LocalRoad)
CleanBusStopData18$Residential <-as.factor(CleanBusStopData18$Residential)
CleanBusStopData18$Retail <-as.factor(CleanBusStopData18$Retail)
CleanBusStopData18$Office <-as.factor(CleanBusStopData18$Office)
CleanBusStopData18$School <-as.factor(CleanBusStopData18$School)



# Logit Regression Model 3 - Likelihood bus stop has seating  -----
library(car)

SeatingGLM <- glm(Seating ~ LogRidership +
                    LogTrips +
                    LogPop +
                    LogZeroCar + 
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    Nearside +
                    Farside +
                    SharedStop +
                    City +
                    StateRoad +
                    LocalRoad +
                    Residential +
                    Retail + 
                    Office,
                  family = "binomial",
                  data = CleanBusStopData18)

SeatingGLM <- glm(Seating ~ LogRidership +
                    LogTrips +
                    LogPop +
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    SharedStop +
                    City +
                    LocalRoad +
                    Residential +
                    Retail,
                  family = "binomial",
                  data = CleanBusStopData18)

summary(SeatingGLM)


vif(SeatingGLM)


## Logit model results
### Relative risk ratios
logit.or = exp(coef(SeatingGLM))

library(stargazer)

stargazer(SeatingGLM, type="text", coef=list(logit.or), p.auto=FALSE, out="SeatingGLM2018.htm")


## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(SeatingGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- SeatingGLM$null.deviance/-2
Transport.ll.proposed <- SeatingGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.2312758

## P-value
1 - pchisq(2*(Transport.ll.proposed - Transport.ll.null), df=(length(SeatingGLM$coefficients)-1)) 
### P = 3.205569e-05




# Logit Regression Model 4 - Likelihood bus stop has shelter  -----
library(car)

ShelterGLM <- glm(Shelter ~ LogRidership +
                    LogTrips +
                    LogPop +
                    LogZeroCar + 
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    Nearside +
                    Farside +
                    SharedStop +
                    City +
                    StateRoad +
                    LocalRoad +
                    Residential +
                    Retail + 
                    Office,
                  family = "binomial",
                  data = CleanBusStopData18)

ShelterGLM <- glm(Shelter ~ LogRidership +
                    LogTrips +
                    LogPop +
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    SharedStop +
                    City +
                    LocalRoad +
                    Residential +
                    Retail,
                  family = "binomial",
                  data = CleanBusStopData18)

summary(ShelterGLM)


vif(ShelterGLM)


## Logit model results
### Relative risk ratios
logit.or = exp(coef(ShelterGLM))

library(stargazer)

stargazer(ShelterGLM, type="text", coef=list(logit.or), p.auto=FALSE, out="ShelterGLM2018.htm")


## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(ShelterGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- ShelterGLM$null.deviance/-2
Transport.ll.proposed <- ShelterGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.1908401 

## P-value
1 - pchisq(2*(Transport.ll.proposed - Transport.ll.null), df=(length(ShelterGLM$coefficients)-1)) 
### P = 0.000975644





# Import bus stop amenity change data -----

BusStopAmenityChangeData <- read_csv(here("Data", "BusStopAmenityChangeData.csv"))

summary(BusStopAmenityChangeData)


# Prepare data -----

## filter out excluded data 

CleanAmenityChangeData <- filter(BusStopAmenityChangeData, ExcludeIgnoreRidership == "No")


## Change data to numeric as needed
CleanAmenityChangeData$RouteCount <- as.numeric(CleanAmenityChangeData$RouteCount)
CleanAmenityChangeData$WeeklyTrips <- as.numeric(CleanAmenityChangeData$WeeklyTrips)

## code categorical variables 
CleanAmenityChangeData$SharedStop <-ifelse(CleanAmenityChangeData$Shared=="Yes",1,0)
CleanAmenityChangeData$City <-ifelse(CleanAmenityChangeData$CityCounty=="City",1,0)
CleanAmenityChangeData$StateRoad <-ifelse(CleanAmenityChangeData$RoadOwner=="state",1,0)
CleanAmenityChangeData$LocalRoad <-ifelse(CleanAmenityChangeData$RoadOwner=="local",1,0)
CleanAmenityChangeData$AddedAmenity <-ifelse(CleanAmenityChangeData$Change=="Yes",1,0)

summary(CleanAmenityChangeData)

## Test categorical variable combinations
xtabs(~ AddedAmenity + SharedStop, data=CleanAmenityChangeData)
xtabs(~ AddedAmenity + City, data=CleanAmenityChangeData)
xtabs(~ AddedAmenity + StateRoad, data=CleanAmenityChangeData)
xtabs(~ AddedAmenity + LocalRoad, data=CleanAmenityChangeData)


## Check to see if log transformations are needed
hist(CleanAmenityChangeData$TotalRidership) 

hist(CleanAmenityChangeData$RouteCount) ## Inclusion of routes might not be viable

hist(CleanAmenityChangeData$WeeklyTrips)
CleanAmenityChangeData$LogTrips <- log(CleanAmenityChangeData$WeeklyTrips)
hist(CleanAmenityChangeData$LogTrips) ## Inclusion of routes might not be viable

hist(CleanAmenityChangeData$Population)

hist(CleanAmenityChangeData$PctZeroCar)

hist(CleanAmenityChangeData$PctTransit)

hist(CleanAmenityChangeData$PctNonWhite)

hist(CleanAmenityChangeData$PctLowIncome)

hist(CleanAmenityChangeData$EMP)

summary(CleanAmenityChangeData)



# Correlation Matrix -----
library(reshape2)

## Keep variables of interest

#### Remove PctOwnerOccupied because of correlations with transport metrics
#### Remove NOVA because of correlations with density and vehicle ownership

CorrelationData <- CleanAmenityChangeData %>%
  dplyr::select(AddedAmenity,
                SharedStop,
                City,
                StateRoad,
                LocalRoad,
                Population,
                PctZeroCar,
                PctTransit,
                PctNonWhite,
                PctLowIncome,
                EMP)


#Correlation Matrix
CorMatrix <- round(cor(CorrelationData),2)
head(CorMatrix) #### Returns the first or last parts of a vector, matrix, table, data frame or function

#### Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(CorMatrix) #### Save data for upper triangle
upper_tri

### Convert an object into a molten data frame.
melted_Cor <- melt(upper_tri, na.rm=TRUE)

## Heatmap
ggheatmap <- ggplot(melted_Cor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

### Print the heatmap
print(ggheatmap)

### Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



## Convert relevant columns into factors
CleanAmenityChangeData$SharedStop <-as.factor(CleanAmenityChangeData$SharedStop)
CleanAmenityChangeData$City <-as.factor(CleanAmenityChangeData$City)
CleanAmenityChangeData$StateRoad <-as.factor(CleanAmenityChangeData$StateRoad)
CleanAmenityChangeData$LocalRoad <-as.factor(CleanAmenityChangeData$LocalRoad)
CleanAmenityChangeData$AddedAmenity <-as.factor(CleanAmenityChangeData$AddedAmenity)




# Logit Regression Model 3 - Likelihood bus stop had added amenity  -----
library(car)

AmenityChangeGLM <- glm(AddedAmenity ~ Population +
                    Population +
                    PctZeroCar +
                    PctTransit +
                    PctNonWhite +
                    PctLowIncome +
                    EMP +
                    SharedStop +
                    City +
                    StateRoad +
                    LocalRoad,
                  family = "binomial",
                  data = CleanAmenityChangeData)

AmenityChangeGLM <- glm(AddedAmenity ~ Population +
                          Population +
                          PctZeroCar +
                          PctNonWhite +
                          EMP +
                          SharedStop +
                          City +
                          StateRoad,
                        family = "binomial",
                        data = CleanAmenityChangeData)

summary(AmenityChangeGLM)


vif(SeatingGLM)


## Logit model results
### Relative risk ratios
logit.or = exp(coef(AmenityChangeGLM))

library(stargazer)

stargazer(AmenityChangeGLM, type="text", coef=list(logit.or), p.auto=FALSE, out="AmenityChange.htm")


## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(AmenityChangeGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")

## McFadden's Pseudo R2
Transport.ll.null <- AmenityChangeGLM$null.deviance/-2
Transport.ll.proposed <- AmenityChangeGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.2452249

## P-value
1 - pchisq(2*(Transport.ll.proposed - Transport.ll.null), df=(length(SeatingGLM$coefficients)-1)) 
### P = 2.436273e-12

