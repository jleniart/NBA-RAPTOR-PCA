#Exploratory Data Analysis

#Read file and remove non-numeric variables from dataset
latestRAPTOR = read.csv("latest_RAPTOR_by_team_2021.csv")
RaptorNums = latestRAPTOR[ ,6:23]
summary(RaptorNums) #check summary statistics for each variable

#Split dataset into regular season and playoffs
#We will use the regular season data because it includes all teams/players
RaptorRegular = latestRAPTOR[latestRAPTOR$season_type=='RS', ]
RaptorRegularNums = RaptorRegular[ ,6:23]

#To look at the playoff date, use the below code...
#RaptorPlayoff = latestRAPTOR[latestRAPTOR$season_type=='PO', ]
#RaptorPlayoffNums = RaptorPlayoff[ ,6:23]

#Select only the observations with mp >= 99 (which is Q1 for mp values)
RaptorRegularMP = RaptorRegularNums[RaptorRegularNums$mp>=99, ]
summary(RaptorRegularMP)

#Remove war_reg_season and war_playoffs
#These variables are redundant because we have a war_total variable
RaptorRegularFinal = RaptorRegularMP[ ,c(1:12,15:18)]

#--------------------------------------------------------------
#Principal Components Analysis (PCA)

#Check the correlations between the variables
library(corrplot)
corRaptor = cor(RaptorRegularFinal)
corrplot(corRaptor)

#Check the summary statistics
summary(RaptorRegularFinal) #best to use scaled PCA due to difference in values

#--------------------------------------------------------------
#Running PCA using the updated dataset

#Remove our dependent variable (war_total)
RaptorRegularFinalX = RaptorRegularFinal[ ,c(1:11,13:16)]

#Check unscaled PCA first
RaptorPCA = prcomp(RaptorRegularFinalX)
summary(RaptorPCA) #as expected - only one component (with 99.9% of variance)
plot(RaptorPCA, main="PCA - Unscaled RAPTOR", xlab="Principal Components")

#Now run scaled PCA (to do so, use correlation matrix or scale=T)
RaptorPCA2 = prcomp(RaptorRegularFinalX, scale=T)
summary(RaptorPCA2)
plot(RaptorPCA2, main="PCA - Scaled RAPTOR", xlab="Principal Components")
abline(1, 0, col="red") #variance >= 1 is a guideline for selecting PCs
RaptorPCA2

#--------------------------------------------------------------
#Running PCA using the historical dataset
historicalRAPTOR = read.csv("historical_RAPTOR_by_player.csv")
histRaptorNums = historicalRAPTOR[ ,4:15]
summary(histRaptorNums)

#Select only the observations with mp >= 99 (same threshold)
histRaptorMP = histRaptorNums[histRaptorNums$mp>=99, ]
summary(histRaptorMP)

#Drop war_reg_season and war_playoffs
histRaptorFinal = histRaptorMP[ ,c(1:6,9:12)]
summary(histRaptorFinal)

#Check correlations
library(corrplot)
corHistRaptor = cor(histRaptorFinal)
corrplot(corHistRaptor)
corHistRaptor

#Check for multicolinearity using VIF
library(car)
vif(lm(war_total ~ ., data=histRaptorFinal))
#vif() gives an error because some variables are perfectly correlated

#Remove dependent variable war_total and run scaled PCA
histRaptorFinalX = histRaptorFinal[ ,c(1:5,7:10)]
histRaptorPCA = prcomp(histRaptorFinalX, scale=T)
summary(histRaptorPCA)
plot(histRaptorPCA, main="PCA - Historical RAPTOR", xlab="Principal Components")
abline(1, 0, col="red")
histRaptorPCA

#Source file below contains code to create visualizations of the PCs
library(ggplot2)
source("PCA_Plot.R")
PCA_Plot(histRaptorPCA)
PCA_Plot_Secondary(histRaptorPCA)

#Run parallel analysis
#This can help decide how many PCs to select
nrow(histRaptorFinalX)
ncol(histRaptorFinalX)
randM = matrix(rnorm(9*17263, 0, 1), ncol=9)
randDF = data.frame(randM)
randPCA = prcomp(randDF, scale=T)
plot(randPCA, main = "Random PCA", xlab="Principal Components")
abline(1, 0, col="red")
#use parallel function to run 500 simulations with random data
library(psych)
parallelPCA = fa.parallel(histRaptorFinalX, n.iter=500)
warnings()

#Remove pace_impact and rerun PCA

#--------------------------------------------------------------
#Rerunning PCA with most recent "era"
ModEraRAPTOR = read.csv("raptor_by_player_10-22.csv")
ModEraRaptorNums = ModEraRAPTOR[ ,4:15]
summary(ModEraRaptorNums)

#Select only the observations with mp >= 99 (same threshold)
ModEraRaptorMP = ModEraRaptorNums[ModEraRaptorNums$mp>=99, ]
summary(ModEraRaptorMP)

#Drop war_reg_season and war_playoffs
ModEraRaptorFinal = ModEraRaptorMP[ ,c(1:6,9:12)]
summary(ModEraRaptorFinal)

#Remove dependent variable war_total and run scaled PCA
ModEraRaptorFinalX = ModEraRaptorFinal[ ,c(1:5,7:10)]
ModEraRaptorPCA = prcomp(ModEraRaptorFinalX, scale=T)
summary(ModEraRaptorPCA)
plot(ModEraRaptorPCA, main="PCA - Modern Era RAPTOR", xlab="Principal Components")
abline(1, 0, col="red")
ModEraRaptorPCA

#--------------------------------------------------------------
#Proposed Next Steps: Use components from PCA in regression