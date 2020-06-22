a <- read.csv(choose.files())
b <- a[,-1]

#We need to convert the data into decimal format
b1 <- as.data.frame(apply(apply(b, 2, gsub, patt=",", replace="."), 2, as.numeric))
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
b_norm<-as.data.frame(lapply(b1,FUN=normalize))
attach(b_norm)

install.packages("dlookr")
install.packages("dplyr")
library(dlookr)
library(dplyr)

describe(b_norm)
tibble1 <- describe(b_norm)
tibble1

normality(b_norm)
tibble2 <- normality(b_norm)
tibble2

correlate(b_norm)
tibble3 <-correlate(b_norm)
plot_correlate(b_norm)

eda_report(b_norm, X..Silica.Concentrate, output_format = "html", output_file = "EDA.html")

##Building the model
library(car)
library(carData)

#Splitting the data into 80% traindata and 20% testdata
part <- sample(nrow(b_norm),nrow(b_norm)*0.8,replace = FALSE)
train <- b_norm[part,]
test <- b_norm[-part,]

#Building model with traindata
set.seed(123)

mymodel<-lm(X..Silica.Concentrate~.,data = train)
summary(mymodel)
##R-squared value is 0.6798

plot(mymodel)

influencePlot(mymodel)
avPlots(mymodel)
library(MASS)
stepAIC(mymodel)

##Predicting on test data
predictions1 <- predict(mymodel, test)

library(ModelMetrics)

##Calculating RMSE
rmse(test$X..Silica.Concentrate, predictions)
##RMSE value is 0.1296043

mymodel1 <- lm(X..Silica.Concentrate ~ X..Iron.Feed + X..Silica.Feed + Starch.Flow +Amina.Flow + Ore.Pulp.Flow + Ore.Pulp.pH + Ore.Pulp.Density +
                 Flotation.Column.01.Air.Flow + Flotation.Column.02.Air.Flow + Flotation.Column.03.Air.Flow + Flotation.Column.04.Air.Flow +Flotation.Column.05.Air.Flow + 
                 Flotation.Column.07.Air.Flow + Flotation.Column.02.Level +Flotation.Column.03.Level + Flotation.Column.04.Level + Flotation.Column.05.Level +Flotation.Column.06.Level + Flotation.Column.07.Level, 
               data = train)

summary(mymodel1) #R-squared value is 0.1517

plot(mymodel1)
influencePlot(mymodel1)
avPlots(mymodel1)
stepAIC(mymodel1)

##Predicting on test data
predictions2 <- predict(mymodel1, test)

library(ModelMetrics)

##Calculating RMSE
rmse(test$X..Silica.Concentrate, predictions)
##RMSE value is 0.21087

#Building model with traindata
set.seed(123)

library(dplyr)
attach()
Grouped_data <- group_by(b_norm,X..Silica.Concentrate)

s1 <- summarise(Grouped_data,mean_X..Iron.Feed=mean(X..Iron.Feed),
                mean_X..Silica.Feed=mean(X..Silica.Feed),
                mean_Starch.Flow=mean(Starch.Flow),
                mean_Amina.Flow=mean(Amina.Flow),
                mean_Ore.Pulp.Flow=mean(Ore.Pulp.Flow),
                mean_Ore.Pulp.pH=mean(Ore.Pulp.pH),
                mean_Ore.Pulp.Density=mean(Ore.Pulp.Density),
                mean_Flotation.Column.01.Air.Flow=mean(Flotation.Column.01.Air.Flow),
                mean_Flotation.Column.02.Air.Flow=mean(Flotation.Column.02.Air.Flow),
                mean_Flotation.Column.03.Air.Flow=mean(Flotation.Column.03.Air.Flow),
                mean_Flotation.Column.04.Air.Flow=mean(Flotation.Column.04.Air.Flow),
                mean_Flotation.Column.05.Air.Flow=mean(Flotation.Column.05.Air.Flow),
                mean_Flotation.Column.06.Air.Flow=mean(Flotation.Column.06.Air.Flow),
                mean_Flotation.Column.07.Air.Flow=mean(Flotation.Column.07.Air.Flow),
                mean_Flotation.Column.01.Level=mean(Flotation.Column.01.Level),
                mean_Flotation.Column.02.Level=mean(Flotation.Column.02.Level),
                mean_Flotation.Column.03.Level=mean(Flotation.Column.03.Level),
                mean_Flotation.Column.04.Level=mean(Flotation.Column.04.Level),
                mean_Flotation.Column.05.Level=mean(Flotation.Column.05.Level),
                mean_Flotation.Column.06.Level=mean(Flotation.Column.06.Level),
                mean_Flotation.Column.07.Level=mean(Flotation.Column.07.Level))

#55569 observations only

#Building model with traindata
set.seed(123)

pca<-princomp(s1, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pca)
str(pca)
loadings(pca)

plot(pca) # graph showing importance of principal components 
biplot(pca)

plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type="b")
pca$loadings
pca$scores
pca$scores[,1:15] # Using first 15 pca as it contains 95.99% of information
new_data <- as.data.frame(pca$scores[,1:15])

# Considering top 15 principal components and binding them with Silica Concentrate
combined_new_data <-as.data.frame(cbind(s1$X..Silica.Concentrate,pca$scores[,1:15]))
View(combined_new_data)

names(combined_new_data) <- c("%SilicaConcentrate","PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA11","PCA12","PCA13","PCA14","PCA15")
View(combined_new_data)

part2 <- sample(nrow(combined_new_data),nrow(combined_new_data)*0.8,replace = FALSE)
train2 <- combined_new_data[part2,]
test2 <- combined_new_data[-part2,]

################################################################

#using Conditional Inference Regression Random Forest algorithm

install.packages("party")
library(randomForest)
library(caret)
library(ModelMetrics)

newmodel<-randomForest(train2$`%SilicaConcentrate`~.,data=train2, mtry=5, ntree=100)
newmodel
## Mean of squared residuals: 0.0002186984

#Predicting on the test set
newmodel_pred <- predict(newmodel, test2)
RMSE <- rmse(test2$`%SilicaConcentrate`, newmodel_pred)
RMSE

#RMSE value is 0.01391



