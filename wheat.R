library(ggpubr)
library(usdm)
library(sjPlot)
library(car)
library(DHARMa)
library(ggplot2)

#import data
data<- read.table("data.csv", sep = ",", header = TRUE)
head(data)

#plot to overview
ggplot(data, aes(x = year, y = production)) + 
  geom_bar(stat="identity", fill="lightblue") + 
  geom_smooth(method=lm) + theme_dark()


#select cols of interest
data <- data[, - c(10)]

#replace NAs with column mean
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#plot to check the distribution
hist(data$production)
ggqqplot(data$production)

#fit LM model
m1 <- lm(production ~  scale(year) + scale(area) + scale(fertilizer) +
           scale(machine) + scale(pesticide) + 
           scale(farmer) + scale(prep) + scale(temp), data = data)

#model validation
summary(m1)
par(mfrow=c(2,2))
plot(m1)

#Variance Inflation Factor
usdm::vif(data[,-c(2)]) #decide to remove year
usdm::vif(data[,-c(1,2)]) #decide to remove fertilizer
usdm::vif(data[,-c(1,2,5)]) #not need to remove machine
usdm::vif(data[,-c(1,2,4,5)])

#model fitting with exclusion of high collinear variables
m2 <- lm(production ~ scale(area) +
           scale(machine) + scale(pesticide) + 
           scale(farmer) + scale(prep) + scale(temp), data = data)

#model validation
summary(m2)
par(mfrow=c(2,2))
plot(m2)

#model comparison
anova(m1, m2) 
#m2 significant different from m1
#use m1 as the final one

#model plotting
plot_model(m1, show.values = TRUE, show.intercept = TRUE, sort.est = TRUE, 
           vline.color = "black", rm.terms = c("(Intercept)"),
          title = "Impacts on winter wheat production")

#plot multivariate lm with added-variable plots
avPlots(m1)

#temporal autocorrelation
#simulationOutput <- simulateResiduals(m1)
#testTemporalAutocorrelation(simulationOutput, data$year, plot = T)
