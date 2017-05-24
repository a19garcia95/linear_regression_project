#STATISTICAL ANALYSIS

statistics_metro_and_energy <- subset(dataset1, select = c("metro", "energy"))
summary(statistics_metro_and_energy)

# correlation between metro and energy
cor(statistics_metro_and_energy)
plot(statistics_metro_and_energy)

install.packages('corrgram')
library(corrgram)

corrgram(statistics_metro_and_energy, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Metro vs Energy")
#This states again that there is a 33% correlation between the 2 variables metro and energy

#-----------------------------------------------------------------------------------------------------

#DATA PREPROCESSING

#install.packages('dplyr')
#install.packages('caTools')
library(dplyr)
library(caTools)

dataset1 <- read_dta("states.dta")

#energy is the dependent variable, metro is the independent variable
dataset <- select(dataset1, metro, energy)


# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$energy, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#-----------------------------------------------------------------------------------------------------

#FITTING LINEAR REGRESSION TO TRAINING SE

regressor = lm(formula = energy ~ metro,
               data = training_set)
#REGRESSOR INFO: METRO HAS ONE *, MEANING THAT IS 33%(Low) SIGNIFICANT (Low-Med-High)

y_pred = predict(regressor, newdata = test_set)

#-----------------------------------------------------------------------------------------------------

#VISUALISING THE TRAINING SET
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$metro, y = training_set$energy),
             colour = 'red') +
  geom_line(aes(x = training_set$metro, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Energy vs Metro (Training set)') +
  xlab('metro (Metropolitan area population, in %') +
  ylab('Energy (Per Capita Energy Consumed in BTU)')

#-----------------------------------------------------------------------------------------------------

#VISUALISING THE TEST SET
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$metro, y = test_set$energy),
             colour = 'red') +
  geom_line(aes(x = training_set$metro, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Energy vs Metro (Training set)') +
  xlab('metro (Metropolitan area population, in %') +
  ylab('Energy (Per Capita Energy Consumed in BTU)')


#-----------------------------------------------------------------------------------------------------









