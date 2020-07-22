library(tidyverse)
library(modelr)

#### 1. ####
df <- read.csv("mushroom_growth.csv")
df
names(df)
# dependent = GrowthRate

#### 2. Plots ####
ggplot(df,aes(y=GrowthRate,x=Nitrogen))+
  geom_point()

ggplot(df,aes(y=GrowthRate,x=Humidity))+
  geom_point()

ggplot(df,aes(y=GrowthRate,x=Species))+
  geom_point()

ggplot(df,aes(y=GrowthRate,x=Light))+
  geom_point()



#### 3. Models ####
mod1 <- lm(df, formula = GrowthRate ~ Species)
mod2 <- aov(df,formula = GrowthRate ~ Light)

summary(mod1)
summary(mod2)



#### 4. MSE ####

mod1mse <- mean(residuals(mod1)^2)
mod2mse <- mean(residuals(mod2)^2)

#### 5. ####
# mod2 is better because it has a smaller MSE


#### 6. Predictions ####

mod2.pred <- add_predictions(df,mod2)


#### 7. Plot Prediction ####

ggplot(mod2.pred,aes(x=Light))+
  geom_point(aes(y=GrowthRate)) +
  geom_point(aes(y=pred),color="red")






