library(tidyverse)
library(modelr)
library(GGally)

df <- read.csv("GradSchool_Admissions.csv")

summary(df) 
names(df)

df$admit <- as.logical(df$admit)
df$rank <- as.factor(df$rank)


mod1 <- glm(data = df, formula = admit ~ rank)
mod2 <- glm(data = df, formula = admit ~ rank + gre + gpa,family = "binomial")
mod3 <- glm(data = df, formula = admit ~ rank * gre * gpa,family = "binomial")


summary(mod1)
summary(mod2)
summary(mod3)

mod1mse <- mean(residuals(mod1)^2)
mod2mse <- mean(residuals(mod2)^2)
mod3mse <- mean(residuals(mod3)^2)

# mod3 has lowest mse

mod3.pred <- add_predictions(df,mod3,type = "response")

ggpairs(mod3.pred)
# admit has a bigger correlation with rank than gre or gpa


ggplot(mod3.pred, aes(x=gpa,y=pred,color=rank))+
  geom_point()

ggplot(mod3.pred,aes(x=gre,y=pred,color=rank))+
  geom_point()








