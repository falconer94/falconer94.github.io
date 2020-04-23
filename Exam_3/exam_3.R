# 1. Do an analysis of the mtcars data set. That's right, our old friend mpg 
  # will be the dependent variable.
# 2. Do your analysis in an Rmarkdown document, complete with explanations 
  # for what is going on, 
  # what models you are trying, and 
  # figures showing predicted vs real data, etc.
# 3. Knit that Rmarkdown to an html file and add it to your website with 
  # a link that is easy for me to find on your page.



library(tidyverse)
library(GGally)
library(modelr)

?mtcars
df <- mtcars

df$cyl <- as.factor(df$cyl)

cor.test(mod1)

ggpairs(df)

# Lighter cars have lower mpg. 
# Fewer cylanders have lower mpg. 


mod1 <- glm(data=df, formula = wt ~ mpg)
mod2 <- glm(data=df, formula = wt ~ mpg + cyl)
mod3 <- glm(data=df, formula = wt ~ mpg * cyl)

mod1mse <- mean(residuals(mod1)^2)
mod2mse <- mean(residuals(mod2)^2)
mod3mse <- mean(residuals(mod3)^2)

# mod3 has lowest mean squared error

mod3.pred <- add_predictions(df,mod3,type = "response")


# plots

ggplot(mod3.pred,aes(y=mpg,color=cyl)) +
  geom_point(aes(x=wt))+
  geom_line(aes(x=pred))+
  labs(title="Relationship between gas efficiency, weight, and number of cylinders",
       x="Weight (1000 lbs)",y="Gas efficiency (MPG)",color="Cylinders")

# As the weight and number of cylenders increases, the gas efficiency decreases. 

