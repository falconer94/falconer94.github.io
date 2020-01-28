library(tidyverse)

?read.table()
getwd()
setwd("../../../Data_Course/Data/")
list.files()
df <- read.csv("landdata-states.csv") # read.csv looks for different characteristics than read.table
class(df) #data.frame
head(df)

# 1. read.csv automatically uses "." as decimals. fill = TRUE, so blank fields are added if rows are unequal. Turns off comments. 
# 2. In .csv2, ";" separates each data point and "," are decimals. 
# 3. Other nations use csv2. 

class(df$State)
class(df$Date)


# 4.
df_State.as.char <- 
  read.csv("landdata-states.csv") %>% 
  as.character(df_State.as.char$State)
class(df_State.as.char)

dim(df)
str(df)
summary(df)

# 5.
summary(df$Home.Value)

# 6. Home.Value, the fourth column name
names(df)[4]

hist(df$Land.Value)
plot(x=df$region,y=df$Land.Value)

# 7.
plot(x=df$Year,y=df$Land.Value,col=df$region)
# the color changes to represent each region



