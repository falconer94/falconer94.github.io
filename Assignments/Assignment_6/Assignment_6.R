library(tidyverse)
data("mtcars")
str(mtcars)
?mtcars

# only automatic
auto <- mtcars %>% filter(am == "0")
write.csv(auto,"./automatic_mtcars.csv")

# horsepower vs miles per gallon
png("mpg_vs_hp_auto.png")
ggplot(auto,aes(x=hp,y=mpg))+
  geom_point()+
  labs(title="The effect of horsepower on miles-per-gallon",x="Horsepower",y="Miles-per-gallon")
dev.off()

# weight vs miles-per-gallon
tiff("mpg_vs_wt_auto.tiff")
ggplot(auto,aes(x=wt,y=mpg))+
  geom_point()+
  labs(title="The effect of weight on miles-per-gallon",
       x="Weight (1,000 lbs)",y="Miles-per-gallon")
dev.off()

# subset disp less than or equal to 200 cu in
max200 <- mtcars %>% filter(disp <= "200")
write.csv(max200,"./mtcars_max200_displ.csv")

# max hp of each group


hp_orig <- mtcars[which.max(mtcars$hp),]
hp_200 <- max200[which.max(max200$hp),]
hp_auto <- auto[which.max(auto$hp),]

max_hp <- full_join(hp_orig,hp_200) %>% full_join(hp_auto)

write.table(max_hp,"./hp_maximums.txt")
