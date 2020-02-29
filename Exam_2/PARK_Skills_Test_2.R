library(tidyverse)

# I

options(scipen = 999) 
ld <- read.csv("./landdata-states.csv")

str(ld)
names(ld)

jpeg("./PARK_Fig_1.jpg")
ggplot(ld,aes(x=Year,y=Land.Value,color=region))+
  geom_smooth()+
  labs(y="Land Value (USD)",Key="Region")+
  theme_minimal()
dev.off()



# II

ld2 <- ld[is.na(ld$region),]
unique(ld2$State)
             


# III

# Variable = column = country, year, u5mr
# Observation = row = that country
# Value = cell

mr <- read.csv("unicef-u5mr.csv")
names(mr)
# Remove U5MR. and Name
names(mr) <- str_remove(names(mr),"U5MR.") %>% str_remove("Name")

# gather values
mrl <- gather(mr, key = Year,value = U5MR, 2:67)
names(mrl)
# remove rows with NA
mrl <- na.omit(mrl)


# IV

jpeg("PARK_Fig_2.jpg")
ggplot(mrl,aes(x=Year,y=U5MR,color=Continent))+
  geom_point(size=3)+
  labs(y="MortalityRate")+
  scale_x_discrete(breaks=seq(0, 2020, 20))+
  theme_minimal()
dev.off()


# IV part 2
# the mean values of each continent
## Year as numeric? nope

mrl2 <- mrl %>%
  group_by(Continent,Year) %>% 
  summarize(U5MR = mean(U5MR))

jpeg("PARK_Fig_3.jpg")
ggplot(mrl2,aes(x=Year,y=U5MR,color=Continent,group=Continent))+
  geom_line(size=2)+
  labs(y="Mean Mortality Rate (deaths per 1000 live births")+
  scale_x_discrete(breaks=seq(0, 2020, 20))+
  theme_minimal()
dev.off()



# V
#Note: The y-axis shows proportions, not raw numbers
#This is a scatterplot, faceted by region

ggplot(mrl,aes(x=Year,y=U5MR))+
  geom_point(size= .5, color="blue")+
  labs(y="Mortality Rate")+
  scale_x_discrete(breaks=seq(0, 2020, 20))+
  scale_y_continuous(labels = label_value())+
  theme_minimal()+
  facet_wrap(~Region)
