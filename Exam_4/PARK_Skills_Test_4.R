library(tidyverse)

# I Histograms ####

df <- read.csv("DNA_Conc_by_Extraction_Date.csv")

hist(df$DNA_Concentration_Katy,
     main="Katy's PCR",
     xlab="Concentration of DNA")

hist(df$DNA_Concentration_Ben,
     main="Ben's PCR",
     xlab="Concentration of DNA")

hist(df$DNA_Concentration_Katy,
     main="Katy's PCR",
     xlab="Concentration of DNA")

hist(df$DNA_Concentration_Ben,
     main="Ben's PCR",
     xlab="Concentration of DNA")

# II ####

# Boxplot; "Year collected" to factor
str(df)
# Katy
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Katy,
     main="Katy's Extractions",xlab="YEAR",ylab="DNA Concentration")
# Ben
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Ben,
     main="Ben's Extractions",xlab="YEAR",ylab="DNA Concentration")




# III ####
jpeg("PARK_Plot1.jpeg")
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Katy,
     main="Katy's Extractions",xlab="YEAR",ylab="DNA Concentration")
dev.off()
jpeg("PARK_Plot2.jpeg")
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Ben,
     main="Ben's Extractions",xlab="YEAR",ylab="DNA Concentration")
dev.off()



# IV ####
df$Year_Collected<- as.factor(df$Year_Collected)
summary(df)
Avgs<- df %>% group_by(Year_Collected) %>% 
  summarize(Bens_performance = mean(DNA_Concentration_Ben),
            Katys_performance = mean(DNA_Concentration_Katy)) %>% 
  mutate(Difference = Bens_performance - Katys_performance)

Ben_lowest_avg <- Avgs %>% arrange(Difference) %>% head(1)
Ben.sucked.the.most.on <- Ben_lowest_avg[1,1]



# V ####
df2<- df %>% filter(Lab == "Downstairs")
jpeg("Ben_DNA_over_time.jpg")
plot(x=as.POSIXct(df2$Date_Collected),y=df2$DNA_Concentration_Ben,
     xlab="Date_Collected",ylab="DNA_Concentration_Ben")
dev.off()



# VI ####
Ben_highest_avg <- Avgs %>% arrange(desc(Bens_performance)) %>% head(1)
Ben.performed.best.on <- Ben_highest_avg[1,1]
Avgs %>% select(Year_Collected,Bens_performance) %>% 
  write.csv("Ben_Average_Conc.csv")

