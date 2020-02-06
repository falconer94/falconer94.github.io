library(tidyverse)
# I
df <- read.csv("DNA_Conc_by_Extraction_Date.csv")
str(df)

hist(df$DNA_Concentration_Katy,
     main="Katy's PCR",
     xlab="Concentration of DNA")

hist(df$DNA_Concentration_Ben,
     main="Ben's PCR",
     xlab="Concentration of DNA")

# II

# Boxplot; "Year collected" to factor
str(df)
# Katy
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Katy,
     main="Katy's Extractions",xlab="YEAR",ylab="DNA Concentration")
# Ben
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Ben,
     main="Ben's Extractions",xlab="YEAR",ylab="DNA Concentration")

# III
jpeg("PARK_Plot1.jpeg")
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Katy,
     main="Katy's Extractions",xlab="YEAR",ylab="DNA Concentration")
dev.off()
jpeg("PARK_Plot2.jpeg")
plot(x=as.factor(df$Year_Collected),y=df$DNA_Concentration_Ben,
     main="Ben's Extractions",xlab="YEAR",ylab="DNA Concentration")
dev.off()

# IV
summary(df)
df3<- df %>% group_by(Year_Collected) %>% 
  summarize(Bens_performance = mean(DNA_Concentration_Ben),
            Katys_performance = mean(DNA_Concentration_Katy))
ben_sucks <- diff(df3$Bens_performance)
katy_sucks <- diff(df3$Katys_performance)
nice_going_ben <- ben_sucks<katy_sucks
view(nice_going_ben)

# V
df2<- df %>% filter(Lab == "Downstairs")
jpeg("Ben_DNA_over_time.jpg")
plot(x=as.POSIXct(df2$Date_Collected),y=df2$DNA_Concentration_Ben,
     xlab="Date_Collected",ylab="DNA_Concentration_Ben")
dev.off()





