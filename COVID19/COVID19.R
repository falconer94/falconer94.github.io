library(tidyverse)

cv <- read.csv("Provisional_Death_Counts_for_Coronavirus_Disease__COVID-19_.csv")

# Tidy ####
names(cv)

# pna = pneumonia

# Rename columns
cv <- cv %>% 
  rename("Date.as.of" = "ï..Data.as.of") %>%
  rename("All.cv.deaths" = "All.COVID.19.Deaths..U07.1.") %>%
  rename("All.pna.deaths" = "All.Pneumonia.Deaths..J12.0.J18.9.") %>% 
  rename("Pna.cv.deaths"  = "Deaths.with.Pneumonia.and.COVID.19..J12.0.J18.9.and.U07.1.") %>% 
  rename("All.flu.deaths" = "All.Influenza.Deaths..J09.J11.") %>% 
  rename("pna.flu.cv.deaths" = "Pneumonia..Influenza..and.COVID.19.Deaths")


# Rename age classes
?str_replace
cv$Indicator <- str_replace(cv$Indicator, "â€“","-") %>% 
  str_remove("years") %>% str_remove(" year")


# by_age; remove unused columns
by_age <- 
  cv %>% filter(Group == "By age") %>% 
  select(Indicator,All.cv.deaths,All.pna.deaths,All.flu.deaths,Pna.cv.deaths,)
names(by_age)

# Remove commas; numeric

by_age$All.cv.deaths <- as.numeric(gsub(",","",by_age$All.cv.deaths))

by_age$All.pna.deaths <- as.numeric(gsub(",","",by_age$All.pna.deaths))

by_age$Pna.cv.deaths <- as.numeric(gsub(",","",by_age$Pna.cv.deaths))

by_age$All.flu.deaths <- as.numeric(gsub(",","",by_age$All.flu.deaths))


str(by_age)

# Indicator as facter; reorder levels
by_age$Indicator <- 
  factor(by_age$Indicator,levels = c("Under 1","1-4 ","5-14 ","15-24 ","25-34 ","35-44 ","45-54 ","55-64 ","65-74 ","75-84 ","85  and over", "All ages"))
str(by_age)
levels(by_age$Indicator)

# Remove All ages
by_age <- 
  by_age %>% filter(Indicator != "All ages")



# Gather columns as variable
by_age <- 
  gather(by_age, -"Indicator", key = "type",value = "deaths")

# Type as factor
by_age$type <- 
  as.factor(by_age$type)


# Plotting ####
# x = Week
# y = Deaths: by all causes ; pneumonia ; pneomonia and covid ; flu ; pn or cov or flu



# Plot 
ggplot(by_age, aes(x= Indicator,y=deaths,color=type)) +
  geom_line(aes(group=type),size=2)+
  
  labs(title = "Number of disease-related deaths in each age group", x = "Age (years)", y = "Number of deaths")+
  scale_color_discrete(name = "Type of death",
                      breaks= c("All.cv.deaths","All.flu.deaths",
                                "All.pna.deaths", "Pna.cv.deaths"),
                        labels = c("COVID 19", "Influenza","Pneumonia","Pneumonia\nand COVID 19"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))
  
levels(by_age$type)









#### Underlying conditions and symptoms ####

# Tidy
uc <- read.csv("Underlying_conditions_by_age_group.csv") %>% 
  rename("Underlying.condition" = "ï..Underlying.condition") %>% 
  rename("19-49" = "X18â..49") %>% 
  rename("50-64" = "X50â..64") %>% 
  rename("65 and over" = "â..65") 
names(uc)

sm <- read.csv("Symptoms_by_age_group.csv") %>% 
  rename("Symptoms" = "ï..Symptoms") %>% 
  rename("19-49" = "X18â..49") %>% 
  rename("50-64" = "X50â..64") %>% 
  rename("65 and over" = "â..65.years")
names(sm)


# Extract %; numeric
uc$Overall <- 
  str_sub(uc$Overall, -5,-2) %>% str_remove("\\(") %>% as.numeric()
uc$`19-49` <- 
  str_sub(uc$`19-49`, -5,-2) %>% str_remove("\\(") %>% as.numeric()
uc$`50-64` <- 
  str_sub(uc$`50-64`, -5,-2) %>% str_remove("\\(") %>% as.numeric()
uc$`65 and over` <- 
  str_sub(uc$`65 and over`, -5,-2) %>% str_remove("\\(") %>% as.numeric()


sm$Overall <- 
  str_sub(sm$Overall, -5,-2) %>% str_remove("\\(") %>% as.numeric()
sm$`19-49` <- 
  str_sub(sm$`19-49`, -5,-2) %>% str_remove("\\(") %>% as.numeric()
sm$`50-64` <- 
  str_sub(sm$`50-64`, -5,-2) %>% str_remove("\\(") %>% as.numeric()
sm$`65 and over` <- 
  str_sub(sm$`65 and over`, -5,-2) %>% str_remove("\\(") %>% as.numeric()


# Gather age groups

uc <- uc %>%  
  gather("Overall","19-49","50-64","65 and over", key="age.group", value = "percentage")

sm <- sm %>%  
  gather("Overall","19-49","50-64","65 and over", key="age.group", value = "percentage")
names(sm)

# Rename symptoms/conditions
uc$Underlying.condition <- str_remove_all(uc$Underlying.condition,"Â") %>% str_remove_all("Â¶") %>% 
  str_trim() %>% str_remove_all("¶")

sm$Symptoms <- str_remove_all(sm$Symptoms,"Â") %>% str_remove_all("Â¶") %>% 
  str_trim() %>% str_remove_all("¶")


# age group as factor
str(uc)

uc$age.group <- as.factor(uc$age.group)
sm$age.group <- as.factor(sm$age.group)


# Plot

  ggplot(uc, aes(x=age.group,y = percentage,color=Underlying.condition)) +
    geom_line(aes(group=Underlying.condition),size=2)+
    labs(title= "Underlying conditions of COVID 19 patients", 
         color= "Underlying condition", x = "Age group",y= "Proportion")+
    theme_minimal()

  
  
  ggplot(sm, aes(x=age.group,y = percentage,color=Symptoms)) +
    geom_line(aes(group=Symptoms),size=2)+
  labs(title= "Symptoms of COVID 19 patients", 
       x = "Age group",y= "Proportion")+
  theme_minimal()



#### World Data ####
wd <- read.csv("owid-covid-data.csv")
