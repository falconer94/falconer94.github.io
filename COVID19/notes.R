#### Plot ideas ####
#Ages in each state (map?); 
# then compare ages in each state and number of deaths

# Compare recovered individuals by age group to died



#### Time Series ####


# plotting decomposed data shows

library(forecast)

#### Cluster ####

t()

#### Classification ####
library(party)

=
# Log transformation
log(by_age)

# Decompose(data) additive
decompose(by_age)

# plot(decomp data)










# usmap Tidy ####
by_state <- by_state %>% rename(values = All.COVID.19.Deaths..U07.1.)
by_state <- by_state %>% rename(state = State)


by_state_covid <- by_state %>% select("state", "values")

# Remove blanks
by_state_covid <- by_state_covid %>% filter(!values == "")

# Remove commas
by_state_covid$values <- gsub(",","",by_state_covid$values)

# as numeric
by_state_covid$values <- as.numeric(by_state_covid$values)

# To FIPS and remove NAs
by_state_covid$state <- fips(by_state_covid$state) 
by_state_covid <- by_state_covid %>% na.omit("state")


saveRDS(by_state_covid, "covid.RDS")
