library(tidyverse)
library(janitor)
library(beepr)
#options(scipen=999) # y scientific notation tho



#### Import ####
sav <- read.csv("Finance_data/Dec2020/Savings.csv", header = FALSE)
che <- read.csv("Finance_data/Dec2020/checking.csv", header = FALSE)
cha_j <- read.csv("Finance_data/Dec2020/Jeff.CSV")
cha_k <- read.csv("Finance_data/Dec2020/Kenna.CSV")



#### Tidy and Join ####

# Columns
# Date, Account, Description, Category, Amount

#### cha_j 
names(cha_j)

cha_j <- cha_j %>% 
  rename(Date = Transaction.Date) %>% 
  select(-Post.Date, -Type, -Memo) %>% 
  add_column(Account = "Chase_Jeff") 


#### cha_k
cha_k <- cha_k %>% 
  rename(Date = Transaction.Date) %>% 
  select(-Post.Date, -Type, -Memo) %>% 
  add_column(Account = "Chase_Kenna") 


#### che

che <- che %>% 
  select(-V3, -V4) %>% 
  rename(
    Date = V1,
    Amount = V2,
    Description = V5
  ) %>% 
  add_column(Account = "Checking") 


#### sav
names(sav)
sav <- sav %>% 
  select(-V3, -V4) %>% 
  rename(
    Date = V1,
    Amount = V2,
    Description = V5
  ) %>% 
  add_column(Account = "Savings") 



# Joined

joined <- full_join(cha_j, cha_k) %>% 
  full_join(che) %>% 
  full_join(sav)


#### Rename Descriptions ####

joined$Description[grepl("Spotify", joined$Description)] <- "Spotify"
joined$Description[grepl("POWER", joined$Description)] <- "Power"
joined$Description[grepl("Energy", joined$Description)] <- "Energy"
joined$Description[grepl("eDeposit", joined$Description)] <- "Deposit"
joined$Description[grepl("PARK MCKENNA L", joined$Description)] <- "Chase_Card_Kenna"
joined$Description[grepl("Utah Community C", joined$Description)] <- "Mazda"
joined$Description[grepl("TPM", joined$Description)] <- "HOA"
joined$Description[grepl("TRANSFER", joined$Description)] <- "Online_Transfer"
joined$Description[grepl("JEFFREY PARK", joined$Description)] <- "Chase_Card_Jeff"
joined$Description[grepl("MORTG", joined$Description)] <- "Mortgage"
joined$Description[grepl("COMCAST", joined$Description)] <- "Comcast"
joined$Description[grepl("SPROUTS", joined$Description)] <- "Sprouts"
joined$Description[grepl("WAL-MART", joined$Description)] <- "Walmart"
joined$Description[grepl("WM SUPERCENTER", joined$Description)] <- "Walmart"
joined$Description[grepl("AMZN", joined$Description)] <- "Amazon"
joined$Description[grepl("Amazon.com", joined$Description)] <- "Amazon"
joined$Description[grepl("LOWES", joined$Description)] <- "Lowes"
joined$Description[grepl("CITY OF OREM UTIL", joined$Description)] <- "Utilities"
joined$Description[grepl("JesusChrist", joined$Description)] <- "Tithing"






#### Categories ####
# Home, Education, Fun, Hobby, Groceries, 
# Health, Transfer, Monthly, Automotive, Gas, 
# Gifts, Deposit, Work, Interest, Tithing

unique(joined$Category)

### Rename categories
joined$Category[joined$Category == "Entertainment"] <- "Fun"
joined$Category[joined$Category == "Shopping"] <- "Fun"
joined$Category[joined$Category == "Food & Drink"] <- "Fun"
joined$Category[joined$Category == "Travel"] <- "Fun"
joined$Category[joined$Category == "Personal"] <- "Fun"
# joined$Category[joined$Category == "check"] <- "Deposit"
# joined$Category[joined$Category == "Gift"] <- "Gifts"
joined$Category[joined$Category == "Bills & Utilities"] <- "Monthly"
joined$Category[joined$Category == "Health & Wellness"] <- "Health"


### Fill in blank categories
joined$Category[grepl("Power", joined$Description)] <- "Monthly"
joined$Category[grepl("Energy", joined$Description)] <- "Monthly"
joined$Category[grepl("Spotify", joined$Description)] <- "Monthly"
joined$Category[grepl("Mazda", joined$Description)] <- "Monthly"
joined$Category[grepl("Mortgage", joined$Description)] <- "Monthly"
joined$Category[grepl("Comcast", joined$Description)] <- "Monthly"
joined$Category[grepl("HOA", joined$Description)] <- "Monthly"
joined$Category[grepl("Utilities", joined$Description)] <- "Monthly"

joined$Category[grepl("PAYMENT", joined$Description)] <- "Transfer"
joined$Category[grepl("Payment", joined$Description)] <- "Transfer"
joined$Category[grepl("Chase", joined$Description)] <- "Transfer"
joined$Category[grepl("Deposit", joined$Description)] <- "Transfer"
joined$Category[grepl("Ibotta", joined$Description)] <- "Transfer"
joined$Category[grepl("VENMO", joined$Description)] <- "Transfer"
joined$Category[grepl("Online_Transfer", joined$Description)] <- "Transfer"

joined$Category[grepl("Tithing", joined$Description)] <- "Monthly"

### Reoder columns
joined <- joined[,c(1,4,3,5,2)]

#### Change Classes ####
# joined$Category <- as.factor(joined$Category)
# joined$Account <- as.factor(joined$Account)

#### Export ####

# Sum by category
Category_Total <- joined2  %>% 
  group_by(Category) %>% 
  summarize(Amount = sum(Amount)) 

Category_Total2 <- Category_Total %>% 
  adorn_totals()

write.csv(joined, "Finance_data/Nov2020/Nov2020.csv")
write.csv(Category_Total, "Finance_data/Nov2020/Nov2020_categories.csv")


#### Analyze ####

# Graph by category totals
Category_Total %>% 
  ggplot(aes(x = Category, y = Amount)) +
  geom_point()


# Remove Transfer
joined2 <- joined %>% 
  filter(Category != "Transfer")


joined2 %>% 
  ggplot(aes(x = Date, y = Amount)) +
  geom_point(aes(color = Category))








#### Unique categories ####
unique(joined2$Category)



beep("treasure")
