# chapter 6, Dec 7, 2019
# Analysis of longitudinal data



# loading libraries
library(dplyr)
library(tidyr)

# loading the datasets and reading and saving the datasets
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep  ="\t", header = T)
write.csv(BPRS, file = "BPRS.csv")
write.csv(RATS, file = "RATS.csv")

#looking at the names 
names(BPRS)
names(RATS)
#looking at the structure
str(BPRS)
str(RATS)

# converting integer to factorial variable i.e categorical variables to factors
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
RATS$Group <- factor(RATS$Group)
RATS$ID <- factor(RATS$ID)
glimpse(BPRS)
glimpse(RATS)

# Convert datasets from wide to long form, Add week and time variables
BPRSL <- BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject) %>% mutate(weeks = as.integer(substr(weeks,5,5)))
RATSL <- RATS %>% gather(key = WD, value = Weight, -ID, -Group) %>% mutate(Time = as.integer(substr(WD,3,5)))

glimpse(BPRSL)
names(BPRSL)
str(BPRSL)

glimpse(RATSL)
names(RATSL)
str(RATSL)

# save datasets for analysis
write.csv(BPRSL, file = "BPRSL.csv")
write.csv(RATSL, file = "RATSL.csv")
