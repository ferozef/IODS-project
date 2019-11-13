#Feroze_analysis
#10.11.19
# Chapter 2: Regression and model validation--> Data wrangling and data Ananlysis, the Data wrangling exercise create an analysis dataset for the Analysis exercise. During the Analysis exercise will we explore the data, perform analysis and interpret the results.
  


#get directory
getwd()

data1 <-- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt" , sep = "\t", header= TRUE)
head(data1)

#output of data1
#"Aa Ab Ac Ad Ae Af ST01 SU02 D03 ST04 SU05 D06 D07 SU08 ST09 SU10 D11 ST12 SU13 D14 D15 SU16 ST17 SU18 D19 ST20 SU21 D22 D23 SU24 ST25 SU26 D27
#1 -3 -1 -2 -1 -1 -1   -4   -2  -4   -4   -2  -4  -4   -3   -3   -2  -3   -3   -3  -4  -3   -2   -3   -2  -4   -2   -3  -3  -2   -2   -4   -4  -4
#2 -2 -2 -2 -2 -1 -1   -4   -2  -4   -4   -4  -2  -3   -4   -4   -1  -4   -1   -3  -2  -3   -4   -4   -2  -3   -1   -2  -2  -3   -4   -2   -4  -2
#3 -4 -1 -1 -1 -1 -1   -3   -1  -4   -4   -2  -3  -4   -1   -3   -1  -4   -4   -2  -4  -2   -3   -3   -1  -4   -3   -2  -4  -3   -3   -4   -4  -3
#4 -4 -2 -3 -2 -1 -1   -3   -3  -4   -4   -3  -4  -4   -2   -3   -1  -3   -3   -2  -4  -3   -2   -3   -1  -3   -3   -3  -3  -3   -2   -3   -2  -3
#5 -3 -2 -2 -1 -2 -1   -4   -2  -5   -3   -4  -4  -4   -3   -4   -2  -4   -2   -3  -4  -3   -3   -4   -1  -4   -3   -2  -3  -3   -4   -4   -3  -3
#6 -4 -2 -1 -1 -1 -1   -4   -3  -5   -4   -3  -5  -5   -4   -4   -1  -5   -3   -1  -5  -4   -2   -3   -2  -4   -3   -4  -5  -4   -2   -4   -2  -5
#ST28 SU29 D30 D31 SU32 Ca Cb Cc Cd Ce Cf Cg Ch Da Db Dc Dd De Df Dg Dh Di Dj Age Attitude Points gender
#1   -4   -3  -4  -4   -3 -2 -4 -3 -4 -3 -2 -3 -4 -3 -4 -4 -5 -4 -2 -4 -3 -4 -4 -53      -37    -25     NA
#2   -2   -3  -3  -4   -5 -4 -4 -4 -5 -5 -3 -2 -4 -4 -3 -3 -4 -3 -2 -3 -3 -2 -4 -55      -31    -12     NA
#3   -5   -2  -4  -3   -5 -3 -5 -4 -4 -3 -4 -4 -2 -1 -4 -4 -1 -4 -1 -3 -1 -1 -5 -49      -25    -24     NA
#4   -3   -3  -4  -4   -3 -3 -4 -4 -4 -3 -4 -4 -3 -2 -4 -5 -2 -5 -1 -5 -4 -2 -5 -53      -35    -10     NA
#5   -5   -3  -3  -4   -4 -2 -4 -4 -3 -3 -3 -4 -4 -3 -4 -4 -4 -4 -2 -5 -5 -3 -3 -49      -37    -22     NA
#6   -4   -2  -5  -5   -3 -3 -5 -4 -4 -3 -4 -5 -4 -3 -5 -4 -4 -4 -3 -4 -3 -3 -5 -38      -38    -21     NA



#dimension of the data
dim(data1)
# 183 60


# Structure of the data
str(data1)
# Mostly likert scale (1-5) variables
# Includes also Age (positive integers) and gender (as a two level factor: F and M)
# Attitude = Global attitude toward statistics ~Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj
# Points=Exam points (0-33)
summary(data1)


# access the dplyr library
install.packages("dplyr")
library(dplyr)




head(keep_columns)
#To mutate the column from data1

data1 <- data1 %>% mutate(Asenne=Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj)

head(data1)
#to plot
plot(data1$Attitude,data1$Asenne)


# Quite "normal" distribution, more zero values than expected
hist(data1$Points)

# questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(data1, one_of(deep_questions))
data1$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(data1, one_of(surface_questions))
data1$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(data1, one_of(strategic_questions))
data1$stra <- rowMeans(strategic_columns)

# choose a handful of columns to keep
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")

head (keep_columns)

data1$Attitude
data1$Attitude / 10

data1 <- data1%>% mutate(attitude=Attitude/10)

learning2019 <- data1 %>% mutate(attitude=Attitude/10)
# select the 'keep_columns' to create a new dataset



# see the stucture of the new dataset

str(learning2019)


keep_columns <- c("gender","age","attitude", "deep", "stra", "surf", "Points")


colnames(learning2019)


colnames(learning2019)

learning2019 <-  learning2019 %>% select( gender, age=Age, attitude, deep, stra, surf, points=Points)

learning2019 <- filter (learning2019, points!=0)
colnames(learning2019)


getwd()

# Save created data to folder 'data' as an Excel worksheet
library(openxlsx)
write.xlsx(learning2019,file="learning2019.xlsx")


# Read the data back to R and check that structure and a few first observations look the same
library(readxl)

readtest <- read.xlsx("learning2019.xlsx")
readtest2 <- readxl::read_excel("learning2019.xlsx") %>%
  mutate_at(vars(gender), factor)


str(learning2019)
str(readtest)
str(readtest2)

head(learning2019)
head(readtest)
