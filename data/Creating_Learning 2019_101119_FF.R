#Feroze_analysis
#10.11.19
# Chapter 2: Regression and model validation--> Data wrangling and data Ananlysis, the Data wrangling exercise create an analysis dataset for the Analysis exercise. During the Analysis exercise will we explore the data, perform analysis and interpret the results.



#get directory
getwd()

data1 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header = TRUE)
head((data1))
#output of dataAa Ab Ac Ad Ae Af ST01 SU02 D03 ST04 SU05 D06 D07 SU08 ST09 SU10 D11 ST12 SU13
#1  3  1  2  1  1  1    4    2   4    4    2   4   4    3    3    2   3    3    3
#2  2  2  2  2  1  1    4    2   4    4    4   2   3    4    4    1   4    1    3
#3  4  1  1  1  1  1    3    1   4    4    2   3   4    1    3    1   4    4    2
#4  4  2  3  2  1  1    3    3   4    4    3   4   4    2    3    1   3    3    2
#5  3  2  2  1  2  1    4    2   5    3    4   4   4    3    4    2   4    2    3
#6  4  2  1  1  1  1    4    3   5    4    3   5   5    4    4    1   5    3    1
#D14 D15 SU16 ST17 SU18 D19 ST20 SU21 D22 D23 SU24 ST25 SU26 D27 ST28 SU29 D30
#1   4   3    2    3    2   4    2    3   3   2    2    4    4   4    4    3   4
#2   2   3    4    4    2   3    1    2   2   3    4    2    4   2    2    3   3
#3   4   2    3    3    1   4    3    2   4   3    3    4    4   3    5    2   4
#4   4   3    2    3    1   3    3    3   3   3    2    3    2   3    3    3   4
#5   4   3    3    4    1   4    3    2   3   3    4    4    3   3    5    3   3
#6   5   4    2    3    2   4    3    4   5   4    2    4    2   5    4    2   5
#D31 SU32 Ca Cb Cc Cd Ce Cf Cg Ch Da Db Dc Dd De Df Dg Dh Di Dj Age Attitude
#1   4    3  2  4  3  4  3  2  3  4  3  4  4  5  4  2  4  3  4  4  53       37
#2   4    5  4  4  4  5  5  3  2  4  4  3  3  4  3  2  3  3  2  4  55       31
#3   3    5  3  5  4  4  3  4  4  2  1  4  4  1  4  1  3  1  1  5  49       25
#4   4    3  3  4  4  4  3  4  4  3  2  4  5  2  5  1  5  4  2  5  53       35
#5   4    4  2  4  4  3  3  3  4  4  3  4  4  4  4  2  5  5  3  3  49       37
#6   5    3  3  5  4  4  3  4  5  4  3  5  4  4  4  3  4  3  3  5  38       38
#Points gender
#1     25      F
#2     12      M
#3     24      F
#4     10      M
#5     22      M
#6     21      F1
# deminsion of the data
dim(data1)
# 183 Rows, 60 columns
summary(data1)
# Get structure of data1
str(data1)
#choose of columns for analyses
library(dplyr)
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")
head(keep_columns)
data1 <- data1 %>% mutate(Asenne=Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj)
plot(data1$Attitude,data1$Asenne)
# Check the distribution of exam points
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


# Create column 'attitude' by scaling the column "Attitude"
data1$Attitude
data1$Attitude/10
data1 <- data1 %>% mutate(attitude=Attitude/10)
learning2019 <- data1 %>% mutate(attitude=Attitude/10)

# check the structure of new dataset
str(learning2019)
keep_columns <-c("gender", "Age","attitude", "deep", "stra", "surf", "Points")
learning2019 <- learning2019 %>% select (gender, age=Age, attitude, deep, stra, surf, points=Points)
learning2019 <- filter(learning2019, points !=0)

colnames(learning2019)

getwd()
library(openxlsx)


write.xlsx(learning2019,file="learning2019.xlsx")


# Read the data back to R and check that structure and a few first observations look the same
readtest <- read.xlsx("learning2019.xlsx")

str(learning2019)
str(readtest)

head(learning2019)
head(readtest)

#dimensions of the data
dim(learning2019)
#166 rows  7 columns

#structure of data

str(learning2019)


#'data.frame':	166 obs. of  7 variables:
#$ gender  : Factor w/ 2 levels "F","M": 1 2 1 2 2 1 2 1 2 1 ...
#$ age     : int  53 55 49 53 49 38 50 37 37 42 ...
#$ attitude: num  3.7 3.1 2.5 3.5 3.7 3.8 3.5 2.9 3.8 2.1 ...
#$ deep    : num  3.58 2.92 3.5 3.5 3.67 ...
#$ stra    : num  3.38 2.75 3.62 3.12 3.62 ...
#$ surf    : num  2.58 3.17 2.25 2.25 2.83 ...
#$ points  : int  25 12 24 10 22 21 21 31 24 26 ...


# Discription of the data
# Our dataset consists of seven variables, the vraible gender is a categorical (factor) binary variable with two categories of male and female.
#Our second variable is age which is taken as an integar variable (positive whole number values). 
#The thrid variable attiude is a numerical variable (continuous variable).
# Our fourth, fifth and six  variable are deep learning, strategicand surface variable which are again nemarical (continuous)  variable. 
#Our seventh variable is points which is a positive integar variable.

library(ggplot2)


p1 <- ggplot(learning2019, aes(x = attitude, y = stra, col=gender))

p2 <- p1 + geom_point()
p2

# initialize plot with data and aesthetic mapping
p1 <- ggplot(learning2019, aes(x = attitude, y = points, col=gender))

# define the visualization type (points)
p2 <- p1 + geom_point()

# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm") + ggtitle("Student's attitude versus exam points")
p3
# add a main title and draw the plot

p4 <- ggtitle("Student's attitude versus exam points")
p4
