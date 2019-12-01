##Feroze_analysis
##23.11.19
## Chapter 4:--> Data wrangling and data Ananlysis, the Data wrangling exercise create an analysis dataset for the Analysis exercise.



#loading  the “Human development” and “Gender inequality” datas into R

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)


gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")


#Datasets exploration:


str(hd)

str(gii)

dim(hd)
#[1] 195   8
dim(gii)
#[1] 195  10


summary(hd)

summary(gii)

#To find the names
names(hd)
#[1] "HDI.Rank"                              
#[2] "Country"                               
#[3] "Human.Development.Index..HDI."         
#[4] "Life.Expectancy.at.Birth"              
#[5] "Expected.Years.of.Education"           
#[6] "Mean.Years.of.Education"               
#[7] "Gross.National.Income..GNI..per.Capita"
#[8] "GNI.per.Capita.Rank.Minus.HDI.Rank"

names(gii)
#[1] "GII.Rank"                                    
#[2] "Country"                                     
#[3] "Gender.Inequality.Index..GII."               
#[4] "Maternal.Mortality.Ratio"                    
#[5] "Adolescent.Birth.Rate"                       
#[6] "Percent.Representation.in.Parliament"        
#[7] "Population.with.Secondary.Education..Female."
#[8] "Population.with.Secondary.Education..Male."  
#[9] "Labour.Force.Participation.Rate..Female."    
#[10] "Labour.Force.Participation.Rate..Male."


names(hd) <- c("HDI.Rank", "Country", "HDI", "LEB", "EYE", "MYE", "GNI", "GNI.CR") 
str(hd)
names(hd)
#[1] "HDI.Rank" "Country"  "HDI"      "LEB"      "EYE"      "MYE"     
#[7] "GNI"      "GNI.CR" 




names(gii) <- c("Gi.Rank", "Country", "Gii", "MMR", "ABR", "PRP", "SEC.F", "SEC.M", "labourF", "labourM")
str(gii)
names(gii)
#[1] "Gi.Rank" "Country" "Gii"     "MMR"     "ABR"     "PRP"     "SEC.F"  
#[8] "SEC.M"   "labourF" "labourM"

#To check that length of unique values in Country column is same as number of rows in the data set 
length(unique(gii$Country))

#[1] 195
gii

#To Mutate the “Gender inequality” data and create two new variables
#ratio of edu2F / edu2M (females/males with secondary education)
#ratio of labF / labM (females/males with labour force participation)
library(dplyr)
gii_Mutated_edu <- mutate(gii, edu2_FM_ratio = gii$SEC.F / gii$SEC.M)
gii_Mutated_lab <- mutate(gii, lab_FM_ratio = gii$labourF /  gii$labourM)

# To join the datasets and save the file
human <- inner_join(gii, hd, by = c('Country'))

dim(human)
glimpse(human)
write.csv(human, file= "human.csv")



# Exercise 5

human <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", stringsAsFactors = F)

getwd()
# read the data
read.csv(file ="C:/Users/ferozef/Documents/R/IODS-project/human.csv") %>% head()

# string manipulation
library(stringr)
colnames(human)
str(human$GNI)
human$GNI <- str_replace(human$GNI, pattern = ",", replace = "") %>% as.numeric

# Keep following variables: 
"Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F" 


keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F" )
human <- select(human, one_of(keep))

# filter out rows with missing values
human <- filter(human, complete.cases(human))
dim(human)

#162   9
# filter out regions, keep countries
human$Country
head(human, 10)
tail(human, 10)
# removing the regions

last <- nrow(human) -7
# filter, first feature is row, second is for columns
human <- human[1:last, ]
head(human, 10)
tail(human, 10)

# add countries as rownames
rownames(human) <- human$country
human <- select(human, -"Country")
head(human)
dim(human)

# save dataset
write.csv(human, file= "human.csv", row.names = TRUE)










