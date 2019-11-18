##Feroze_analysis
##17.11.19
## Chapter 3: logistic Regression--> Data wrangling and data Ananlysis, the Data wrangling exercise create an analysis dataset for the Analysis exercise. During the Analysis exercise will we explore the data, perform analysis and interpret the results.

##Read both student-mat.csv and student-por.csv
math=read.table("student-mat.csv",sep=";",header=TRUE)
por=read.table("student-por.csv",sep=";",header=TRUE)


math_por=merge(math,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(math_por)) # 382 students

## common columns to use as identifiers

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

## join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by)



library(dplyr)
# print out the column names of 'math_por'

colnames(math_por)
# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data

glimpse(alc)

##Observations: 382
##Variables: 35

library(dplyr); library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(data = alc, aes(x = high_use))

# draw a bar plot of high_use by sex

g2+ geom_bar()+ facet_wrap("sex")

write.csv(alc, file= "alc.csv")



