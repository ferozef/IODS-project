#Feroze_analysis
#10.11.19
# Chapter 2: Regression and model validation--> Data wrangling and data Ananlysis, the Data wrangling exercise create an analysis dataset for the Analysis exercise. During the Analysis exercise will we explore the data, perform analysis and interpret the results.


setwd( "C:/Users/ferozef/Documents/R/IODS-project/)


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


#The scatterplot shows regression lines for a linear regression model in which points are the dependant variable (shown on Y -axis), and the student attitudes as the independent variable (shown on x-axis). The regression lines are fitted for both the genders separately. Both the genders show that the student attitude is positively and linearly associated with the points


#Exploring data


pairs(learning2019[!names(learning2019) %in% c("gender")],col=learning2019$gender)


#this plot shows the scatterplots between the variables agr, attitude, deep, stra, surf, and points in a bivariate relationship, based on both gender's. To further eloborate the relationship in magnitude and direction, I will conduct complex visual analyses to calculate the bivariate correlations between the same variables. 


library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs()
ggpairs(learning2019, 
        mapping = aes(col = gender, alpha = 0.3), 
        lower = list(combo = wrap("facethist", bins = 20))
        )

# Highest correlation was observed between points and attiutude, (Overall corr: 0,437, Male: 0,451 and Female: 0,422).



#To reconfirm the linear trend between the points and attitude we will use linear regression lm method.

#Linear regression
#The highest correlation is between attitude and points, Cor: 0.4365245. Let’s take a closer look.

qplot(attitude, points, data = learning2019) + geom_smooth(method = "lm")

# the regression line shows and reconfirm a linear positive relationship between the varaibles attitude and points.


#The first linear regression model will be made between points as dependent variable and attitude, stratergic questions and surface learning  as independent variable.

my_model_1 <- lm(points ~ attitude+stra+surf, data = learning2019)
results <- summary(my_model)

knitr::kable(results$coefficients, digits=3, caption="Regression coefficients")

#Resuts: Attitude is significantly associated with points (p=0,03), the other two variables of stragegic and surface learning are not associated with points (p=0,117 stratergic learning and p= 0,466 surface learning)


my_model_2 <- lm(points ~ attitude, data = learning2019)
results <- summary(my_model)

knitr::kable(results$coefficients, digits=4, caption="Regression coefficients")

summary(my_model_1)

summary(my_model_2)



#Call:
#lm(formula = points ~ attitude, data = learning2019)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-16.9763  -3.2119   0.4339   4.1534  10.6645 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  11.6372     1.8303   6.358 1.95e-09 ***
#attitude      3.5255     0.5674   6.214 4.12e-09 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.32 on 164 degrees of freedom
#Multiple R-squared:  0.1906,	Adjusted R-squared:  0.1856 
#F-statistic: 38.61 on 1 and 164 DF,  p-value: 4.119e-09



#The multiple R squared values have not chnaged significantly from my model 1 to my model 2 (0.20 to 0.19 respectively) this shows that prioir incluslion of varaiable of strateric learning   and surface learning have a trivial effect on the varaince explained of the dependent variable of points in the linear regression model.

par(mfrow = c(2,2))
plot(my_model, which=c(1,2,5))


#Normal qplot show that the residuals are normal/linear distributed. The cook distance plot shows no significant otuliers(variables56, 35) in the model.
#The residuals Vs fitted plot shows three outliers variables 56, 145 and 35. 