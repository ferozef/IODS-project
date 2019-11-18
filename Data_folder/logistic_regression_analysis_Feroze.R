## This datase has 35 variables and 382 observation. Out this 35 variables 17 are factors variables, 3 integer , 1 logcial and 14 numeric.
str(alc)
##'data.frame':	382 obs. of  35 variables:
#$ school    : Factor w/ 2 levels "GP","MS": 1 1 1 1 1 1 1 1 1 1 ...
#$ sex       : Factor w/ 2 levels "F","M": 1 1 1 1 1 2 2 1 2 2 ...
#$ age       : int  18 17 15 15 16 16 16 17 15 15 ...
#$ address   : Factor w/ 2 levels "R","U": 2 2 2 2 2 2 2 2 2 2 ...
#$ famsize   : Factor w/ 2 levels "GT3","LE3": 1 1 2 1 1 2 2 1 2 1 ...
#$ Pstatus   : Factor w/ 2 levels "A","T": 1 2 2 2 2 2 2 1 1 2 ...
#$ Medu      : int  4 1 1 4 3 4 2 4 3 3 ...
#$ Fedu      : int  4 1 1 2 3 3 2 4 2 4 ...
#$ Mjob      : Factor w/ 5 levels "at_home","health",..: 1 1 1 2 3 4 3 3 4 3 ...
#$ Fjob      : Factor w/ 5 levels "at_home","health",..: 5 3 3 4 3 3 3 5 3 3 ...
#$ reason    : Factor w/ 4 levels "course","home",..: 1 1 3 2 2 4 2 2 2 2 ...
#$ nursery   : Factor w/ 2 levels "no","yes": 2 1 2 2 2 2 2 2 2 2 ...
#$ internet  : Factor w/ 2 levels "no","yes": 1 2 2 2 1 2 2 1 2 2 ...
#$ guardian  : Factor w/ 3 levels "father","mother",..: 2 1 2 2 1 2 2 2 2 2 ...
#$ traveltime: num  2 1 1 1 1 1 1 2 1 1 ...
#$ studytime : num  2 2 2 3 2 2 2 2 2 2 ...
#$ failures  : num  0 0 2 0 0 0 0 0 0 0 ...
#$ schoolsup : Factor w/ 2 levels "no","yes": 2 1 2 1 1 1 1 2 1 1 ...
#$ famsup    : Factor w/ 2 levels "no","yes": 1 2 1 2 2 2 1 2 2 2 ...
#$ paid      : Factor w/ 2 levels "no","yes": 1 1 2 2 2 2 1 1 2 2 ...
#$ activities: Factor w/ 2 levels "no","yes": 1 1 1 2 1 2 1 1 1 2 ...
#$ higher    : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
#$ romantic  : Factor w/ 2 levels "no","yes": 1 1 1 2 1 1 1 1 1 1 ...
#$ famrel    : num  4 5 4 3 4 5 4 4 4 5 ...
#$ freetime  : num  3 3 3 2 3 4 4 1 2 5 ...
#$ goout     : num  4 3 2 2 2 2 4 4 2 1 ...
#$ Dalc      : num  1 1 2 1 1 1 1 1 1 1 ...
#$ Walc      : num  1 1 3 1 2 2 1 1 1 1 ...
#$ health    : num  3 3 3 5 5 5 3 1 1 5 ...
#$ absences  : num  5 3 8 1 2 8 0 4 0 0 ...
#$ G1        : num  2 7 10 14 8 14 12 8 16 13 ...
#$ G2        : num  8 8 10 14 12 14 12 9 17 14 ...
#$ G3        : num  8 8 11 14 12 14 12 10 18 14 ...
#$ alc_use   : num  1 1 2.5 1 1.5 1.5 1 1 1 1 ...
#$ high_use  : logi  FALSE FALSE TRUE FALSE FALSE FALSE ...

##question number 3
#to create models
my_model_1 <- glm(high_use ~ failures + absences+ sex+romantic, data = alc, family = "binomial")
summary(my_model_1)


##we ran a binary logistic regression model with high alocohol usage as the dependent variable
##four variable of interest were taken as predictors of the model(failures,absences, sexM,romantic)
##out the four predicted variables, 3 variables failures,absences, and sexM showed a positive significant association with a outcome variable.


# predict() the probability of high_use
probabilities <- predict(my_model_1, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability >0.5 )

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table (high_use = alc$high_use, prediction = alc$prediction)
##prediction
##high_use FALSE TRUE
##FALSE   256   12
##TRUE     84   30

##The table above shows that the true negatives in our model regarding high_use
##in a significantly high proportion(256/268*100=95.52%)this denote a very high specificity and 
##lose chance of type two error in the dataset regarding high alocohol usage.



library(dplyr); library(ggplot2)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use , col = prediction ) )

# define the geom as points and draw the plot
g+geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)%>%prop.table()%>%addmargins()

##prediction
##high_use      FALSE       TRUE        Sum
##FALSE    0.67015707 0.03141361 0.70157068
##TRUE     0.21989529 0.07853403 0.29842932
##Sum      0.89005236 0.10994764 1.00000000


# produce summary statistics by group
alc %>% group_by(sex) %>% summarise(count = n())

alc %>% group_by(sex) %>% summarise(count = n(), mean_grade = mean(G3))

alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

# initialise a plot of high_use and absences

g1 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))
# define the plot as a boxplot and draw it

g1 + geom_boxplot() + ylab("grade")+ ggtitle("Student absences by alcohol consumption and sex")

##the box plot shows that the males of significantly lower  greades have a higher prevalence in terms of high alocohol usage compared to female.
##Regarding non usage of alocohol, the difference in between the gender is apparently trivial. 

##question number 5
# find the model with glm()
my_model_1 <- glm(high_use ~ failures + absences + sex+ romantic, data = alc, family = "binomial")

# compute odds ratios (OR)
OR <- coef(my_model_1) %>% exp

# compute confidence intervals (CI)
CI <- confint(my_model_1) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

##odd ratio and confidence ratio of significant predictors in previous models
##                     OR      2.5 %    97.5 %
##  (Intercept) 0.1606994 0.09898897 0.2519302
##  failures    1.5740272 1.08568128 2.3050629
##  absences    1.0994355 1.05336109 1.1527402
##  sexM        2.5301750 1.58131504 4.1005152
##  romanticyes 0.7799690 0.46257612 1.2936660

##the variable of high failures had 1.56 times higher odds of high alocohol usage(95% CI= 1.08 to 2.3)
##the variable of absences  had 1.09 times higher odds of high alocohol usage(95% CI= 1.05 to 1.15)
##the variable of sex had 2.53 times higher odds of high alocohol usage(95% CI= 1.58 to 4.1)


## Question 6. 2x2 cross tabulation of predictions versus the actual values 
alc%>% group_by(sex) %>% summarise(count = n(), mean_absences = mean(absences), mean_failures = mean(failures))

##Performing 10-fold cross-validation on my_model_1

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data

loss_func(class = alc$high_use, prob = alc$probability)
# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = my_model_1, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]


##Average number of wrong predicition in cross validation 0,2565, this shows that our model has realtive small error rate as shown by number of wrong predicition through cross-valiadation.


getwd()
list.files()
