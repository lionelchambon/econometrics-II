###Econometrics II: Problem Set 1
###By Lionel Chambon

install.packages("pacman")
install.packages("tidyverse")
install.packages("lmtest")
install.packages("caret")
install.packages("corrplot")
install.packages("kableExtra")
install.packages("haven")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("tinytex")

library(pacman)
library(lmtest)
library(caret)
library(corrplot)
library(tidyverse)
library(kableExtra)
library(haven)
library(rmarkdown)
library(knitr)
library(tinytex)


### PROBLEM 1

###Here, I load some packages that I used during a data analysis project in undergrad.

PS_1_data <- read_dta("/Users/lionelchambon/Desktop/LIONEL/LIONEL_edu/MASTER/M1/Cours/Econometrics II/Data/ee2002ext.dta")

###The data is now loaded.

###Before starting, I remove any empty observations.

PS_1_data <- PS_1_data %>% na.omit(PS_1_data)

### Question 1

summary(PS_1_data)


### Question 2 

monthly_wage_data <- PS_1_data %>% select(salfr)

summary(monthly_wage_data)

###Question 3

educational_attainment <- PS_1_data %>% select(ddipl1) %>% arrange(ddipl1)

head(educational_attainment) %>% kable() %>% kable_material(c("striped", "hover"))

### I found this command by doing some googling and ending up here: 
### https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
###Why do I have to use head? When using summarize, I get the error message "data too long"

### Question 4 

PS_1_labelled_data_education <- PS_1_data %>%
  mutate(Educational_Attainment = case_when(ddipl1 == 1 ~ "No degree",
                                     ddipl1 == 2 ~ "Middle school degree",
                                     ddipl1 == 3 ~ "High school vocational degree",
                                     ddipl1 == 4 ~ "High school degree",
                                     ddipl1 == 5 ~ "Some college",
                                     ddipl1 == 6 ~ "College plus",
                                     ddipl1 == 7 ~ "Still in school")) %>%
  select(Educational_Attainment, ddipl1) 

head(PS_1_labelled_data_education) %>% kbl() %>%
  kable_material(c("striped", "hover"))

### Question 5

meanw_byeducation <- PS_1_data %>%
  mutate(Educational_Attainment = case_when(ddipl1 == 1 ~ "No degree",
                                     ddipl1 == 2 ~ "Middle school degree",
                                     ddipl1 == 3 ~ "High school vocational degree",
                                     ddipl1 == 4 ~ "High school degree",
                                     ddipl1 == 5 ~ "Some college",
                                     ddipl1 == 6 ~ "College plus",
                                     ddipl1 == 7 ~ "Still in school")) %>%
  group_by(Educational_Attainment) %>%
  summarise(Mean_Income = mean(salfr)) %>%
  arrange(Mean_Income) ### When/why do we use arrange?

 
meanw_byeducation %>% kable() %>% kable_material(c("striped", "hover"))

### Question 6

Table_Schooling <- PS_1_data %>% rename(Age_at_Graduation = adfe) %>% select(Age_at_Graduation, ddipl1)

head(Table_Schooling) %>% kable() %>% kable_material(c("striped", "hover"))

### 0 refers to people with zero schooling, 99 to people that are still studying.

### Question 7

meanw_bygraduation <- PS_1_data %>%
  mutate(Educational_Attainment = case_when(ddipl1 == 1 ~ "No degree",
                                            ddipl1 == 2 ~ "Middle school degree",
                                            ddipl1 == 3 ~ "High school vocational degree",
                                            ddipl1 == 4 ~ "High school degree",
                                            ddipl1 == 5 ~ "Some college",
                                            ddipl1 == 6 ~ "College plus",
                                            ddipl1 == 7 ~ "Still in school")) %>%
  group_by(Educational_Attainment) %>%
  summarise(Mean_GradAge = mean(adfe)) %>%
  arrange(Mean_GradAge)

meanw_bygraduation %>% kable() %>% kable_material(c("striped", "hover"))

### Question 8

trimmed_data <- PS_1_data %>% rename(Graduation_Age = adfe) %>% rename(Monthly_Wages = salfr) %>% select(Monthly_Wages, Graduation_Age) %>% filter(Graduation_Age > 0, Graduation_Age < 99)

scatter_1 <-  ggplot(trimmed_data, aes(x=Graduation_Age, y=Monthly_Wages, col=Graduation_Age)) + geom_point()

show(scatter_1)

### Question 9 + 10

trimmed_data_log <- trimmed_data %>% mutate(log_Wages = log(Monthly_Wages)) %>% filter((log_Wages) > 0) 

scatter_2 <-  ggplot(trimmed_data_log, aes(x=Graduation_Age, y=log_Wages, col=Graduation_Age)) + geom_point()

show(scatter_2)

### Question 11

quantile(trimmed_data_log$log_Wages, probs = seq(0,1, by=0.01))

First_percentile = 7.090077
Ninety_nine_th_percentile = 10.373491

### Perhaps some form of trimming could be used to eliminate some of the outliers? The data also looks
### very dense, so there is no clear tendency/relationship between our variables of interest visible?
### However, that could also be an issue with the design of the plot...

### Question 12 - Repeat exercise with smaller intervals

quantile(trimmed_data_log$log_Wages, probs = seq(0,1, by=0.005))

Point_five_percentile = 6.684612
Ninety_nine_th__five_percentile = 10.596635 

### The 0,5th percentile means that 99,5% of respondents/observations have an income superior to 6.684612,
### while the 99,5th percentile indicates that 0,5% of observations have an income greater or equal
### to 10.596635.

### Question 13

###Doesn't R save the values automatically in the environment?


### Question 14

Variance_Covariance_lwadfe <-  trimmed_data_log %>%
  filter(Graduation_Age < 99, Graduation_Age > 0,
         log_Wages > Point_five_percentile, log_Wages < Ninety_nine_th__five_percentile) %>%
  summarise(log_Wages_var = var(log_Wages), Graduation_Age_var = var(Graduation_Age), 
            cov(log_Wages, Graduation_Age))
Variance_Covariance_lwadfe %>% kable() %>% kable_material(c("stripped", "hover"))


### Question 15

###We know that the TSS is the squared sum of the difference between y_i and y_bar 

TSS <- trimmed_data_log %>%
  filter(Graduation_Age < 99, Graduation_Age > 0,
         log_Wages > Point_five_percentile, log_Wages < Ninety_nine_th__five_percentile) %>%
  summarise(TSS = (sum((log_Wages-mean(log_Wages))^2)))

TSS %>% kable() %>% kable_material(c("striped", "hover"))            

### Question 16

###The OLS estimator between lw and adfe is given by the quotient of the covariance and var_adfe

OLS_estimator <- Variance_Covariance_lwadfe %>%
  summarize(OLS_estimator = `cov(log_Wages, Graduation_Age)`/Graduation_Age_var)

OLS_estimator %>% kable() %>% kable_material(c("striped", "hover"))   

### Question 17

###Lets start the exercise with a filter:

filtered_data_log <- trimmed_data_log %>%
  filter(Graduation_Age < 99, Graduation_Age > 0,
         log_Wages > Point_five_percentile, log_Wages < Ninety_nine_th__five_percentile)

head(filtered_data_log) %>% kable() %>% kable_material(c("striped", "hover"))  

filtered_regression = lm(log_Wages ~ Graduation_Age, data=filtered_data_log)
summary(filtered_regression)

### Repeat the exercise with no filter:

unfiltered_data_log <- trimmed_data_log %>%
  filter(Graduation_Age < 99, Graduation_Age > 0)

head(filtered_data_log) %>% kable() %>% kable_material(c("striped", "hover"))  

unfiltered_regression = lm(log_Wages ~ Graduation_Age, data=unfiltered_data_log)
summary(unfiltered_regression)

###Plotting both results for comparison:

filtered_scatter <- ggplot(filtered_data_log, aes(x=Graduation_Age, y=log_Wages, col=Graduation_Age)) + geom_point() +
  geom_smooth(method=lm , color="black") + ggtitle("Filtered") + theme(plot.title = element_text(hjust = 0.5))

show(filtered_scatter)

unfiltered_scatter <- ggplot(unfiltered_data_log, aes(x=Graduation_Age, y=log_Wages, col=Graduation_Age)) + geom_point() +
  geom_smooth(method=lm , color="black") + ggtitle("Unfiltered") + theme(plot.title = element_text(hjust = 0.5))

show(unfiltered_scatter)

###The filter appears to suppress outliers/extreme values.

### Question 18

### a) The regression table tells us that an additional unit of "Graduate_Age", that is, an additional 
### year of schooling, increases log income by 0.0487 units. (How do we convert?)

### b) 

TSS <- trimmed_data_log %>%
  filter(Graduation_Age < 99, Graduation_Age > 0,
         log_Wages > Point_five_percentile, log_Wages < Ninety_nine_th__five_percentile) %>%
  summarise(TSS = (sum((log_Wages-mean(log_Wages))^2)),
            SSE = sum((fitted(filtered_regression)-mean(log_Wages))^2),
            SSR = TSS-SSE)

TSS %>% kable() %>% kable_material(c("striped", "hover"))   

### c)

TSS <- trimmed_data_log %>%
  filter(Graduation_Age < 99, Graduation_Age > 0,
         log_Wages > Point_five_percentile, log_Wages < Ninety_nine_th__five_percentile) %>%
  summarise(TSS = (sum((log_Wages-mean(log_Wages))^2)),
            SSE = sum((fitted(filtered_regression)-mean(log_Wages))^2),
            SSR = TSS-SSE,
            R_2 = 1-SSR/TSS)

TSS %>% kable() %>% kable_material(c("striped", "hover"))

### Question 19

predicted_values <- predict(filtered_regression)
residuals <- residuals(filtered_regression)

hist(residuals)
hist(predicted_values)

cor(predicted_values, residuals)

###When using the command cor, the estimate is incredibly small, proving the desired result. However,
###when using cor.test, my p-value returns as 1, which can't be right?

### Question 20

hist(residuals)

###The symmetric, bell-shaped distribution of residuals is indicative of a normal distribution of
###the variance.

###END OF PROBLEM 1#############


###Problem 2


install.packages("readxl")
library(readxl)
PS_1_data_2 <- read_excel("/Users/lionelchambon/Desktop/LIONEL/LIONEL_edu/MASTER/M1/Cours/Econometrics II/Data/problem_2_ps_1.xlsx")
head(PS_1_data_2)

### Question 1

testscores_regression <- lm(GPA ~ ACT, data=PS_1_data_2)
summary(testscores_regression)

ggplot(PS_1_data_2, aes(x = ACT, y = GPA)) + 
  geom_point() +
  stat_smooth(method = "lm")

###Our estimate beta_hat is 0.10220, our intercept is 0.56813.

###The intercept tells us the value of the variable of interest with no treatment - think of consumption
###if income is zero. The interpretation here is that the the student with the lowest ACT score in the sample.
###had a GPA of 2.8, but we cannot make general conclusions due to the small sample size.

###Since this is a linear model, we can simply multiply the ACT-coefficient by five:

five_point_increase = 0.10200*5
show(five_point_increase)

### Question 2

table_residual_fitted <- PS_1_data_2 %>% 
  mutate(Residual_2 = resid(testscores_regression),
         Fitted_2 = fitted(testscores_regression))  %>% 
  select(ACT, Residual_2, Fitted_2, GPA)

table_residual_fitted %>% kbl() %>%
  kable_material(c("striped", "hover"))

###Verifying the sum:

Residuals_2 <- resid(testscores_regression)
sum_residuals = sum(Residuals_2)
show(sum_residuals)

###This value is very close to zero, CQFD.

### Question 3

predicted_score <- 0.56813+(0.10220*20)
show(predicted_score)

### Question 4

###To find the variation, we need to compute the R_2:

TSS <- PS_1_data_2 %>%
  summarise(TSS = (sum((GPA-mean(GPA))^2)),
            SSE = sum((fitted(testscores_regression)-mean(GPA))^2),
            SSR = TSS-SSE,
            R_2 = 1-SSR/TSS)

TSS %>% kable() %>% kable_material(c("striped", "hover"))   

###This tells us that roughly 58% of GPA variation in this sample is explained by ACT scores.


###END OF PS 1: EMPIRICAL#################################################################

