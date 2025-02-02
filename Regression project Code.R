# Install and load the R package
library(readxl)
library(dplyr)
library(statmod)
library(GLMsData)
library(tidyverse)

# Specify the Excel file path
file_path <-("F:\\Applied Statistics Course - ISU\\Regression & Time Series Analysis\\Project\\Dataset\\stroke.xlsx")
# Read the Excel file
st <- read_excel(file_path)

# Explore the data
head(st)
str(st)

# Convert the categorical variable to a factor
st$gender <- as.factor(st$gender)
st$ever_married <- as.factor(st$ever_married)
st$work_type <- as.factor(st$work_type)
st$Residence_type <- as.factor(st$Residence_type)
st$smoking_status <- as.factor(st$smoking_status)

levels(st$gender)
levels(st$ever_married)
levels(st$work_type)
levels(st$Residence_type)
levels(st$smoking_status)

num_zeros <- sum(st$stroke == 0)
num_zeros
num_ones <- sum(st$stroke ==1)
num_ones

# Explore the data
head(st)
str(st)

# Create a binary factor for multi levels variable
gd0 <- c("Male")
st$gender <- factor(ifelse(st$gender %in% gd0, 0, 1))
print(st)

wt0 <- c("Private")
st$work_type <- factor(ifelse(st$work_type %in% wt0, 0, 1))
print(st)

sm0 <- c("smokes")
st$smoking_status <- factor(ifelse(st$smoking_status %in% sm0, 0, 1))
print(st)


## Model Analysis
st1 <- glm(stroke ~ age + gender + hypertension + heart_disease + ever_married + work_type
           + Residence_type + avg_glucose_level + bmi + smoking_status, data = st,
           family = binomial(link ="logit"))
summary(st1)

st2 <- glm(stroke ~ age + gender + hypertension + heart_disease + ever_married + work_type
           + Residence_type + avg_glucose_level + bmi + smoking_status, data = st,
           family = binomial(link ="probit"))
summary(st2)

st3 <- glm(stroke ~ age + gender + hypertension + heart_disease + ever_married + work_type
           + Residence_type + avg_glucose_level + bmi + smoking_status, data = st,
           family = binomial(link ="cloglog"))
summary(st3)


st4 <- glm(stroke ~ age + hypertension * heart_disease + work_type * avg_glucose_level
           + smoking_status, data = st,
           family = binomial(link ="probit"))
summary(st4)

## for,back,step
initial_model <- glm(stroke ~ 1, data = st, family = binomial(link="probit"))
full_model <- glm(stroke ~ age + gender + hypertension + heart_disease + ever_married + work_type
                  + Residence_type + avg_glucose_level + bmi + smoking_status, data = st,
                  family = binomial(link ="probit"))

st5 <- step(initial_model, direction = "forward",
            scope = list(lower=initial_model, upper = full_model))
st6 <- step(full_model, direction = "backward",
            scope = list(lower=initial_model, upper = full_model))
st7 <- step(initial_model, direction = "both",
            scope = list(lower=initial_model, upper = full_model))
summary(st5)
summary(st6)
summary(st7)

## outlying detection
cook_distances <- cooks.distance(st5)
cook_distances

inf.mea = influence.measures(st5)
inf.mea

cook_distances <- cooks.distance(st6)
cook_distances

inf.mea = influence.measures(st6)
inf.mea

cook_distances <- cooks.distance(st7)
cook_distances

inf.mea = influence.measures(st7)
inf.mea

##multicollinearity issue
library(car)
vif(st5, type = "terms")

library(car)
vif(st6, type = "terms")

library(car)
vif(st7, type = "terms")

##quantile residual
qqnorm(qresid(st5),
       ylim = c(-3, 3), 
       xlim = c(-3,3))
qqline(qresid(st5))


fitted_values <- st5$fitted.values
fitted_values
quantile_resid <- qresid(st5)
quantile_resid

## fitted values vs quantile residuals
plot(fitted_values, quantile_resid,
     xlab = "Fitted Values",
     ylab = "Quantile Residuals",
     main = "Quantile Residuals vs Fitted Values")
abline(h = 0, col = "blue")

##quantile residual
qqnorm(qresid(st6),
       ylim = c(-3, 3), 
       xlim = c(-3,3))
qqline(qresid(st6))


fitted_values <- st6$fitted.values
fitted_values
quantile_resid <- qresid(st6)
quantile_resid

## fitted values vs quantile residuals
plot(fitted_values, quantile_resid,
     xlab = "Fitted Values",
     ylab = "Quantile Residuals",
     main = "Quantile Residuals vs Fitted Values")
abline(h = 0, col = "blue")


##quantile residual
qqnorm(qresid(st7),
       ylim = c(-3, 3), 
       xlim = c(-3,3))
qqline(qresid(st7))


fitted_values <- st7$fitted.values
fitted_values
quantile_resid <- qresid(st7)
quantile_resid

## fitted values vs quantile residuals
plot(fitted_values, quantile_resid,
     xlab = "Fitted Values",
     ylab = "Quantile Residuals",
     main = "Quantile Residuals vs Fitted Values")
abline(h = 0, col = "blue")



## Confidence Interval
confint(st5,level= 0.95)

confint(st6,level= 0.95)

confint(st7,level= 0.95)

## Coefficient
printCoefmat(coef(summary(st5)))

printCoefmat(coef(summary(st6)))

printCoefmat(coef(summary(st7)))


## likelihood ratio test
lrt_result <- anova(st5, st2, test = "Chisq")
print("Likelihood Ratio Test Result:")
print(lrt_result)

lrt_result <- anova(st6, st2, test = "Chisq")
print("Likelihood Ratio Test Result:")
print(lrt_result)

lrt_result <- anova(st7, st2, test = "Chisq")
print("Likelihood Ratio Test Result:")
print(lrt_result)


# Score test
z.score <- glm.scoretest(st5, st$stroke)
P.score <- 2*(1-pnorm(abs(z.score)))
c(z.score, P.score)

z.score <- glm.scoretest(st6, st$stroke)
P.score <- 2*(1-pnorm(abs(z.score)))
c(z.score, P.score)

z.score <- glm.scoretest(st7, st$stroke)
P.score <- 2*(1-pnorm(abs(z.score)))
c(z.score, P.score)

## Goodness of Fit Test
library(ResourceSelection)
hoslem_result <- hoslem.test(st5$y, fitted(st5), g = 10)
print(hoslem_result)

hoslem_result <- hoslem.test(st5$y, fitted(st6), g = 10)
print(hoslem_result)

hoslem_result <- hoslem.test(st5$y, fitted(st7), g = 10)
print(hoslem_result)


## Odds Ratio
odds_ratios <- exp(coef(st5))
print(odds_ratios)

odds_ratios <- exp(coef(st6))
print(odds_ratios)

odds_ratios <- exp(coef(st7))
print(odds_ratios)







