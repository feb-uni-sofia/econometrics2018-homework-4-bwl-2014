## Homework 4, Problem 1

library(dplyr)
library(ggplot2)
## Read the data
houseWork <- read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/houseWork.csv')
str(houseWork)

## a)
table(houseWork$sex)
## b)
work_by_sex <- group_by(houseWork, sex)
summarise(work_by_sex, mean=mean(hours))
## c)
houseWork1 <- within(houseWork, {
female <-ifelse(sex=="f", 'TRUE', 'FALSE')
male <- ifelse(sex=="m", 'TRUE', 'FALSE')
})
houseWork1

## d)
fit <- lm(hours~female, data=houseWork1)
summary(fit)
## e)
#В линейната регресия проверяваме дали средните часове домакинска работа 
#зависят от това си мъж или жена. Оценката за коефициентът пред променливата
#за жените е отрицателен, т.е. не е задължително жените да прекарват повече
#време в домакинска работа. А п-верта е с много малка стойност. От тук можем да 
#направим извод, че вероятността да сгрешим, ако отхвърлим хипотезата
#е много малка.

## f)
t.test(hours~female, data = houseWork1, alternative = 'less')
## g)
pt(-48.929, df=10853)
## h)
#Ако приемем, че може да съществува стойност п=0 (което не мислим, че е възможно), 
#трябва да отхвърлим хипотезата.
## i)

## j)
fit <- lm(hours~female, data=houseWork1)
fit1 <- lm(hours~male, data=houseWork1)
fitAll <- lm(hours ~ female + male, data=houseWork1)
summary(fitAll)
