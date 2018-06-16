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
## /score -0.5 for using character 'TRUE', 'FALSE' instead of logical TRUE, FALSE.
houseWork1 <- within(houseWork, {
    female <-ifelse(sex=="f", 'TRUE', 'FALSE')
    male <- ifelse(sex=="m", 'TRUE', 'FALSE')
})
houseWork1

## d)
fit <- lm(hours~female, data=houseWork1)
summary(fit)
## e)
## /score -0.5 
#В линейната регресия проверяваме дали средните часове домакинска работа 
#зависят от това си мъж или жена. Оценката за коефициента пред променливата
#за жените е отрицателен, т.е. не е задължително жените да прекарват повече
#време в домакинска работа. А п-верта е с много малка стойност. От тук можем да 
#направим извод, че вероятността да сгрешим, ако отхвърлим хипотезата
#е много малка. (коя хипотеза)??

## f)
t.test(hours~female, data = houseWork1, alternative = 'less')
## g)

## /score -2 грешен p-Wert. p-wert вече е изчислен в t.test
## и е на практика 1. Правилно тествате хипотезата, че mu_f <= mu_m
## или еквивалентно H0: mu_m - mu_f => 0 срещу H1: mu_m - mu_f < 0.
## В такъв случай малки стойности на тест-статистиката противоречат
## на H0. Вероятността да наблюдаваме стойност на тест-статистиката при
## вярна нулева хипотеза e P(T < t | H0) = pt(48.929, df = 10853), което
## е много близо до 1.

## срещу алтернатива
## 
pt(-48.929, df=10853)
## h)
#Ако приемем, че може да съществува стойност п=0 (което не мислим, че е възможно), 
                                        #трябва да отхвърлим хипотезата.
## Коментар: вероятността, която сте получили тук не може да е точно равена на 0, защото т-разпределението
## има строго положителна плътност за всички реални числа. Просто тук е толкова
## малъка, че R я показва като 0.
## 

## i) ## /score -2

## j) ## /score -2
fit <- lm(hours~female, data=houseWork1)
fit1 <- lm(hours~male, data=houseWork1)
fitAll <- lm(hours ~ female + male, data=houseWork1)
summary(fitAll)
