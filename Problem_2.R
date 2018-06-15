
## Homework 4, Problem 2
crime <- read.delim('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/crime.csv', stringsAsFactors = FALSE)
str(crime)

## a)
fit <- lm(C~HS, data=crime)
summary(fit)
#Коефициентът пред показателя за завършено средно образование е положителен,
#което показва, че има връзка между криминалността и степента на завървено образование.
#Вероятността нулевата хипотеза да бъде отхвърлена с грешка е много малка, което
#се вижда от п-верта.
## b)
install.packages('psych')
library(psych)
pairs(~HS+I+C+U, data=crime, main="Simple Scatterplot Matrix")
## c)
fit1 <- lm(C~HS+U, data=crime)
summary(fit1)
#Тук наблюдаваме отрицателна стойност за коефициента пред завършеното
#средно образование, а п-верта има стойност по-голяма от 0.05, което означава, че
#не отхвърляме хипотезата, че криминалността зависи от средното образование.
#Наблюдавайки коефициента, показващ дали извършителят живее в градска среда
#има значение, виждаме, че отхвърляме хипотезата, че има връзка между двете събития
#с много малка вероятност за грешка.

## d)

## e)
fit2 <- lm(C~HS+U+I, data=crime)
summary(fit2)
f.test<-var.test(crime$HS, crime$I, alternative="two.sided")
f.test
