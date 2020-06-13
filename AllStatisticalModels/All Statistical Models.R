#Problem 3


setwd("E:/SRH/Sem 2/Analytics 2/data")
getwd()
data <- read.csv("credit.csv")
str(data)

data$creditworthy <- as.factor(data$creditworthy)

str(data)


#Logistic Regression

fit.LR <- glm(creditworthy ~., data, family = "binomial")
plot(fit.LR)

summary(fit.LR)



library("tidyverse")  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)

tidy(fit.LR)

predictLR <- predict(fit.LR, data, type = "response")


#LDA before Greedy

library(psych)

#Scatterplot, histogram, co-relation
pairs.panels(data[1:4],
             gap = 0,
             bg= c("gold","green")[data$creditworthy],
             pch=21,main="Creditworthy") #to show color grouping


library(MASS)
fit.LDA <- lda(creditworthy ~., data)
fit.LDA #displaying output
summary(fit.LDA)

predictLDA <- predict(fit.LDA, data)

ldahist(data = predictLDA$x[,1], g = data$creditworthy)


#LDA with greedy
library(klaR)
library(MASS)
fit.ldagw <- greedy.wilks(creditworthy ~ . , data)
fit.ldagw

#LDA after  greedy

fit.ldafinal <- lda(fit.ldagw$formula, data)

predictLDAfinal <- predict(fit.ldafinal, data)
ldahist(data = predictLDAfinal$x[,1], g = data$creditworthy)


#CART
library(rpart)
fit.cart <- rpart(creditworthy ~ ., data)
fit.cart


#pruning
p.rpartpruned <- prune(fit.cart, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(p.rpartpruned) 
text(p.rpartpruned)


#output
library(partykit)
plot(as.party(fit.cart))

# Tree with party
library(partykit)
fit.t <- ctree(creditworthy ~ ., data)
plot(fit.t)

library(partykit)
fit.random <- cforest(creditworthy ~ .,data) 

#Prediction required for Data

nd <- data.frame(age=c(41, 43),
                 income=c(2500, 5000),
                children=c(1, 3),
               car=c(1, 1))


str(nd)



#Predictions with all

p.LR <- predict(fit.LR, nd, type = "response")
print("LR")
p.LR
p.LDA <- predict(fit.ldafinal, nd)
print("LDA")
p.LDA

p.ctree <- predict(fit.t,nd,
                           type = "response" ) #or prob
print("ctree")
p.ctree

p.rpart <- predict(fit.cart,nd)

print("rpart")
p.rpart



print("Random Forest")
p.RF<- predict(fit.random,nd,type = "response")
p.RF

print("Pruned")
p.pruned<- predict(p.rpartpruned,nd)
p.pruned
