library(ISLR)
data(College)
head(College)

# number 1 - splitting the college data set
#train <- sample(c(True", "False"),nrow(College),rep = T)
#test<-(!train)
train <- College[0:388,]
test <- College[390:777,]

# number 2 - linear model fit on training set and test error report.

college.lm <- lm(train$Apps ~ train$Private+train$Grad.Rate+train$Accept+train$Enroll+train$Top10perc+train$Top25perc+train$F.Undergrad+train$P.Undergrad+train$Outstate+train$Room.Board+train$Books+train$Personal+train$PhD+train$Terminal+train$S.F.Ratio+train$perc.alumni+train$Expend)
pred.lm <- predict(college.lm, test)
mean((pred.lm - test$Apps)^2)

# number 3 - Ridge
train.matrix <- model.matrix(Apps ~ ., data = train)
test.matrix <- model.matrix(Apps ~ ., data = test)
grid <- 10 ^ seq(4, -2, length = 100)
fit.ridge <- glmnet(train.matrix, train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train.matrix, train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
ridge <- cv.ridge$lambda.min
ridge
pred.ridge <- predict(fit.ridge, s = ridge, newx = test.matrix)
mean((pred.ridge - test$Apps)^2)

# number 4 - Lasso
fit.lasso <- glmnet(train.matrix, train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.matrix, train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
lasso <- cv.lasso$lambda.min
lasso
pred.lasso <- predict(fit.lasso, s = lasso, newx = test.matrix)
mean((pred.lasso - test$Apps)^2)

#number 5 - PCR
library(pls)
fit.pcr <- pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, test, ncomp = 10)
mean((pred.pcr - test$Apps)^2)

#number 6 - PLS
fit.pls <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")
pred.pls <- predict(fit.pls, test, ncomp = 10)
mean((pred.pls - test$Apps)^2)

