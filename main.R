library(rpart)
library(rpart.plot)
library(readr)
data.df <- read_csv("Bank_Loan_Granting.csv")
data.df <- data.df[ , -c(1, 7)]
#data.df$CCAvg <- round((as.numeric(substr(data.df$CCAvg, 1, 1))/
                          #as.numeric(substr(data.df$CCAvg, 3, 4))), 2)

set.seed(1)
train.index <- sample(c(1:dim(data.df)[1]), dim(data.df)[1]*0.7)
train.df <- data.df[train.index, ]
valid.df <- data.df[-train.index, ]

cart <- rpart(Personal_Loan ~ ., data = train.df, control = rpart.control(cp = 0.005))

#  График изменения относительных ошибок от числа узлов дерева
plotcp(cart)
with(cart, {lines(cptable[, 2] + 1, cptable[, 3],
                  type = "b", col = "red")
  legend(locator(1), c("Ошибка обучения",
                       "Ошибка крос-проверки (CV)", "min(CV ошибка)+SE"),
         lty = c(1, 1, 2), col = c("red", "black", "black"), bty = "n") })

# строим дерево
prp(cart, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
length(cart$frame$var[cart$frame$var == "<leaf>"])

#прогнозы
predict_rpart  <- predict(cart,  valid.df)
predict_rpart <- data.frame(Personal_Loan_Pred = predict_rpart,
                            Personal_Loan = valid.df$Personal_Loan)
head(predict_rpart)

#матрица ошибок
pred_table <- table(predict_rpart$Personal_Loan, round(predict_rpart$Personal_Loan_Pred))
pred_table
sum(diag(pred_table)) / nrow(valid.df)
