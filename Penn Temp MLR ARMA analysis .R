# PA TEMP Analysis
library(patemp)

data(patemp)
#MLR with 1 categorical variable (month) and ARMA errors
fit = lm(temp~factor(month), data = df_patemp)
summary(fit)
newdata = data.frame(month = seq(1,12,1))
predict(fit, newdata = newdata)

aic5.wge(fit$residuals) # AR(1)

est1 = est.arma.wge(fit$residuals, p = 1)

forecasts = fore.arma.wge(fit$residuals,phi = est1$phi,lastn = FALSE,n.ahead = 12)
fors = preds + forecasts$f

plot(fors,type = "l")


#Rolling ASE

trainingSize = 30
horizon = 12
ASEHolder = numeric()

for( i in 1:(180-(trainingSize + horizon) + 1))
{
  
  forecasts = fore.arma.wge(fit$residuals[i:(i+(trainingSize-1))],phi = est1$phi,lastn = FALSE,n.ahead = horizon)
  #fors = preds + forecasts$f
  fors = preds
  
  ASE = mean((patemp[(trainingSize+i):(trainingSize+ i + (horizon) - 1)] - fors)^2)
  
  ASEHolder[i] = ASE
}

ASEHolder
hist(ASEHolder)
WindowedASE = mean(ASEHolder)

summary(ASEHolder)
WindowedASE



#Plot 12 months into the future
dev.off()
plot(seq(1,180,1), patemp, type = "l",xlim = c(0, 200), ylab = "Temperature", main = "Pennsylvania Temperature")
lines(seq(181,192,1), fors, type = "l", col = "red")





