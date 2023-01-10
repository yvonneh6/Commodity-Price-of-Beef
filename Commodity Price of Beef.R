library(readxl)
library(astsa)
library(fpp2)
library(tseries)

# data
beef = read_excel("beef-120.xlsx")
train=beef[1:115,]
test=beef[-(1:115),]
tsplot(beef$Price) # original time series plot

par(mfrow=c(3,1))
tsplot(train$Price)
acf(train$Price)
pacf(train$Price)

# first differencing
diff.beef = diff(train$Price)
par(mfrow=c(3,1))
tsplot(diff.beef)
acf(diff.beef)
pacf(diff.beef)

# stationary tests
adf.test(train$Price) # not stationary
kpss.test(train$Price, null="Trend") # not trend stationary

# stationary tests
adf.test(diff(train$Price)) # stationary
kpss.test(diff(train$Price), null="Trend") # trend stationary

# spectral analysis
par(mfrow=c(2,1))
mvspec(diff(train$Price)) # periodogram
train.spec=mvspec(diff(train$Price), spans=c(3,3), log="no", taper=.5,lwd=2,col='blue')
abline(v=c(.035,0.1), lty=2)

# periodicity
train.spec$details

# 95% confidence interval
df=train.spec$df
U = qchisq(.025, df)
L = qchisq(.975, df)
train.spec$spec[4];train.spec$spec[12]
df*train.spec$spec[4]/L;df*train.spec$spec[4]/U

# second differencing
diff.beef10 = diff(diff(train$Price,10))
par(mfrow=c(3,1))
tsplot(diff.beef10)
acf(diff.beef10, lag.max = 50) # The seasonal ACF cuts off after lag 9. The nonseasonal ACF cuts off after lag 2.
pacf(diff.beef10,lag.max = 50) # The seasonal PACF tails off. The nonseasonal PACF tails off.

# stationary tests
adf.test(diff.beef10) # stationary
kpss.test(diff.beef10, null="Trend") # trend stationary

# ARMA(0,1,2)x(0,1,9)_10
sarima(train$Price,p=0, d=1, q=2, P=0, D=1, Q=9, S=10)
train.model.1 = sarima.for(train$Price,p=0, d=1, q=2, P=0, D=1, Q=9, S=10, n.ahead = 5)
accuracy(object = train.model.1$pred, x=test$Price)


# ARMA(0,1,1)x(0,1,1)_10
sarima(train$Price,p=0, d=1, q=1, P=0, D=1, Q=1, S=10)
train.model.1.1 = sarima.for(train$Price,p=0, d=1, q=1, P=0, D=1, Q=1, S=10, n.ahead = 5)
train.model.1.1$pred
accuracy(object = train.model.1.1$pred, x=test$Price)


