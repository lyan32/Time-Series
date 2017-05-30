
library(TSA)
library(FSAdata)
library(forecast)
data("BloaterLH")



#module 1
BloaterLH$eggs
class(BloaterLH$eggs)
# win.graph(width=3,height=3,pointsize=8)
plot(y=BloaterLH$eggs,x=zlag(BloaterLH$eggs), xlab='eggs', main = "Scatter plot of BLoaterLH eggs ")

#module 2
#model=lm(BloaterLH$eggs-time(BloaterLH$eggs))


y= BloaterLH$eggs
x= zlag(BloaterLH$eggs)
index = 2:length(x) # Create an index to get rid of the first NA value in x
cor(y[index],x[index]) # Calculate correlation between numerical values in x and y

# high correlation between eggs of succeeding years
model1 = lm(BloaterLH$eggs~time(BloaterLH$eggs)) # label the model as model1
summary(model1)

plot(BloaterLH$eggs,type='o',ylab='eggs',xlab='time in years', main = "Time series plot for BloaterLH Eggs")
abline(model1) # add the fitted least squares line from model1


t = time(BloaterLH$eggs)
t2 = t^2
model1.1 = lm(BloaterLH$eggs~t+t2) # label the model as model1
summary(model1.1)

plot(ts(fitted(model1.1)), ylim = c(min(c(fitted(model1.1),
as.vector(BloaterLH$eggs))), max(c(fitted(model1.1),as.vector(BloaterLH$eggs)))),ylab='eggs' ,
     main = "Fitted quadratic curve to BloaterLH Eggs data")
lines(as.vector(BloaterLH$eggs),type="o")

#convert to ts
eggs <-ts(BloaterLH$eggs) 
class(Bloat)



#har.=harmonic(BloaterLH$eggs,0.001) # calculate cos(2*pi*t) and sin(2*pi*t)
#model4=lm(BloaterLH$eggs~har.)
#summary(model4)

#plot(ts(fitted(model4),freq=12,start=c(1964,1)),ylab='Temperature',type='l', ylim=range(c(fitted(model4),tempdub)),main="Fitted model to average monthly temperature series") # ylim ensures that the y axis range fits the raw data and the fitted values
#points(tempdub)


model2=lm(BloaterLH$eggs~time(BloaterLH$eggs))
summary(model2)

acf(BloaterLH$eggs)
pacf(BloaterLH$eggs)


win.graph(width=4.875,height=3,pointsize=8)


eggs.transform = BoxCox.ar(eggs,method = "yule-walker")
eggs.transform$ci

lambda = 0.45 # 0.001
BC.BloaterLH = (BloaterLH$eggs^lambda-1)/lambda

diff.BC.BloaterLH = diff(BC.BloaterLH)
plot(diff.BC.BloaterLH,type='o',ylab='Eggs', main='First difference of Eggs')
adf.test(diff.BC.BloaterLH)



acf(diff.BC.BloaterLH)
pacf(diff.BC.BloaterLH)

eacf(diff.BC.BloaterLH,ar.max=2,ma.max=2)


res = armasubsets(y=diff.BC.BloaterLH,nar=1,nma=1,y.name='test',ar.method='ols')
plot(res)


log.eggs = log(eggs)
plot(eggs,type='o',ylab='Log-transformed eggs')

diff.log.eggs = diff(log(eggs))
plot(diff.log.eggs,type='o',ylab='The first difference of log-transformed eggs')

acf(diff.log.eggs)
pacf(diff.log.eggs)



qqnorm(eggs)
qqline(eggs, col = 2)
shapiro.test(eggs)

#nonstationarity
BloaterLH$eggs
acf(as.vector(log(eggs)),xaxp=c(0,24,12), main="Sample ACF for the BloaterLH.")

acf(diff(as.vector(log(eggs))),xaxp=c(0,24,12), main="Sample ACF for the first difference of BloaterLH Eggs.")

pacf(diff(as.vector(log(eggs))),xaxp=c(0,24,12), main="Sample PACF for the first difference of BloaterLH Eggs.")

#eacf(diff(as.vector(log(eggs))))
# Sample EACF of the BloaterLH Eggs.
eacf?
?eacf
eacf(diff.BC.BloaterLH,ar.max=2,ma.max=2)

library(lmtest)
res = armasubsets(y=diff.BC.BloaterLH,nar=1,nma=1,y.name='test',ar.method='ols')
plot(res)



#The set of possible models is {ARIMA(0,2,1),ARIMA(0,2,2),ARIMA(1,2,1),ARIMA(1,2,2),ARIMA(2,2,2)}
library(leaps)

#ARIMA (0,1,1)
model_011_css = arima(diff.BC.BloaterLH,order=c(0,1,1),method='CSS')
coeftest(model_011_css)

model_011_ml = arima(diff.BC.BloaterLH,order=c(0,1,1),method='ML')
coeftest(model_011_ml)

# ARIMA(0,2,1)
model_021_css = arima(diff.BC.BloaterLH,order=c(0,2,1),method='CSS')
coeftest(model_021_css)

model_021_ml = arima(diff.BC.BloaterLH,order=c(0,2,1),method='ML')
coeftest(model_021_ml)



# But when AR(2) is in the model, MA(1) is still significant. 
# So I'll increase the order of MA to 2 and see if it is significant.


#use aic bic to select best model

# Simply put your AIC or BICs into an object
# i.e. scores.aic <- AIC(model1, model2, model3, model4)

# Then set the score to the type of criterion
# i.e. score = "aic"

# sort.score(scores.aic, score = "aic")

# Output should reveal criterion scores in descending order

residual.analysis <- function(model, std = TRUE){
  library(TSA)
  library(FitAR)
  if (std == TRUE){
    res.model = rstandard(model)
  }else{
    res.model = residuals(model)
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = length(model$residuals)-1 , StartLag = k + 1, k = 0, SquaredQ = FALSE)
}
par(mfrow=c(1,1))
par(mar = rep(2, 4))
residual.analysis(model = model_011_ml)

residual.analysis(model = model_021_ml)



sort.score <- function(x, score = c("bic", "aic")){
  
  if (score == "aic"){
    x[with(x, order(AIC)),]
    
  } else if (score == "bic") {
    
    x[with(x, order(BIC)),]
    
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
    
  }
  
}

sc.AIC = AIC(model_011_ml,model_021_ml)
sc.BIC = BIC(model_011_ml,model_021_ml) 




sort.score(sc.AIC, score = "aic")
sort.score(sc.BIC, score = "bic")

eggsarima <- arima(eggs, order=c(0,1,1))
eggsarima

eggforecasts <- forecast.Arima(eggsarima,h=5)
eggforecasts


fit <- arima(log(eggs), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 5))
pred <- predict(fit, n.ahead = 10*5)
ts.plot(eggs,2.718^pred$pred, log = "y", lty = c(1,3))



myts <- ts(eggs, start=c(2009, 1), end=c(2014, 12), frequency=12) 


# fit an ARIMA model of order P, D, Q
fit <- arima(eggs, order=c(0, 1, 1)
             
             # predictive accuracy
             library(forecast)
             accuracy(fit)
             
             # predict next 5 observations
             library(forecast)
             forecast(fit, 5)
             plot(forecast(fit, 5))
 