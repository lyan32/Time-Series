#reads csv file and calls it D
D=read.csv("data1.csv")

# changes dataframe to time series with start year of 1927 and end year 2016
D=ts(as.vector(D), start = 1927, end =2016) # changes datafram to time series
#plots time series of year against ozone 
plot(D, xlab = 'year', ylab = 'ozone', type='o', main = "Time Series Plot of ozone vs year")

#plots scatter plot

plot(D,type = 'p',xlab='year', ylab='ozone',
     main='Scatterplot of ozone vs year')
points(y=D,x=time(D),pch=as.vector(month(D)))

# plots time series model 
plot(D, xlab = 'year', ylab = 'ozone', type='o', main = "Time Series Plot of ozone vs year")
model = lm(D~time(D)) # label the linear trend model as model1
summary(model) # summarises output from lm function

plot(D,type='o',ylab='year', main =" linear model")
abline(model) # finds line of best fit

#histogram
hist((D),xlab= 'ozone')

#qq plot
qqnorm(D)
shapiro.test(D) #performs shapiro-wilk test

# draws acf plot
acf(D)



har.=harmonic(D,0.25) # calculate cos(2*pi*t) and sin(2*pi*t)
model2=lm(D~har.) #names model
summary(model2) #summarises information
plot(model2) # plots model2

# plots cosine trend 
plot(ts(fitted(model2),freq=12,start=c(1927,0.25)),ylab='y',
     type='l',
     ylim=range(c(fitted(model2),D)),main="Fitted model to average yearly ozone") # ylim ensures that the y axis range fits the raw data and the fitted values
points(D)
