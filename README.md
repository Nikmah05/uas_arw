# uas_arw
car_amount = c(566, 538, 598, 635, 490, 515, 406, 502, 589, 512, 653, 519,
               523, 560, 626, 580, 560, 535, 521, 547, 466, 550, 452, 400,
               533, 504, 581, 604, 648, 573, 582, 556, 506, 413, 459, 548,
               598, 422, 535)
car_amount

car = read.csv("C:/Users/lenovo/Downloads/Report.csv",sep=",",header=TRUE)
car

library(zoo)

# Define the start and end dates
start_date <- as.Date("2024-04-25")
end_date <- as.Date("2024-06-02")

# Create a sequence of dates from start to end date
date_seq <- seq.Date(from = start_date, to = end_date, by = "day")
date_seq
# Create the time series with zoo
car_ts <- zoo(car_amount, order.by = date_seq)
car_ts

# Plot time series
plot(car_ts,type="o",pch=16,cex=0.5,xlab='Periode(Harian)',ylab='Jumlah Mobil',main='Report CCTV')

#cek kestasioneran data
library(tseries)
adf.test(car_ts)

#Plot ACF dan PACF
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(car_ts,lag.max=25,type="correlation",main="ACF for the Car Amount")

acf(car_ts,lag.max=25,type="partial",main="PACF for the Car Amount")

#Plot setelah diferensiasi
diff_car = diff(car_ts)
diff_car
plot(diff(car_ts))

#Plot ACF dan PACF setelah diferensiasi
acf(diff_car,lag.max=25,type="correlation",main="ACF for the Number")
acf(diff_car,lag.max=25,type="partial",main="PACF for the Number")

#cek kestasioneran data
adf.test(diff_car)

model_ARIMA=auto.arima(car[,2])
model_ARIMA

# Fit the AR(1) model
fit1= arima(car_ts, order = c(1, 0, 0))
fit1

#Uji signifikasi
library(lmtest)
coeftest(fit1)

#Residual
res.car.ar1<-as.vector(residuals(fit1))
res.car.ar1

#ACF and PACF of the Residuals
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(res.car.ar1,lag.max=25,type="correlation",main="ACF of the Residuals of AR(1) Model")
acf(res.car.ar1, lag.max=25,type="partial",main="PACF of the Residuals of AR(1) Model")

# Fit the MA(1) model
fit2 = arima(car_ts, order = c(0, 0, 1))
fit2

#Uji signifikasi
library(lmtest)
coeftest(fit2)

#Residual
res.car.ar2<-as.vector(residuals(fit2))
res.car.ar2

#Pengujian residual pada model optimal apakah sudah white noise?
Box.test(fit2$residuals, type = "Ljung")

# Fit the ARMA(1,1) model
fit3 = arima(car_ts, order = c(1, 0, 1))
fit3

#Residual
res.car.ar3<-as.vector(residuals(fit3))
res.car.ar3

#Pengujian residual pada model optimal apakah sudah white noise?
Box.test(res.car.ar3, type = "Ljung")

#Diagnostik Model
aic.model<-data.frame(Nama=c("m1","m2","m3"), Model=c("ARIMA(1,0,0)","ARIMA(0,0,1)","ARIMA(1,0,1)"), AIC = c(fit1$aic, fit2$aic, fit3$aic))
aic.model

#to obtain the fitted values we use the function fitted() from the forecast package
library(forecast)
# Mengambil nilai fitted dari model optimal
fitted_values <- fitted(fit1)
fitted_values

# Membuat prediksi untuk 12 hari ke depan
forecast_values <- forecast(fit1, h = 12)
forecast_values

# Plotting the fitted values
plot(car_ts, type = "o", pch = 16, cex = 0.5, xlab = 'Periode (Harian)', ylab = 'Jumlah Mobil', main = 'Jumlah Mobil Harian dengan Model ARIMA Optimal')
lines(date_seq,fitted_values, col = "red", lwd = 2)
legend("topright", legend = c("Data Aktual", "Nilai Fit"), col = c("black", "red"), lty = 1, lwd = 2, pch = c(16, NA))

# Plotting the forecast values
plot(forecast_values, main = "Forecast Jumlah Mobil Harian", xlab = "Tanggal", ylab = "Jumlah Mobil")
lines(forecast_values$mean, col = "blue", lwd = 2)
forecast_values
