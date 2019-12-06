plot(spSample)
plot(income.pm25)

#Mean, standard deviation, median pm.25 measurements
mean.pm2.5 <- mean(spSample$PM25AGG)
sd.pm2.5 <- sd(spSample$PM25AGG, na.rm = T)
median.pm2.5 <- median(spSample$PM25AGG, na.rm = T)

#Mean, standard deviation, median, income
mean.income <- mean(spSample$Income)
sd.inc <- sd(spSample$Income, na.rm = T)
median.inc <- median(spSample$Income, na.rm = T)

#histogram PM 2.5
PM2.5.Exposure <- (spSample$PM25AGG)
hist_pm.25 <- hist(PM2.5.Exposure, breaks=30, main = "Histogram of PM 2.5 concentration")
plot(hist_pm.25)

#Skewness of PM 2.5 measurements
pm2.5_skew <- skewness(PPM)

#histogram of Income
Earnings <- (spSample$Income)
hist_inc <- hist(Earnings, breaks=30, main= "Histogram of income levels in Greater Vancouver")

#creates a list of neighbors for every census tract
inctract.nb <- poly2nb(income.tracts)

#used to import weights to  
inctract.net <- nb2lines(inctract.nb,coords=coordinates(income.tracts))

#used to directly assign weights
inctract.lw <- nb2listw(inctract.nb, zero.policy = TRUE, style = "W")
print.listw(inctract.lw, zero.policy = TRUE)

neighbornet_map <-tm_shape(income.tracts) +
                    tm_borders(col = "lightgrey") +
                    tm_shape(inctract.net) + 
                    tm_lines(col="red") 


                    
