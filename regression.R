######Linear Regression##########

# pm.income.poly <- na.omit(pm.income.poly)

#Plot income and PM2.5 from the pm.income.poly dataset you created
plot(pm.income.poly$Income~pm.income.poly$PM25AGG)
#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
# pm.income.poly.0_removed <-  pm.income.poly[pm.income.poly$PM25AGG!= 0,]
#Now plot the data again
#plot(pm.income.poly.0_removed$PM25AGG~pm.income.poly.0_removed$Income)

?lm
#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(pm.income.poly$Income~pm.income.poly$PM25AGG)
#Add the regression model to the plot you created
abline(lm.model)
#Get the summary of the results
summary(lm.model)
plot(lm.model)

#You want to determine if the model residuals are spatially clustered. 
?residuals.lm
#First obtain the residuals from the model
model.resids <- as.data.frame(residuals.lm(lm.model))
#Then add the residuals to your spatialpolygon dataframe
pm.income.poly$resids <- unlist(model.resids)
#Observe the result to make sure it looks correct
head(pm.income.poly)

View(pm.income.poly@data)

residual_map <- tm_shape(pm.income.poly) + tm_legend(legend.outside=TRUE)+
  tm_polygons(col = "resids",
               title = "Residuals",
               palette = "Reds",
               n = 6)

 
residual_map




#trying to run GMI

pm.income.poly.nb <- poly2nb(pm.income.poly)

pm.income.poly.lw <- nb2listw(pm.income.poly.nb, zero.policy = T)

residuals.mi <- moran.test(pm.income.poly$resids, pm.income.poly.lw, zero.policy = T, na.action = na.omit) 
?moran.test
residuals.mi

moran.range_pm.25 <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(pm.income.poly.lw)

re_mI <- residuals.mi$estimate[[1]]
re_eI <- residuals.mi$estimate[[2]]
re_var <- residuals.mi$estimate[[3]]

z_GMI_res <- (re_mI - re_eI) / re_var^0.5
z_GMI_res

lisa.residuals <- localmoran(pm.income.poly$resids, pm.income.poly.lw, na.action = na.omit, zero.policy = T)

pm.income.poly$Ii <- lisa.residuals[,1]
pm.income.poly$E.Ii<- lisa.residuals[,2]
pm.income.poly$Var.Ii<- lisa.residuals[,3]
pm.income.poly$Z.Ii<- lisa.residuals[,4]
pm.income.poly$P<- lisa.residuals[,5]
########################

lmi_residuals <- tm_shape(pm.income.poly) + tm_legend(legend.outside=TRUE)+
  tm_polygons(col = "Ii", 
              title = "Local Moran's I: Regression
Residuals", 
              palette = "Reds", n = 6) 

lmi_residuals

moran.plot(pm.income.poly$residuals, pm.income.poly.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="placeholder", 
           ylab="placeholder", quiet=NULL)

# using tmapper to map residuals 

                  


?tm_polygons



