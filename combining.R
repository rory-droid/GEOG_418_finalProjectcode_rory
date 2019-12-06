#These steps will help you combine the outputs from your spatial interpolation with your income data.
install.packages("spatialEco")
library("spatialEco")
#If you have too many cells, you can reduce the number by aggregating values
step.1 <- aggregate(r_idw, fact=1, fun=mean, na.action = na.omit)
plot(step.1)



#Convert the raster dataset to points
step.2 <-  rasterToPoints(r_idw, fun=NULL, spatial=FALSE, crs=crs(pm25.spatial), na.action=na.omit)
step.2 <- as.data.frame(step.2) #convert the point dataset to a spatial dataframe
Coords <- step.2[,c("x", "y")]  #assign coordinates to a new object
crs <- crs(census.tracts) #utilize an existing projection
step.3 <- SpatialPointsDataFrame(coords = Coords, data = step.2, proj4string = crs) #create a spatial points dataframe
step.4 <- aggregate(x=step.3, by=income.tracts, FUN=mean, na.actions = na.omit) #aggregate points into census tracts
step.5 <- raster::intersect(step.4, income.tracts)  #get the intersection of step.4 with the income.tracts dataset (this will take a while) 



pm.income.poly <- sp.na.omit(step.5)
pm.income.poly$PM25AGG <- pm.income.poly$var1.pred
View(pm.income.poly@data)

#You are now ready to perform a regression
