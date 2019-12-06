P.idw <- gstat::idw(PM25AGG ~ 1, spSample, newdata=grd, idp=5.2)
r_idw       <- raster(P.idw)
# r_idw_mask     <- mask(r, income.tracts)

IDW_sfc <- tm_shape(r_idw) + 
  tm_raster(n=10,palette = "-RdBu",
            title="Predicted PM 2.5 in ppm") + 
  tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
IDW_sfc

# Leave one out Validation
IDW.out <- vector(length = length(spSample))
for (i in 1:length(spSample)) {
  IDW.out[i] <- gstat::idw(PM25AGG ~ 1, spSample[-i,], spSample[i,], idp=5.2)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ spSample$PM25AGG, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ spSample$PM25AGG), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
rmse <- sqrt( sum((IDW.out - spSample$PM25AGG)^2) / length(spSample$PM25AGG))
rmse


# Implementation of a jackknife technique to estimate a confidence interval at each unsampled point.

# Create the interpolated surface
img <- gstat::idw(PM25AGG ~ 1, spSample, newdata=grd, idp=5.2)
n   <- length(spSample)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(PM25AGG ~ 1, spSample[-i,], newdata=grd, idp=5.2)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 


r.confidence_IDW <- raster(img.sig, layer="v")
#r.CI.idw.mask <- mask(r, income.tracts)

# Plot the map
tm_shape(r.confidence_IDW) + tm_raster(n=7,title="95% confidence interval \n(in ppm)") +
  tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)