#creates a list of neighbors for every census tract
inctract.nb <- poly2nb(income.tracts)

#used to import weights to  
inctract.net <- nb2lines(inctract.nb,coords=coordinates(income.tracts))

#used to directly assign weights
inctract.lw <- nb2listw(inctract.nb, zero.policy = TRUE, style = "W")
print.listw(inctract.lw, zero.policy = TRUE)


inctract.nb <- poly2nb(income.tracts)

inctract.net <- nb2lines(inctract.nb,coords=coordinates(income.tracts))


inctract.lw <- nb2listw(inctract.nb, zero.policy = TRUE, style = "W")
print.listw(inctract.lw, zero.policy = TRUE)


#Global Moran's I: Determining whether income has positive SAC (or segregated) or negative SAC (integrated)
# Produces a global Moran's I number for a variable, an expected moran's I, and calculates the variance of the dataset.
income_mi <- moran.test(income.tracts$Income, inctract.lw, zero.policy = TRUE)
income_mi


moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(inctract.lw)


inc_mI <- income_mi$estimate[[1]]
inc_eI <- income_mi$estimate[[2]]
inc_var <- income_mi$estimate[[3]]

#Generates a Z score for the Moran's I number of the dataset. Can be used to decide if Spatial 
#Autocorrelation is random, positive, or negative over the entire dataset.
z_GMI_inc <- (inc_mI - inc_eI) / inc_var^0.5
z_GMI_inc

#Local Moran's I
lisa.test <- localmoran(income.tracts$Income, inctract.lw)

income.tracts$Ii <- lisa.test[,1]
income.tracts$E.Ii<- lisa.test[,2]
income.tracts$Var.Ii<- lisa.test[,3]
income.tracts$Z.Ii<- lisa.test[,4]
income.tracts$P<- lisa.test[,5]
########################

map_LISA_income <- tm_shape(income.tracts) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's I: Income", 
              style = "fisher", 
              palette = "viridis", n = 6) 
  tm_legend(legend.outside=TRUE)
map_LISA_income

moran.plot(income.tracts$Income, inctract.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="income at location i", 
           ylab="income of neighborhood of location i", quiet=NULL)
