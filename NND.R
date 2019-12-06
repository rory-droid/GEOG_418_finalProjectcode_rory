  



spSample$x <- coordinates(spSample)[,1]
spSample$y <- coordinates(spSample)[,2]



#check for and remove duplicated points
#check for duplicated points
#finds zero distance among points
zd <- zerodist(spSample)

zd
#remove duplicates
spSample.re <- remove.duplicates(spSample)

#create an "extent" object which can be used to create the observation window for spatstat
spSample.ext <- as.matrix(extent(spSample.re)) 

#observation window
window <- spatstat::as.owin(list(xrange = spSample.ext[1,], yrange = spSample.ext[2,]))

#create ppp oject from spatstat
spSample.ppp <- ppp(x = spSample$x, y = spSample$y, window = window)

  #####
  ##Nearest Neighbour Distance
  ###NEAREST NEIGHBOUR
  nearestNeighbour <- nndist(spSample.ppp)
  
  ##Convert the nearestNeighbor object into a dataframe.
  nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
  ##Change the column name to "Distance"
  colnames(nearestNeighbour) = "Distance"
  
  
  
  ##Calculate the nearest neighbor statistic to test for a random spatial distribution.
  #mean nearest neighbour
  nnd = sum(nearestNeighbour$Distance)/250
    
  library("rgeos")  

    income.tracts <- spTransform(income.tracts, CRS("+init=epsg:26911"))
  Total_area <- gArea(income.tracts)
    pointDensity <- 250/Total_area
    
    ?gArea
    
    r.nnd = 1/(2*pointDensity^0.5)
    
    d.nnd = 1.07453/pointDensity^0.5
    
    R = nnd/r.nnd
    
    SE.NND <- 0.26136/(250*pointDensity)^0.5
    
    z = (nnd-r.nnd)/SE.NND
  