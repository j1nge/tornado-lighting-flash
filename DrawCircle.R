#### DRAW CIRCLE OF A RADIUS R ON A LAT-LON GRAPH
#### DX^2 + DY^2 = R^2.   
#### POLAR COORDINATES: 
####    R*COS(THETA) = DX
####    R*SIN(THETA) = DY
#### RELATION TO LAT/LON (WHICH HAVE DIFFERENT SCALES).  
####    DX = AE * COS(LAT.CENTER) * DLON
####    DY = AE * DLAT
#### NOTE: DLON AND DLAT MUST BE IN RADIANS (NOT DEGREES)
#### ELIMINATING DX AND DY FROM THE ABOVE EQUATIONS
####    DLON = R * COS(THETA) / (AE * COS(LAT.CENTER))
####    DLAT = R * SIN(THETA) /  AE
####    

lplotfile       = TRUE			### indicator to produce pdf output

##################################################
### DEFINE CIRCLE
##################################################
radius.to.draw  =  600			### in kilometers
radius.of.earth = 6378			### in kilometers (denoted 'ae' in above equations)
lon.center      =  100 * pi/180	### Oaklahoma is around 100W; convert to radians
lat.center      =   35 * pi/180 ### Oaklahoma is around  35N; convert to radians

ntheta          = 100
theta           = seq(from=0,to=2*pi,length.out=100)

dlon            = radius.to.draw * cos(theta) / (radius.of.earth * cos(lat.center))
dlat            = radius.to.draw * sin(theta) /  radius.of.earth

lon.to.plot     = (lon.center + dlon) * 180/pi	### longitude to plot; convert to degrees
lat.to.plot     = (lat.center + dlat) * 180/pi  ### latitude to plot; convert to degrees

##################################################
### MAKE PLOT
##################################################
lon.range = c(70,120)
lat.range = c(20,50)
fout      = './figures/DrawCircle.pdf'
if (lplotfile) pdf(fout,width=8,height=6)
par(mfcol=c(1,1),mar=c(5,5,4,3))
par(cex.lab=1.3,cex.axis=1.3,cex.main=1.3)
plot(1,1,type='n',xlim=lon.range,ylim=lat.range,xlab='longitude',ylab='latitude')
lines(lon.to.plot,lat.to.plot,col='red',lwd=2)
points(lon.center* 180/pi,lat.center* 180/pi,pch=19,col='red')
ftitle.top = paste('circle of radius ',radius.to.draw,'km',sep='')
ftitle.bot = paste('center: ',lon.center*180/pi,'W and ',lat.center*180/pi,'N',sep='')
title(main=ftitle.top,line=2.0)
title(main=ftitle.bot,line=0.5)
if (lplotfile) dev.off()




