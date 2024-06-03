rm(list=ls())

lplotfile  = TRUE

dir.data   = '/Users/delsole/Data/Lightning/OR_GLM-L2-LCFA_G17_s20191201354000_e20191201354200_c20191201354229/'
dir.Rlib   = '/Users/delsole/R/delsole_tools/'

source(paste(dir.Rlib,'pdf.eps.R',sep=''))
library(ncdf4)
library(chron)

### GLM LIGHTNING DATA
### https://www.goes-r.gov/resources/docs.html
### https://storage.googleapis.com/gcs-public-datasets/GOES-R%20beginners%20guide%20-%20EN.pdf
### https://console.cloud.google.com/marketplace/details/noaa-public/goes-16
### https://www.goes-r.gov/spacesegment/glm.html
### https://www.meted.ucar.edu/goes_r/glm
# https://www.star.nesdis.noaa.gov/GOES/documents/GLM_Quick_Guides_May_2019.pdf
# https://www.star.nesdis.noaa.gov/GOES/meso.php?sat=G18&lat=37N&lon=102W
# https://www.star.nesdis.noaa.gov/goesr/documents/ATBDs/Baseline/ATBD_GOES-R_GLM_v3.0_Jul2012.pdf
# https://www.goes-r.gov/products/baseline-lightning-detection.html
# https://www.goes-r.gov/downloads/resources/documents/Beginners_Guide_to_GOES-R_Series_Data.pdf
# https://www.goes-r.gov/education/docs/fs_aerosols.pdf
# https://www.goes-r.gov/resources/docs.html
# https://unidata.github.io/python-gallery/examples/mapping_GOES16_TrueColor.html#sphx-glr-download-examples-mapping-goes16-truecolor-py
# Geostationary Lightning Mapper python
# Kathy Pegion: help with GLM?  

### PLOTTING USMAPS
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
# vignette('ggplot2-specs')
library(sp)
library(ggplot2)
library(usmap)

####################################################
######### SPECIFY TORNADO
####################################################
tornado.year = '2019'
tornado.mon  = '04'
tornado.day  = '30'
tornado.hour = '13'
tornado.min  = '44'
tornado.sec  = '00'

tornado.time  = paste(tornado.hour,tornado.min,tornado.sec,sep=':')
tornado.day   = paste(tornado.year,tornado.mon,tornado.day,sep='-') 
tornado.name  = paste(tornado.year,tornado.mon,tornado.day,tornado.hour,tornado.min,tornado.sec,sep='')
tornado.chron = chron(dates=tornado.day,times=tornado.time,format=c('y-m-d','h:m:s')) 

####################################################
######### EXTRACT TORNADO LOCATION	
## SPREADSHEET FROM https://www.spc.noaa.gov/wcm/#jmc
####################################################
tornado.file  = '/Users/delsole/2023/Eric Jing/Data/1950-2022_all_tornadoes.csv'
tornado.table = read.csv(file=tornado.file,header=TRUE)
npic          = which(as.numeric(tornado.year) == tornado.table[,'yr'] & 
                      as.numeric(tornado.mon)  == tornado.table[,'mo'] &
                                 tornado.time  == tornado.table[,'time'] )
if (length(npic) != 1) stop('trouble finding tornado')
tornado.slat  = tornado.table[npic,'slat']
tornado.slon  = tornado.table[npic,'slon']
tornado.elat  = tornado.table[npic,'elat']
tornado.elon  = tornado.table[npic,'elon']

latlon.tornado.frame = data.frame(lon=c(tornado.slon,tornado.elon),lat=c(tornado.slat,tornado.elat))

####################################################
######### READ GLM FILE NAMES (date is not checked for synchronicity)
####################################################
glm.names = list.files(path=dir.data)
nfiles     = length(glm.names)
if (!lplotfile) nfiles = 1

####################################################
######### CHECK GLM FILES ARE SEQUENTIAL (crude check, not perfect)
####################################################
glm.init = as.numeric(substr(glm.names,28,28+6)) ## extract month/day/sec from file name
diff.init = diff(glm.init)
if (any(!(diff.init == 600 | diff.init == 200))) stop('time steps are not consecutive')

####################################################
######### EXTRACT GLM TIME OF DAY
####################################################
glm.year = substr(glm.names,21,24)
glm.doy  = substr(glm.names,25,27)
glm.hour = substr(glm.names,28,29)
glm.min  = substr(glm.names,30,31)
glm.sec  = substr(glm.names,32,33)

date.jan1 = as.Date(paste(glm.year,1,1,sep='-'),'%Y-%m-%d')
glm.yday  = date.jan1 + as.numeric(glm.doy) - 1

glm.time = paste(glm.hour,glm.min,glm.sec,sep=':')
glm.chron = chron(dates=as.character(glm.yday),times=glm.time,format=c('y-m-d','h:m:s'))

####################################################
######### COMPUTE DIFFERENCE IN TIMES
####################################################
delta.time = round(as.numeric(difftime(glm.chron,tornado.chron,units='secs')))

####################################################
######### READ DATA AND GENERATE PLOT
####################################################
fout = paste('./figures/t',tornado.name,'.pdf',sep='')
if (lplotfile) pdf(file=fout,width=8.5,height=7)
for (nf in 1:nfiles) {
	fname      = paste(dir.data,glm.names[nf],sep='')
	ncin       = nc_open(fname)
	flash_lat  = ncvar_get(ncin,'flash_lat')
	flash_lon  = ncvar_get(ncin,'flash_lon')
	
	### remove flashes outside of CONUS
	loutside  = flash_lat <= 25 | flash_lat >= 50 | flash_lon <= -125 | flash_lon >= -70
	flash_lat = flash_lat[!loutside]
	flash_lon = flash_lon[!loutside]
	
	latlon.frame   = data.frame(lon=flash_lon,lat=flash_lat)
	latlon.trans   = usmap_transform(latlon.frame)
	latlon.tornado = usmap_transform(latlon.tornado.frame)
	
	
	ftitle.top = paste('Lightning from GLM',glm.yday[nf],glm.time[nf])
	ftitle.bot = paste(delta.time[nf],'seconds before tornado')
	p = plot_usmap(regions='states') + 
	    geom_point(data=latlon.tornado,aes(x = x, y = y, size = 0.9),color = "blue", alpha = 1) +
	    geom_point(data=latlon.trans  ,aes(x = x, y = y, size = 1),color = "red" , alpha = 0.25) +
	    labs(title=ftitle.top,subtitle=ftitle.bot) +
	    theme(plot.title = element_text(size=20),plot.subtitle=element_text(size=15))
	print(p)
	nc_close(ncin)

}

if (lplotfile) dev.off()

