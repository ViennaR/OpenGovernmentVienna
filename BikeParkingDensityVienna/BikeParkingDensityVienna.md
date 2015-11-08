Another addition to the  [OpenGovernmentVienna](https://github.com/ViennaR/OpenGovernmentVienna) repo by Christian Brandstaetter showing the density of bike parking lots in Vienna.

The script executes the following steps:

1. Download Vienna map including district boundaries and bike parking lots from http://data.wien.gv.at.
2. Plot two Vienna maps showing bike parking lots and density per district.


```r
library(rgdal) 
library(rgeos) 
library(XML)
library(RCurl)
library(ggplot2) 
library(plotrix) 
library(maptools)

#library(ggmap)
#require(XML) 
#require(RCurl) 
#require(RColorBrewer) 

download.vienna.bydistrict <- function(tablename, skip.row = 3) {
	baseurl <- "https://www.wien.gv.at/statistik"
	popurl <- sprintf("%s/%s.html", baseurl, tablename)

	poptable <- readHTMLTable(getURL(popurl))[[1]]
	poptable <- poptable[-c(1:skip.row), ]
	poptable <- poptable[, -1]
	row.names(poptable) <- NULL
	poptable <- sapply(poptable, function(x) gsub(".", "", x, fixed = TRUE))
	poptable <- gsub(",", ".", poptable, fixed = TRUE)
	poptable <- matrix(as.numeric(poptable), nrow = nrow(poptable))
	poptable
}

download.vienna.shape <- function(shapename, outdir = "data") {
	baseurl <- "http://data.wien.gv.at/daten/geo?service=WFS&request=GetFeature&version=1.1.0&typeName=ogdwien:"
	urlparam <- "&srsName=EPSG:4326&outputFormat=shape-zip"	
	url <- sprintf("%s%s%s", baseurl, shapename, urlparam)
	
	dir.create(outdir, showWarnings = FALSE)
	destfile = file.path(outdir, sprintf("%s.zip", shapename))
	download.file(url, destfile = destfile)
	unzip(destfile, exdir = file.path(outdir, shapename))
	file.remove(destfile)
}
```


```r
# Retrieve Data
## Read District Boundaries
download.vienna.shape("BEZIRKSGRENZEOGD")
```

```
## [1] TRUE
```

```r
wmap <- readOGR("data/BEZIRKSGRENZEOGD", layer="BEZIRKSGRENZEOGDPolygon") 
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "data/BEZIRKSGRENZEOGD", layer: "BEZIRKSGRENZEOGDPolygon"
## with 23 features
## It has 15 fields
```

```r
## Read Streets
###download.vienna.shape("STRASSENKNOTENOGD")
###smap <- readOGR("data/STRASSENKNOTENOGD", layer="STRASSENKNOTENOGD", p4s = "+init=epsg:4326") 
###ERROR: Incompatible geometry: 4

## Read bike parking lots
download.vienna.shape("FAHRRADABSTELLANLAGEOGD")
```

```
## [1] TRUE
```

```r
bmap <- readOGR("data/FAHRRADABSTELLANLAGEOGD", layer="FAHRRADABSTELLANLAGEOGDPoint") 
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "data/FAHRRADABSTELLANLAGEOGD", layer: "FAHRRADABSTELLANLAGEOGDPoint"
## with 4061 features
## It has 5 fields
```

```r
## Download Population per district
distpop <- download.vienna.bydistrict("bevoelkerung/tabellen/bevoelkerung-alter-geschl-bez")
distpopsum <- rowSums(as.matrix(distpop))

## Download Size of Each district
distsize <- download.vienna.bydistrict("lebensraum/tabellen/nutzungsklassen-bez", skip.row = 2)
distsizekm2 <- distsize[, 1] / 100
```


```r
# Calculate stuff

## Number of bike parking lots per district
bikelotsperdistrict <- table(bmap$BEZIRK)
lotdensity <- bikelotsperdistrict / distsizekm2

## Normalization of Colour Scaling
normdichte <- round(lotdensity/max(lotdensity)*50,digits=0) 
colfunc <- colorRampPalette(c("lightblue", "darkgreen")) 
colfunc100 <- colfunc(100) 
bezirksfarben <- colfunc100[normdichte]

# Transformationen  111.1 
# km in Grad (einfache Umrechnung) 
hundm <- (1/111.1)/10 
# 100 m für Radius der Punkte  
centroids <- gCentroid(wmap, byid=TRUE) # Mittelpunkt/Bezirk
```


# Plots  

## Plot 1 - Vienna Map with Bike Parking Lots (Discrete Density)


```r
layout(1:2, heights=c(5,1)) 
par(mar=c(0.5,0.5,1,0.5), oma=rep(3, 4), las=1) 
plot(wmap, main="Bike Parking Lots in Vienna (100 m Umkreis)", col=bezirksfarben[wmap$BEZNR]) 

###plot(smap, add=TRUE) -> doesn't work 
# add bike parking lots
plot(bmap, add = TRUE, col = "yellow") 
text(as.character(wmap$BEZ_RZ), x = centroids@coords[,1], y = centroids@coords[,2], col="orangered",cex=0.8,font=2)

# Legende  par(mar=c(1,0.5,3,0.5)) 
colseq <- seq(range(lotdensity)[1],range(lotdensity)[2],20) 
image(x=colseq,y=1,z=matrix(seq_along(colseq)), col=colfunc(10), main=expression(paste("Radparkplätzedichte (bezirksweise) km"^-2)),axes=F) 
axis(1)  
```

![plot of chunk bikedensity-plot1](figure/bikedensity-plot1-1.svg) 

# Plot 2 - Vienna Map with Bike Parking Lots (Continous Density)  


```r
#  Transformation of bike parking lots shape to data.frame for ggplot2
RK <- data.frame(coordinates(bmap)) 
colnames(RK) <- c("long","lat") 
# Straßenfile  
#ws2 <- fortify(smap,region="OBJECTID")  
# Kopieren des Wien-shapefiles  
wmap2 <- wmap 
# Zuweisen einer ID  
wmap2@data$id <- rownames(wmap2@data) 
# Umwandeln in dataframe  
test1 <- fortify(wmap2, region="id")   

ggplot(data=test1) + aes(x=long,y=lat) +  
	geom_polygon(aes(group=group),col="black",fill=NA) + 
	geom_point(data=RK,aes(x=long,y=lat)) + 
	#geom_line(data=ws2,aes(group=group)) + 
	xlab("longitute")+ ylab("latitude") + 
	stat_density2d(data=RK,aes(fill = ..level..),size=1,bins=200,alpha=0.1, geom="polygon",n=100) +  
	ggtitle("Bike Parking Lots in Vienna 2015") + 
	scale_fill_continuous(name = "Kernel Density Estimation") + 
	theme_bw() 
```

![plot of chunk bikedensity-plot2](figure/bikedensity-plot2-1.svg) 

Authors: Christian Brandstaetter with minor modifications by Mario Annau
