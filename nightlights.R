#TODO:
#+ gzip all outputrasters and extract/delete tifs as required 
#+ delete the 2nd tif in the tiles (avg_rad_...).
#+ keep tiles gzipped until required. extract/delete as needed
#+ modularize everything; processNtLts especially
#+ give functions better names more descriptive
#+ validation of inputs, error handling
#+ give temp files unique names to avoid problems in case of parallelization
#+ settings and default settings list/DF
#+ optimize download of tiles
#+ zone files functions
#+ logging
#+ debug mode
#+ verify treatment of ATA i.e. single adm level countries
#+ logic of getCtryPolyAdmLevelNames esp lvlEngName assignment needs scrutiny
#+OLS

#Notes: gdalwarp is not used for cropping because the crop_to_cutline option causes a shift in the cell locations which then affects the stats extracted. A gdal crop to extent would be highly desirable though so seeking other gdal-based workarounds

if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')

pacman::p_load(readr, dplyr, lubridate, rgdal, raster, sp, rgeos, rworldmap, cleangeo, foreach, doParallel, compiler, gdalUtils, data.table, ff)

require(readr)
require(dplyr)
library(data.table)
require(ff)

require(lubridate)

require(rgdal)
require(gdalUtils)
require(raster)

require(sp)
require(rgeos)
require(rworldmap)
require(cleangeo)

library(foreach) #Enables for-each statements to be used for parallel processing
library(doParallel) #Allows for parallel processing using multiple cores

require(compiler)

enableJIT(0)

#rasterOptions(tmpdir = "/media/NewVolume/Downloads/RTemp/")

ntLtsIndexUrlViirs <- "https://www.ngdc.noaa.gov/eog/viirs/download_monthly.html"

#projection system to use
#can we use only one or does it depend on the shapefile loaded?
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

map <- rworldmap::getMap()
map <- clgeo_Clean(map)

#Set raster directory path
dirRasterOLS <- "./tiles"

#Set directory path
dirRasterVIIRS <- "./tiles"

dirRasterOutput <- "./outputrasters"

dirRasterWeb <- "./outputrasters_web"

dirZonals <- "./zonals"

dirPolygon <- "./polygons"

dirNlData <- "./data"

shpTopLyrName <- "adm0"

#cropMaskMethod" Method used to crop and mask tiles to country polygons. options: "gdal" or "rast" gdal is usually faster but requires gdal to be installed on the system
cropMaskMethod <- "gdal" 

extractMethod <- "gdal"

omitCountries <- "none"

#' Create mapping of VIIRS nightlight tiles
#'
#' Creates a data.frame mapping VIIRS nightlight tile names to their vertice coordinates. This is used to
#'     identify nightlight tiles as well as to build a spatial polygons dataframe used to plot the tiles. OLS
#'     only has one tile for the whole world and thus does not need mapping.
#'
#' @return A data.frame of names of tiles and lon-lat coordinate of top-left corner of each
#'
#' @examples
#' getNlTiles()
#'
#' @export
getNlTiles <- function()
{
  #6 nightlight tiles named by top-left geo coordinate numbered from left-right & top-bottom
  #creates columns as strings. createSpPolysDF converts relevant columns to numeric
  nlTiles <- as.data.frame(cbind(id=c(1,2,3,4,5,6), name=c("75N180W", "75N060W", "75N060E", "00N180W", "00N060W", "00N060E"), minx=c(-180, -60, 60, -180, -60, 60), maxx=c(-60, 60, 180, -60, 60, 180), miny=c(0, 0, 0, -75, -75, -75), maxy=c(75, 75, 75, 0, 0, 0)), stringsAsFactors=F)
  
  return (nlTiles)
}

#' Checks if the nlTiles data.frame variable exists
#'
#' Checks if the nlTiles data.frame variable exists in the environment and that it is not null or empty.
#'     This is used by other functions that depend on the nlTiles data.frame to check if it exists. If
#'     it doesn't they may create the data.frame.
#'
#' @return TRUE/FALSE
#'
#' @examples
#' if(!existsNlTiles())
#'   nlTiles <- getNlTiles()
#'
#' @export
existsNlTiles <- function()
{
  if (exists("nlTiles") && class(nlTiles) == "data.frame" && !is.null(nlTiles) && nrow(nlTiles) > 0)
    return (TRUE)
  else
    return (FALSE)
}

#' Checks if the variable \code{"tSpPolysDFs"} exists
#'
#' Checks if the \code{"tSpPolysDFs"} (tile Spatial Polygons DataFrame) variable exists in the environment 
#'     and that it is not null.
#'     This is used by other functions that depend on the \code{"tSpPolysDFs"} data.frame to check if it exists. 
#'     If it doesn't they can create the data.frame.
#'
#' @return TRUE/FALSE
#'
#' @examples
#' if(!existsTSpPolysDFs())
#'   tSpPolysDFs <- createNlTilesSpPolysDF()
#'
#' @export
existsTSpPolysDFs <- function()
{
  if (exists("tSpPolysDFs") && class(tSpPolysDFs) == "SpatialPolygonsDataFrame" && !is.null(SpatialPolygonsDataFrame))
    return(TRUE)
  else
    return(FALSE)
}

#' Creates a tile Spatial Polygons DataFrame from the \code{"nlTiles"} dataframe
#'
#' Creates a Spatial Polygons DataFrame from the \code{"nlTiles"} dataframe of VIIRS tiles
#'
#' @return TRUE/FALSE
#'
#' @examples
#'   tSpPolysDFs <- createNlTilesSpPolysDF()
#'
#' @export
createNlTilesSpPolysDF <- function()
{
  if (!existsNlTiles())
  {
    nlTiles <- getNlTiles()
  }
  
  #convert nlTiles min/max columns to numeric
  for (cIdx in grep("id|min|max", names(nlTiles))) nlTiles[,cIdx] <- as.numeric(as.character(nlTiles[, cIdx]))
  
  #create the empty obj to hold the data frame of tile PolygonsDataFrams
  tSpPolysDFs <- NULL
  
  #for each row in nlTiles
  for (i in 1:nrow(nlTiles))
  {
    #grab the row containing the tile
    t <- nlTiles[i,]
    
    #convert the tile x,y extents to a matrix
    #format is 2 cols x & y
    tMat <- as.matrix(cbind(rbind(t$minx, t$maxx, t$maxx, t$minx), rbind(t$maxy, t$maxy, t$miny, t$miny)))
    
    #create a Polygon object from the tile extents matrix
    tPoly <- list(Polygon(tMat))
  
    #create a Polygons object with a list of 1 polygon
    tPolys <- Polygons(srl = tPoly, ID = "nltiles")
    
    #create a SpatialPolygons object with a list of 1 list of Polygons
    tSpPolys <- SpatialPolygons(Srl = list(tPolys))
    
    #we assign the CRS at this point (note other objects cannot be assigned CRS)
    projection(tSpPolys) <- CRS(wgs84)
    
    #convert the SpatialPolygons object into a SpatialPolygonsDataFrame
    tSpPolysDF <- as(tSpPolys, "SpatialPolygonsDataFrame")
    
    #append the SPDF into a dataframe of SPDFs
    if (is.null(tSpPolysDFs))
      tSpPolysDFs <- tSpPolysDF
    else
      tSpPolysDFs <- rbind(tSpPolysDFs, tSpPolysDF)
  }
  return (tSpPolysDFs)
}

#' Plot a country polygon and the VIIRS tiles
#'
#' Plot a country polygon as defined in the \code{"rworldmap"} package along with the \code{"nlTiles"}
#'     for a visual inspection of the tiles required to process a country's nightlight data
#'
#' @param idx either the index of the country polygon in \code{"rworldmap::getMap()"} or the 3-letter 
#' ISO3 country code
#'
#' @return None
#'
#' @examples
#' plotCtryWithTiles("KEN")
#' 
#'     which is identical to
#' 
#' plotCtryWithTiles(85)
#'
#' @export
plotCtryWithTiles <- function(idx)
{
  #if the map variable does not exist
  if(!exists("map"))
  {
    #get the map from the rworldmap package
    map <- rworldmap::getMap()
    
    #some rworldmap polygons have problems. Rectify them to allow plotting without errors
    map <- clgeo_Clean(map)
  }

  #if the tiles spatial polygons dataframe does not exist create it
  if(!existsTSpPolysDFs())
    tSpPolysDFs <- createNlTilesSpPolysDF()
    
  #if idx is numeric we assume it is the index of the country polygon in the map
  if (is.numeric(idx))
  {
    #idx cannot be less than zero or greater than the number of polygons in the map
    if(idx < 0 || idx > length(map@polygons))
    {
      #invalid index
      return("Index out of range")
    }
  }
  else #try if it is a character ISO3 code
  {
    #valid index so get the corresponding ISO3 country code
    ctryISO3 <- rwmGetISO3(idx)
    
    #print(ctryISO3)

    #if ctryISO3 is empty then the country was not found
    if (is.na(ctryISO3) || ctryISO3 == "")
      return("Unknown country")
    
    #otherwise we have a valid country ISO3 code. get its index
    idx <- which(as.character(map@data$ISO3) == ctryISO3)
  }

  #At this point we have a valid index number
   
  #get the polygon that matches the index
  ctryPolys <- map@polygons[[idx]]
  
  #create a SpatialPolygons object with a list of 1 list of Polygons
  ctrySpPolys <- SpatialPolygons(Srl = list(ctryPolys))
  
  #set the coordinate reference system
  crs(ctrySpPolys) <- CRS(wgs84)
  
  #convert the spatial polygons to an SPsDF
  ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")
  
  #set the 4 margins to 2 inches from the border to avoid boundary errors
  par(mar=rep(2,4))
  
  #plot the tiles first
  plot(tSpPolysDFs)
  
  #plot the country on the same plot and fill with blue
  plot(ctrySpPolysDF, col="blue", add=TRUE)
  
  #ggplot(tSpPolysDFs, aes(x=long,y=lat))+geom_polygon(col="black", fill="white", alpha=0.5)#+geom_polygon(data=ctrySpPolysDF, alpha=0.5)
  #ggplot(ctrySpPolysDF, aes(x=long,y=lat, group=group))+geom_polygon(col="black", fill="white",alpha=0.5)
  
  #a <- spplot(tSpPolysDFs, main=map@polygons[[idx]]@ID)
  #b <- spplot(ctrySpPolysDF)
  
  #a+as.layer(b)
}

#' Create a mapping of all countries and the tiles they intersect
#'
#'This is simply another name for mapCtryPolyToTiles with ctryCodes="all"
#'
#' @param omitCountries A character vector or list of countries to leave out. Default is \code{"none"}
#'
#' @return None
#'
#' @examples
#' mapAllCtryPolyToTiles() no countries omitted
#' 
#' mapAllCtryPolyToTiles(omitCountries="none") no countries omitted
#' 
#' mapAllCtryPolyToTiles(omitCountries=c("error", "long")) will not omit countries that take long to process
#'
#' @export
mapAllCtryPolyToTiles <- function(omitCountries="none")
{
  mapCtryPolyToTiles(ctryCodes="all", omitCountries)
}

#' Create a mapping of all countries and the tiles they intersect
#'
#' Create a dataframe mapping each country in the rworldmap to the VIIRS tiles which they intersect with 
#'     and thus need to be retrieved to process their nightlight imagery. Since some functions use this 
#'     dataframe for long-term processing, omitCountries can eliminate countries that should be excluded
#'     from the list hence from processing. Countries can be added in the omitCountries function.
#'     Default is "none".
#'
#' @param ctryCodes A character vector or list of countries to map. Default is \code{"all"}
#' @param omitCountries A character vector or list of countries to leave out. Default is \code{"none"}
#'
#' @return ctryCodeTiles A data frame of countries and the tiles they intersect with as give by getNlTiles()
#'
#' @examples
#' mapCtryPolyToTiles() map all countries
#' 
#' mapCtryPolyToTiles(ctryCodes="all", omitCountries="none") map all countries, no countries omitted
#' 
#' mapCtryPolyToTiles(omitCountries=c("error", "missing")) will not omit countries that do not have polygons 
#' on GADM
#'
#' @export
mapCtryPolyToTiles <- function(ctryCodes="all", omitCountries="none")
{
  #if ctryCodes is "all" otherwise consider ctryCodes to be a list of countries
  if (length(ctryCodes) == 1 && tolower(ctryCodes) == "all")
  {
    #get list of all country codes
    ctryCodes <- getAllNlCtryCodes(omitCountries)
  }

  #if the rworldmap::getMap() hasn't been loaded, load it
  if (!exists("map"))
  {
    map <- rworldmap::getMap()
  
    map <- clgeo_Clean(sp = map)
  }
  
  #get the indices of the country polygons from the rworldmap
  ctryCodeIdx <- which(map@data$ISO3 %in% ctryCodes)
  
  ctryCodeTiles <- NULL
  
  #for each ctryCode index
  for (i in ctryCodeIdx)
  {
    #get the matching polygon
    ctryPolys <- map@polygons[[i]]
    
    #create a SpatialPolygons object with a list of 1 list of Polygons
    ctrySpPolys <- SpatialPolygons(Srl = list(ctryPolys))
    
    #set the CRS
    crs(ctrySpPolys) <- CRS(wgs84)
    
    #convert the SpatialPolygons to a SpatialPolygonsDataFrame
    ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")
    
    #find the tiles the SPDF intersects with and add to the list of tiles
    ctryCodeTiles <- rbind(ctryCodeTiles, list(tilesPolygonIntersect(ctrySpPolys)))
  }

  #combine the ctryCodes and intersecting tile columns into a dataframe
  ctryCodeTiles <- as.data.frame(cbind(code = as.character(ctryCodes), tiles = ctryCodeTiles))
  
  #name the columns
  names(ctryCodeTiles) <- c("code", "tiles")
  
  #convert the code column to character since it is picked as factor
  ctryCodeTiles$code <- as.character(ctryCodeTiles$code)

  #return the data frame  
  return(ctryCodeTiles)
}

#' Get a list of tiles that a country polygon intersects with
#'
#' Create a dataframe mapping each country in the rworldmap to the VIIRS tiles which they intersect with 
#'     and thus need to be retrieved to process their nightlight imagery. Since some functions use this 
#'     dataframe for long-term processing, omitCountries can eliminate countries that should be excluded
#'     from the list hence from processing. Countries can be added in the omitCountries function.
#'     Default is "none".
#'
#' @param ctryCode The country's ISO3 code
#'
#' @return None
#'
#' @examples
#' mapCtryPolyToTiles() map all countries
#' 
#' mapCtryPolyToTiles(ctryCodes="all", omitCountries="none") map all countries, no countries omitted
#' 
#' mapCtryPolyToTiles(omitCountries=c("error", "missing")) will not omit countries that do not have polygons 
#' on GADM
#'
#' @export
getTilesCtryIntersect <- function(ctryCode)
{
  ctryISO3 <- rwmGetISO3(ctryCode)
  
  #print(ctryISO3)
  
  if (is.na(ctryISO3) || ctryISO3 == "")
    return("Unknown country")
  
  idx <- which(map@data$ISO3 == ctryISO3)
  
  ctryCodeTiles <- NULL
  
  ctryPolys <- map@polygons[[idx]]
  
  #create a SpatialPolygons object with a list of 1 list of Polygons
  ctrySpPolys <- SpatialPolygons(Srl = list(ctryPolys))
  
  crs(ctrySpPolys) <- CRS(wgs84)
  
  ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")
  
  ctryCodeTiles <- tilesPolygonIntersect(ctrySpPolys)

  #Plot for debug  
  #plot(tSpPolysDFs, add=TRUE)
  #plot(ctrySpPolysDF, add=TRUE)
  
  return (ctryCodeTiles)
}


#' Get the index of a tile given its name
#'
#' Get the index of a VIIRS tile as given by getNlTiles() given its name
#'
#' @param tile name as given by getNlTiles()
#'
#' @return Integer index of the tile
#'
#' @examples
#' tileIdx <- tileName2Idx("00N060W")
#'
#' @export
tileName2Idx <- function(tileName)
{
  if (missing(tileName))
    stop("Required parameter tileName")
  
  if (!existsNlTiles())
    nlTiles <- getNlTiles()
  
  return (which(nlTiles$name == tileName))
}

#' Get the name of a tile given its index
#'
#' Get the name of a VIIRS tile as given by getNlTiles() given its index
#'
#' @param tile index as given by getNlTiles()
#'
#' @return Character name of the tile
#'
#' @examples
#' tileIdx <- tileName2Idx("00N060W")
#'
#' @export
tileIdx2Name <- function(tileNum)
{
  if (!existsNlTiles())
    nlTiles <- getNlTiles()
  
  return (nlTiles[tileNum, "name"])
}


#' Get the list of VIIRS tiles that a polygon intersects with
#'
#' Get the list a VIIRS tiles that a polygon intersects with
#'
#' @param a SpatialPolygon or SpatialPolygons
#'
#' @return Character vector of the intersecting tiles as given by getNlTiles()
#'
#' @examples
#' ctryShapefile <- dnldCtryPoly("KEN")
#' ctryPoly <- rgdal::readOGR(getPolyFnamePath("KEN"), getCtryShpLyrName("KEN",0))
#' tileList <- tilesPolygonIntersect(ctryPoly)
#'
#' @export
tilesPolygonIntersect <- function(shp_polygon)
{
  #given a polygon this function returns a list of the names of the viirs tiles
  #that it intersects with
  #Input: a Spatial Polygon e.g. from a loaded shapefile
  #Output: a character vector of tile names as given in the nlTiles dataframe
  
  if (!existsTSpPolysDFs())
  {
    tSpPolysDFs <- createNlTilesSpPolysDF()
  }
  
  if (!existsNlTiles())
    nlTiles <- getNlTiles()
  
  projection(shp_polygon) <- CRS(wgs84)
  
  #init list to hold tile indices
  tileIdx <- NULL
  
  #loop through the 6 tile rows in our SpatialPolygonsDataFrame
  for (i in 1:nrow(tSpPolysDFs))
  {
    #check whether the polygon intersects with the current tile
    tileIdx[i] <- gIntersects(tSpPolysDFs[i,], shp_polygon)
  }

  #return a list of tiles that intersected with the SpatialPolygon
  return (nlTiles[tileIdx, "name"])
}

#' Get the list of VIIRS tiles that a polygon intersects with
#'
#' Get the list a VIIRS tiles that a polygon intersects with
#'
#' @param a SpatialPolygon or SpatialPolygons
#'
#' @return Character vector of the intersecting tiles as given by getNlTiles()
#'
#' @examples
#' ctryShapefile <- dnldCtryPoly("KEN")
#' ctryPoly <- rgdal::readOGR(getPolyFnamePath("KEN"), getCtryShpLyrName("KEN",0))
#' tileList <- tilesPolygonIntersect(ctryPoly)
#'
#' @export
getNtLts <- function(inputYear)
{
  #dmsp/ols data from 1992-2013
  #snpp/viirs data from 2014 - present
  
  if (inputYear < 1992 || inputYear > year(now()))
    return ("Invalid year")
  
  if (inputYear < 2014)
    print("Downloading from DMSP/OLS")
  else
    print("Downloading from SNPP/VIIRS")
}

#' Function to return the url of the VIIRS tile to download
#'
#' Function to return the url of the VIIRS tile to download given the year, month, and nlTile index
#'
#' @param inYear
#' 
#' @param inMonth character month in MM format e.g. Jan="01", Feb="02"
#' 
#' @param inTile The integer index of the tile to download as given by getNlTiles()
#'
#' @return Character string Url of the VIIRS tile file
#'
#' @examples
#' tileUrl <- getNtLtsUrlViirs("2012", "04", "1")
#'
#' @export
getNtLtsUrlViirs <- function(inYear, inMonth, inTile)
{
  if (!existsNlTiles())
    nlTiles <- getNlTiles()
  
  inYear <- as.character(inYear)
   
  nMonth <- as.character(inMonth)

  #nlTile is a global list
  
  #the page that lists all available nightlight files
  ntLtsPageHtml <- "https://www.ngdc.noaa.gov/eog/viirs/download_mon_mos_iframe.html"
  
  #the local name of the file once downloaded
  ntLtsPageLocalName <- "ntltspageviirs.html"

  #if the file does not exist or is older than a week download it afresh
  #not working. download.file does not seem to update mtime
  if (!file.exists(ntLtsPageLocalName) || (date(now()) - date(file.mtime(ntLtsPageLocalName)) > as.difftime(period("1 day"))) || file.size(ntLtsPageLocalName) == 0)
  {
    download.file(url = ntLtsPageHtml, destfile = ntLtsPageLocalName, method = "wget", extra = "  -N --timestamping --no-use-server-timestamps")
  }
  #else
  #  print(paste0(ntLtsPageHtml, " already downloaded"))
  
  #read in the html page
  ntLtsPage <- readr::read_lines(ntLtsPageLocalName)
  
  #search for a line containing the patterns that make the files unique i.e.
  #1. SVDNB_npp_20121001 - year+month+01
  #2. vcmcfg - for file with intensity as opposed to cloud-free counts (vcmslcfg)
  #sample url: https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201210/vcmcfg/SVDNB_npp_20121001-20121031_75N180W_vcmcfg_v10_c201602051401.tgz
  
  #create the pattern
  ntLtsPageRgxp <- paste0("SVDNB_npp_", inYear, inMonth, "01.*", nlTiles[inTile,"name"], ".*vcmcfg")
  
  #search for the pattern in the page
  ntLtsPageHtml <- ntLtsPage[grep(pattern = ntLtsPageRgxp, x=ntLtsPage)]
  
  #split the output on quotes since this url is of the form ...<a href="URL"download> ...
  #the url is in the second position
  ntLtsPageUrl <- unlist(strsplit(ntLtsPageHtml, '"'))[2]

  #****NOTE: temp for testing using local download****
  #
  #fname <- stringr::str_extract(ntLtsPageUrl, "SVDNB.*.tgz")
  #ntLtsPageUrl <- paste0("http://localhost/", fname)
  #
  #****DELETE WHEN DONE****
  
  return (ntLtsPageUrl)
}

#' Check if a year is valid for a given nightlight type
#'
#' Check if a year is valid for a given nightlight type
#'
#' @param year
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlYearNum("2014","VIIRS")
#'
#' @export
validNlYearNum <- function(yearNum, nlType)
{
  if (missing(yearNum))
    stop("Missing parameter yearNum")
  
  if (missing(nlType) || (nlType != "OLS" && nlType != "VIIRS"))
    stop("Missing or invalid required parameter nlType")
  
  yearNum <- as.character(yearNum)
  
  if (class(yearNum) != "character" || yearNum =="" || length(grep("[^[:digit:]]", yearNum) > 0))
    return(FALSE)
  
  nlY <- as.numeric(yearNum)
  
  if (nlType == "OLS")
  {
    if (nlY > 1994 && nlY < 2013)
      return(TRUE)
    else
      return(FALSE)
  }
  else
  if (nlType=="VIIRS")
  {
    if (nlY >= 2012 && nlY <= lubridate::year(lubridate::now()))
      return(TRUE)
    else
      return(FALSE)
  }
  else
    return (FALSE)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
validNlMonthNum <- function(monthNum, nlType="VIIRS")
{
  if (missing(monthNum))
    stop("Missing required parameter monthNum")

  monthNum <- as.character(monthNum)
  nlType <- as.character(nlType)

  if (nlType!="VIIRS")
    stop("nlMonth only valid for nlType=\"VIIRS\"")
    
  if (class(monthNum) != "character" || monthNum =="" || length(grep("[^[:digit:]]", monthNum) > 0))
    return(FALSE)
 
  nlM <- as.numeric(monthNum)
  
  if (nlM >= 1 && nlM <= 12)
    return(TRUE)
  else
    return(FALSE)
}


#' Check if a tile index number is valid for a given nightlight type
#'
#' Check if a tile number is valid for a given nightlight type. Note tile num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param nlTileNum the index of the tile
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validTileNum("1","VIIRS")
#'  returns TRUE
#'
#' validTileNum("9","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("2","OLS")
#'  returns FALSE
#'  
#' @export
validNlTileNum <- function(nlTileNum, nlType)
{
  nlTileNum <- as.character(nlTileNum)
  
  if (missing(nlTileNum))
    stop("Missing parameter nlTileNum")
  
  if (class(nlTileNum) != "character" || nlTileNum =="" || length(grep("[^[:digit:]]", nlTileNum) > 0))
    return(FALSE)
  
  if(!exists("nlTiles"))
    nlTiles <- getNlTiles()
  
  nlT <- as.numeric(nlTileNum)
  
  if (nlT >= 1 && nlT <= length(nlTiles))
    return(TRUE)
  else
    return(FALSE)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNtLtsZipLclNameVIIRS <- function(nlYear, nlMonth, tileNum, dir=dirRasterVIIRS)
{
  nlType <- "VIIRS"
  
  if (missing(nlYear) || !validNlYearNum(nlYear, nlType))
    stop("Missing or invalid required parameter nlYear")
  
  if (missing(nlMonth) || !validNlMonthNum(nlMonth))
    stop("Missing or invalid required parameter nlMonth")
  
  if (missing(tileNum) || !validNlTileNum(tileNum))
    stop("Missing or invalid required parameter tileNum")

  if (missing(dir) && !dir.exists(dir))
  {
    message("Invalid directory ", dir ,". Using default directory \"", getwd(), "/tiles\"")
    
    #TODO: this is not correct! Not the right place to create the directory!
    if (!dir.exists(dir))
    {
      message("creating raster tiles directory")
      
      dir.create("tiles")
    }
  }

  #TODO: create function to return the filename
  #TODO: rename this function since it returns a path not a filename
  return (paste0(dir, "/viirs_", nlYear, "_", nlMonth, "_", tileIdx2Name(tileNum), ".tgz"))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNtLtsTifLclNameVIIRS <- function(nlYear, nlMonth, tileNum, dir=dirRasterVIIRS)
{
  if (missing(nlYear))
    stop("Missing nlYear")
  
  if (missing(nlMonth))
    stop("missing nlMonth")
  
  if (missing(tileNum))
    stop("Missing tileNum")

  if (missing(dir) && !dir.exists(dir))
  {
    message("Invalid directory ", dir ,". Using default directory \"", getwd(), "/tiles\"")

    #TODO: this is not correct! Not the right place to create the directory!
    if (!dir.exists(dir))
    {
      message("creating raster tiles directory")
      
      dir.create("tiles")
    }
  }
  
  #TODO: create function to return the filename
  #TODO: rename this function since it returns a path not a filename
  return (paste0(dir, "/viirs_", nlYear, "_", nlMonth, "_", tileIdx2Name(tileNum), ".tif"))
}

#' Download VIIRS nightlight tile
#'
#' Download VIIRS nightlight tile
#'
#' @param nlYear the year in "YYYY"/"%Y" format e.g. "2012"
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param tileNum the index of the tile as given by getNlTiles()
#' 
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' result <- getNtLtsViirs("2012", "05", "1")
#' 
#' if (result)
#'   print("download successful")
#'  
#' @export
getNtLtsViirs <- function(nlYear, nlMonth, tileNum)
{
  nlType <- "VIIRS"
  
  rsltDnld <- NA
  
  ntLtsZipLocalNameVIIRS <- getNtLtsZipLclNameVIIRS(nlYear, nlMonth, tileNum)
  ntLtsTifLocalNameVIIRS <- getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)
  
  #if (!file.exists(ntLtsZipLocalNameVIIRS) && !file.exists(ntLtsTifLocalNameVIIRS))
  if (!file.exists(ntLtsTifLocalNameVIIRS))
  {
    ntLtsFileUrl <- getNtLtsUrlViirs(nlYear, nlMonth, tileNum)
    
    #rsltDnld <- download.file(ntLtsFileUrl, ntLtsZipLocalNameVIIRS, mode = "wb", method = "wget", extra = "-c")
    rsltDnld <- system(paste0("aria2c -c -x2 ", ntLtsFileUrl, " -o ", ntLtsZipLocalNameVIIRS))
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    print("File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if (rsltDnld == 0)
  {
    message("Extracting ", ntLtsZipLocalNameVIIRS, " ", base::date())
    
    if (!file.exists(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)))
    {
      message("Getting list of files in ", ntLtsZipLocalNameVIIRS, " ", base::date())
      
      tgzFileList <- untar(ntLtsZipLocalNameVIIRS, list = TRUE)
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
      
      if (is.null(tgzFileList))
      {
        message("Error extracting file list. ")
        
        return (-1)
      }
      
      tgzAvgRadFilename <- tgzFileList[grep("svdnb.*.avg_rade9.tif$",tgzFileList, ignore.case = T)]
      
      message("Decompressing ", tgzAvgRadFilename, " ", base::date())
      
      if(!file.exists(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)))
      {
        untar(ntLtsZipLocalNameVIIRS, files = tgzAvgRadFilename, exdir = dirRasterVIIRS)
      
        file.rename(paste0(dirRasterVIIRS,"/",tgzAvgRadFilename), getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum))

        file.remove(ntLtsZipLocalNameVIIRS)
      }
    }
    else
    {
      message("TIF file found")
    }
  }
  
  return (rsltDnld == 0)
}


#' Mask 
#'
#' Download VIIRS nightlight tile
#'
#' @param nlYear the year in "YYYY"/"%Y" format e.g. "2012"
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param tileNum the index of the tile as given by getNlTiles()
#' 
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' result <- getNtLtsViirs("2012", "05", "1")
#' 
#' if (result)
#'   print("download successful")
#'  
#' @export
masq_viirs <- function(shp,rast,i)
{
  #based on masq function from https://commercedataservice.github.io/tutorial_viirs_part1/
  #slightly modified to use faster masking method by converting the raster to vector
  #Extract one polygon based on index value i
  polygon <- shp[i,] #extract one polygon
  
  extent <- extent(polygon) #extract the polygon extent 
  
  #Raster extract
  outer <- crop(rast, extent) #extract raster by polygon extent

  #inner <- mask(outer,polygon) #keeps values from raster extract that are within polygon
  
  inner <- rasterize(polygon, outer, mask=TRUE) #crops to polygon edge & converts to raster
  
  #Convert cropped raster into a vector
  #Specify coordinates
  #coords <- expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
  #                      seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
  
  #Convert raster into vector
  data <- as.vector(inner)
  
  ##THIS SECTION NEEDS WORK. confirm error and NA values and handling of the same
  
  #data <- data[!is.na(data)] #keep non-NA values only ... shall this affect mask values?
  #data[is.na(data)] <- 0
  data[data < 0] <- NA #any negative values are either recording problems or error values as per: 
  
  return(data) 
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNtLtsUrlOls <- function(inYear, inTile)
{
  inYear <- as.character(inYear)
  
  #Function to return the url of the file to download given the year, month, and nlTile index
  #nlTile is a global list
  
  ntLtsBaseUrl <- "https://www.ngdc.noaa.gov/"
  
  #the page that lists all available nightlight files
  ntLtsPageHtml <- "https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html"
  
  #the local name of the file once downloaded
  ntLtsPageLocalName <- "ntltspageols.html"
  
  #if the file does not exist or is older than a week download it afresh
  #not working. download.file does not seem to update mtime
  if (!file.exists(ntLtsPageLocalName) || (date(now()) - date(file.mtime(ntLtsPageLocalName)) > as.difftime(period("1 day"))))
  {
    download.file(url = ntLtsPageHtml, destfile = ntLtsPageLocalName, method = "wget", extra = "-N")
  }
  #else
  #  print(paste0(ntLtsPageHtml, " already downloaded"))
  
  #read in the html page
  ntLtsPage <- readr::read_lines(ntLtsPageLocalName)
  
  #search for a line containing the patterns that make the files unique i.e.
  #1. SVDNB_npp_20121001 - year+month+01
  #2. vcmcfg - for file with intensity as opposed to cloud-free counts (vcmslcfg)
  #sample url: https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201210/vcmcfg/SVDNB_npp_20121001-20121031_75N180W_vcmcfg_v10_c201602051401.tgz
  
  #create the pattern
  ntLtsPageRgxp <- paste0("F.*.", inYear,".*\\.tar")
  
  #search for the pattern in the page
  ntLtsPageHtml <- ntLtsPage[grep(pattern = ntLtsPageRgxp, x=ntLtsPage)]
  
  #split the output on quotes since this url is of the form ...<a href="URL"download> ...
  #the url is in the second position
  ntLtsPageUrl <- unlist(strsplit(ntLtsPageHtml, '"'))
  
  ntLtsPageUrl <- ntLtsPageUrl[grep("v4composite", ntLtsPageUrl)]
  
  ntLtsPageUrl <- unlist(lapply(ntLtsPageUrl, FUN=function(x) paste0(ntLtsBaseUrl, x)))
  
  #****NOTE: temp for testing using local download****
  #
  #fname <- stringr::str_extract(ntLtsPageUrl, "SVDNB.*.tgz")
  #ntLtsPageUrl <- paste0("http://localhost/", fname)
  #
  #****DELETE WHEN DONE****
  
  return (ntLtsPageUrl)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNtLtsZipLclNameOLS <- function(nlYear, tileNum)
{
  return (paste0(dirRasterOLS, "/ols_", nlYear, "_", tileNum, ".tgz"))
}


#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNtLtsTifLclNameOLS <- function(nlYear, tileNum)
{
  return (paste0(dirRasterOLS, "/ols_", nlYear, "_", tileNum, ".tif"))
}

#' Download VIIRS nightlight tile
#'
#' Download VIIRS nightlight tile
#'
#' @param nlYear the year in "YYYY"/"%Y" format e.g. "2012"
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param tileNum the index of the tile as given by getNlTiles()
#' 
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' result <- getNtLtsViirs("2012", "05", "1")
#' 
#' if (result)
#'   print("download successful")
#'  
#' @export
getNtLtsOLS <- function(nlYear, tileNum)
{
  rsltDnld <- NA
  
  ntLtsZipLocalName <- getNtLtsZipLclNameOLS(nlYear, tileNum)
  
  if (!file.exists(ntLtsZipLocalNameOLS))
  {
    ntLtsFileUrl <- getNtLtsUrlOLS(nlYear)
    
    rsltDnld <- download.file(ntLtsFileUrl, ntLtsZipLocalName, mode = "wb", method = "wget", extra = "-c")
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    print("File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  if (rsltDnld == 0)
  {
    message("Extracting ", ntLtsZipLocalName, " ", base::date())
    
    if (!file.exists(getNtLtsTifLclNameOLS(nlYear, nlMonth, tileNum)))
    {
      message("Getting list of files in ", ntLtsZipLocalName, " ", base::date())
      
      tgzFileList <- untar(ntLtsZipLocalName, list = TRUE)
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
      
      if (is.null(tgzFileList))
      {
        message("Error extracting file list. ")
        
        return (-1)
      }
      
      tgzAvgRadFilename <- tgzFileList[grep("svdnb.*.avg_rade9.tif$",tgzFileList, ignore.case = T)]
      
      message("Decompressing ", tgzAvgRadFilename, " ", base::date())
      
      if(!file.exists(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileNum)))
      {
        untar(ntLtsZipLocalName, files = tgzAvgRadFilename, exdir = dirRasterOLS)
        
        file.rename(paste0(dirRasterOLS,"/",tgzAvgRadFilename), getNtLtsTifLclNameOLS(nlYear, tileNum))
      }
    }
    else
    {
      message("TGZ file found")
    }
  }
  
  return (rsltDnld == 0)
}

#' Mask 
#'
#' Download VIIRS nightlight tile
#'
#' @param nlYear the year in "YYYY"/"%Y" format e.g. "2012"
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param tileNum the index of the tile as given by getNlTiles()
#' 
#' @return TRUE/FALSE Whether the download was successful
#'
#' @examples
#' result <- getNtLtsViirs("2012", "05", "1")
#' 
#' if (result)
#'   print("download successful")
#'  
#' @export
masq_ols <- function(shp, rast, i)
{
  #based on masq function from https://commercedataservice.github.io/tutorial_viirs_part1/
  #Extract one polygon based on index value i
  polygon <- shp[i,] #extract one polygon
  extent <- extent(polygon) #extract the polygon extent 
  
  #Raster extract
  outer <- crop(rast, extent) #extract raster by polygon extent
  #inner <- mask(outer,polygon) #keeps values from raster extract that are within polygon
  inner <- rasterize(polygon, outer, mask=TRUE) #crops to polygon edge & converts to raster
  
  #Convert cropped raster into a vector
  #Specify coordinates
  coords <- expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
                        seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
  #Convert raster into vector
  data <- as.vector(inner)
  
  ##THIS SECTION NEEDS WORK. confirm error and NA values and handling of the same
  #keep non-NA values only?
  #data <- data[!is.na(data)]
  
  #in DMSP-OLS 255 == NA
  data[which(data == 255)] <- NA
  
  #non-negative values are errors replace with 0?
  data[data < 0] <- 0
  
  return(data)
}

#' Convert a country name to its ISO3 code
#'
#' Convert a country name to its ISO3 code. Exposes the rworldmap function rwmGetISO3(ctryName). See the examples
#' 
#' @param ctryCode
#' 
#' @return Character full country name
#'
#' @examples
#' ctryNameToCode("kenya")
#'   #returns "KEN"
#'  
#' ctryNameToCode("ken")
#'   #returns "KEN"
#'   
#' @export
ctryNameToCode <- function(ctryName)
{
  return (rworldmap::rwmGetISO3(ctryName))
}

#' Convert a country ISO3 code to the full name
#'
#' Convert a country ISO3 code to the full name. Exposes the rworldmap function isoToName(ctryCode)
#' 
#' @param ctryCode
#' 
#' @return Character full country name
#'
#' @examples
#' ctryCodeToName("KEN")
#'  
#' @export
ctryCodeToNAME <- function(ctryCode)
{
  return(rworldmap::isoToName(ctryCode))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
ctryPolyLyrNames <- function (nLyrs)
{
  #the repeat pattern required to create columns in the format 1,1,2,2,3,3 ...
  #for col names: admlevel1_id, admlevel1_name, ..., admleveN_id, admlevelN_name
  #and polygon data col names: ID_1, NAME_1, ..., ID_N, NAME_N
  nums <- c(paste(1:nLyrs,1:nLyrs))
  
  nums <- unlist(strsplit(paste(nums, collapse = " "), " "))
  
  return(paste(c("ID_", "NAME_"), nums, sep=""))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
ctryPolyAdmColNames <- function (ctryPolyAdmLevels, nLyrs)
{
  #the repeat pattern required to create columns in the format 1,1,2,2,3,3 ...
  #for col names: admlevel1_id, admlevel1_name, ..., admleveN_id, admlevelN_name
  #and polygon data col names: ID_1, NAME_1, ..., ID_N, NAME_N
  nums <- c(paste(1:nLyrs,1:nLyrs))
  
  nums <- unlist(strsplit(paste(nums, collapse = " "), " "))
  
  return(paste(ctryPolyAdmLevels[nums, "name"], c("_id", "_name"), sep=""))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
processNLCountryOls <- function(cntryCode, nlYear)
{
  message("processNLCountryOLS: ")
  
  ctryPoly <- readOGR(getPolyFnamePath(ctryCode), getCtryShpLowestLyrName(ctryCode))
  
  ctryExtent <- extent(ctryPoly)
  
  projection(ctryPoly) <- CRS(wgs84)
  
  nlYear <- substr(nlYearMonth, 1, 4)
  
  nlMonth <- substr(nlYearMonth, 5, 6)
  
  if (existsCtryNlDataFile(ctryCode))
  {
    ctryNlDataDF <- read.csv(getCtryNlDataFnamePath(ctryCode),header = TRUE,sep = ",")
    
    existingDataCols <- names(ctryNlDataDF)
    
    existingYears <- existingDataCols[grep("^NL_[:alphanum:]*", existingDataCols)]
    
    existingYears <- stringr::str_replace(existingYears, "NL_OLS_", "")
    
    if(nlYear %in% existingYears)
    {
      message("Data exists for ", ctryCode, " ", nlYear)
      
      return(-1)
    }
  } else
  {
    #get the list of admin levels in the polygon shapefile
    ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode)
   
    #add the area as reported by the polygon shapefile as a convenience
    areas <- area(ctryPoly)
    
    if (length(ctryPolyAdmLevels) > 0)
    {
      #conver to lower case for consistency
      ctryPolyAdmLevels <- tolower(ctryPolyAdmLevels)
      
      #the number of admin levels
      nLyrs <- length(ctryPolyAdmLevels)
      
      #the names of the layers which will become column names
      ctryPolyAdmCols <- ctryPolyLyrNames(nLyrs)
  
      #pull the ID_ and NAME_ cols from layer1 to lowest layer (layer0 has country code not req'd)
      ctryNlDataDF <- ctryPoly@data[,eval(ctryPolyAdmCols)]
    

      #we add the country code to ensure even a renamed file is identifiable
      #repeat ctryCode for each row in the polygon. equiv of picking layer0
      ctryCodeCol <- rep(ctryCode, nrow(ctryNlDataDF))
      
      #combine the columns
      ctryNlDataDF <- cbind(ctryCodeCol, ctryNlDataDF, areas)
      
      ctryPolyColNames <- ctryPolyAdmColNames(ctryPolyAdmLevels, nLyrs)
      
      #add the country_code and area columns to the dataframe
      ctryPolyColNames <- c("country_code", ctryPolyColNames, "area_sq_km")
      
      names(ctryNlDataDF) <- ctryPolyColNames
      
    } else
    {
      ctryNlDataDF <- data.frame("country_code"=ctryCode, "area_sq_km"=areas)
    }
  }
  
  if(!file.exists(getCtryRasterOutputFname(ctryCode, nlYearMonth)))
  {
    message("Begin processing ", nlYearMonth, " ", base::date())
    
    message("Reading in the rasters " , base::date())
    
    tileList <- getCtryCodeTileList(ctryCode)
    
    ctryRastCropped <- NULL
    
    for (tile in tileList)
    {
      satYear <- substr(tar_fl,1,7)
      
      message("Extracting ", tar_fl, " ", base::date())
      
      message("Getting list of files in ", tar_fl, " ", base::date())
      #get a list of files in the tar archive
      tar_file_list <- untar(tar_fl, list = TRUE)
      
      #get the nightlight data filename
      #the nightlight data filename has the format "web.avg_vis.tif.gz"
      #    tgz_file <- tar_file_list[grep(".*web\\.avg_vis\\.tif\\.gz$",tar_file_list, ignore.case = T)]
      tgz_file <- tar_file_list[grep(".*stable_lights\\.avg_vis\\.tif\\.gz$",tar_file_list, ignore.case = T)]
      
      #extract the nightlight data file
      untar(tar_fl, tgz_file)
      
      #the tif has the same name as the compressed file without the .gz
      tif_file <- str_replace(tgz_file, ".gz", "")
      
      message("Decompressing ", tgz_file, " ", base::date())
      
      gunzip(tgz_file)
      #no need to delete the gz since gunzip deletes the compressed version
    }
  
    #for()
    {
      message("Reading in the raster " , base::date())
      rastGlobal <- raster(tif_file)
      
      projection(rastGlobal) <- CRS(wgs84)
      
      message("Cropping the raster ", base::date())
      ctryRast <- crop(rastGlobal, ctryPoly)
      
      message("Releasing the raster variables")
      rm(rastGlobal)
      
      message("Deleting the raster files ", base::date())
      
      unlink(tif_file)
      
      #merge rasters
    }
    
    #write merged raster
  }
  else
  {
    #read in the raster  
  }
    
  gc()
  
  message("Masking the merged raster ", base::date())
  #rast_ke <- mask(rast_ke, ke_shp_ward)
  rast_ke <- rasterize(ctryPoly, rast_ke, mask=TRUE) #crops to polygon edge & converts to raster
  
  message("Writing the merged raster to disk ", base::date())
  writeRaster(x = rast_ke, filename = paste0(dirRasterOutput, nlYear, ".tif"), overwrite=TRUE)
  
  registerDoParallel(cores=6)
  
  message("Begin extracting the data from the merged raster ", base::date())
  sumAvgRad <- foreach(i=1:nrow(ctryPoly@data), .combine=rbind) %dopar% 
  {
    #message("Extracting data from polygon " , i, " ", base::date())
    dat <- masq(ctryPoly, rast_ke,i)
    
    #message("Calculating the mean of polygon ", i, " ", base::date())
    
    #calculate and return the sum of the mean of all the pixels
    data.frame(mean = sum(dat, na.rm=TRUE))
  }
  
  #merge the calculated means for the polygon as a new column
  ctryNlDataDF <- cbind(extctryNlDract, sumAvgRad)
  
  #name the new column with the yearmonth of the data
  names(extract)[ncol(extract)] <- paste0("NL_OLS_", nlYear)
  
  message("DONE processing ", nlYear, " ", base::date())

  message("COMPLETE. Writing data to disk")
  write.table(extract, getCtryNlDataFname(ctryCode), row.names= F, sep = ",")
    
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
createCtryNlDataDF <- function(ctryCode)
{
  ctryPoly <- readOGR(getPolyFnamePath(ctryCode), getCtryShpLowestLyrName(ctryCode))
  
  ctryExtent <- extent(ctryPoly)
  
  projection(ctryPoly) <- CRS(wgs84)
  
  #get the list of admin levels in the polygon shapefile
  ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode)
  
  #conver to lower case for consistency
  ctryPolyAdmLevels <- tolower(ctryPolyAdmLevels)
  
  #add the area as reported by the polygon shapefile as a convenience
  areas <- area(ctryPoly)
  
  if (length(ctryPolyAdmLevels) > 0)
  {
    #When a country does not have lower administrative levels
    
    #the number of admin levels
    nLyrs <- length(ctryPolyAdmLevels)
    
    #the repeat pattern required to create columns in the format 1,1,2,2,3,3 ...
    #for col names: admlevel1_id, admlevel1_name, ..., admleveN_id, admlevelN_name
    #and polygon data col names: ID_1, NAME_1, ..., ID_N, NAME_N
    #nums <- c(paste(1:nLyrs,1:nLyrs))
    
    #nums <- unlist(strsplit(paste(nums, collapse = " "), " "))
    
    #ctryPolyAdmCols <- paste(c("ID_", "NAME_"), nums, sep="")
    
    ctryPolyAdmCols <- paste(c("NAME_"), 1:nLyrs, sep="")
    
    #pull the ID_ and NAME_ cols from layer1 to lowest layer (layer0 has country code not req'd)
    ctryNlDataDF <- as.data.frame(ctryPoly@data[,eval(ctryPolyAdmCols)])
    
    #add the area as reported by the polygon shapefile as a convenience
    areas <- area(ctryPoly)
    
    #we add the country code to ensure even a renamed file is identifiable
    #repeat ctryCode for each row in the polygon. equiv of picking layer0
    ctryCodeCol <- rep(ctryCode, nrow(ctryNlDataDF))
    
    #combine the columns
    ctryNlDataDF <- cbind(ctryCodeCol, ctryNlDataDF, areas)
    
    #ctryPolyColNames <- paste(ctryPolyAdmLevels[nums, "name"], c("_id", "_name"), sep="")
    ctryPolyColNames <- ctryPolyAdmLevels
    
    #add the country_code and area columns to the dataframe
    ctryPolyColNames <- c("country_code", ctryPolyColNames, "area_sq_km")
    
    names(ctryNlDataDF) <- ctryPolyColNames
  } else
  {
    ctryNlDataDF <- data.frame("country_code"=ctryCode, "area_sq_km"=areas)
  }
  
  return(ctryNlDataDF)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' processNLCountriesViirs(ctryCodes, nlYearMonth)
#'  
#' @export
processNLCountriesViirs <- function(ctryCodes, nlYearMonth)
{
  for (nlCtryCode in nlCtryCodes)
  {
    processNLCountryVIIRS(ctryCode, nlYearMonth)
  }
}

#' Get the integer number of the layer
#'
#' Get the integer number of the layer. Is usually the last character in the name and is a digit. E.g. for the
#'     layername "KEN_adm3" the layer number is "3"
#' 
#' @param layerName - the name of the polygon layer
#' 
#' @return Integer layer number
#'
#' @examples
#' ctryShpLyrName2Num("KEN_adm0")
#'   #returns 0
#'  
#' @export
ctryShpLyrName2Num <- function(layerName)
{
  return(as.numeric(gsub("[^[:digit:]]", "", layerName)))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param ctryCode
#' 
#' @param nlYearMonth
#' 
#' @param cropMastMethod ("rast" or "gdal") Whether to use rasterize or gdal-based functions to crop and mask
#'     the country rasters
#' 
#' @return None
#'
#' @examples
#' processNLCountryVIIRS() #
#'  
#' @export
processNLCountryVIIRS <- function(ctryCode, nlYearMonth, cropMaskMethod="rast")
{
  if(missing(ctryCode) || class(ctryCode) != "character" || is.null(ctryCode) || ctryCode == "")
    stop("ctryCode missing")
  
  if(missing(nlYearMonth) || class(nlYearMonth) != "character" || is.null(nlYearMonth) || nlYearMonth == "")
    stop("nlYearMonth missing")
  
  if (missing(cropMaskMethod) || class(cropMaskMethod) != "character" || is.null(cropMaskMethod) || nlYearMonth == "")
    cropMaskMethod <- "rast"
  
  message("processNLCountryVIIRS: ", ctryCode, " ", nlYearMonth)
  
  nlYear <- substr(nlYearMonth, 1, 4)
  
  nlMonth <- substr(nlYearMonth, 5, 6)
  
  message("Check for existing data file")
  
  if (existsCtryNlDataFile(ctryCode))
  {
    message("Data file found: ", getCtryNlDataFnamePath(ctryCode))
    
    if(existsCtryNlDataVIIRS(ctryCode, nlYearMonth))
    {
      message("Data exists for ", ctryCode, " ", nlYearMonth, ". Skipping")
      
      return(-1)
    }
    
    message("Load country data file")
    ctryNlDataDF <- read.csv(getCtryNlDataFnamePath(ctryCode))
      
    message("Load country polygon lowest admin level")
    ctryPoly <- readOGR(getPolyFnamePath(ctryCode), getCtryShpLowestLyrName(ctryCode))
    
    ctryExtent <- extent(ctryPoly)
    
    projection(ctryPoly) <- CRS(wgs84)
    
  } else
  {
    message("Data file not found. Creating ...")
    
    ctryNlDataDF <- createCtryNlDataDF(ctryCode)
    
    message("Data file not found. Creating ... DONE")

    message("Load country polygon lowest admin level")
    ctryPoly <- readOGR(getPolyFnamePath(ctryCode), getCtryShpLowestLyrName(ctryCode))
  }

  if(!file.exists(getCtryRasterOutputFname(ctryCode, nlYearMonth)))
  {
    message("Begin processing ", nlYearMonth, " ", base::date())
  
    message("Reading in the rasters " , base::date())

    tileList <- getCtryCodeTileList(ctryCode)
    
    ctryRastCropped <- NULL

    for (tile in tileList)
    {
      rastFilename <- getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileName2Idx(tile))
      
      rastTile <- raster(rastFilename)
      
      projection(rastTile) <- CRS(wgs84)
      
      message("Cropping the rasters", base::date())
      
      #extTempCrop <- crop(rastTile, ctryExtent)
      
      tempCrop <- crop(rastTile, ctryPoly)
      
      if(is.null(ctryRastCropped))
      {
        ctryRastCropped <- tempCrop
        
        #ctryExtCropped <- extTempCrop
      }
      else
      {
        ctryRastMerged <- ctryRastCropped
        
        ctryRastCropped <- NULL
        
        ctryRastCropped <- merge(ctryRastMerged, tempCrop)
        
        rm(ctryRastMerged)
        
        #ctryExtMerged <- ctryExtCropped
        
        #ctryExtCropped <- NULL
        
        #ctryExtCropped <- merge(ctryExtMerged, tempCrop)
        
      }
      
      rm(tempCrop)
    }
    
    rm(rastTile)
    
    gc()

    message("Masking the merged raster ", base::date())
    
    if (cropMaskMethod == "rast")
    {
    
      #RASTERIZE
      message("Crop and mask using rasterize ", base::date())
      ctryRastCropped <- rasterize(ctryPoly, ctryRastCropped, mask=TRUE) #crops to polygon edge & converts to raster
      
      message("Writing the merged raster to disk ", base::date())
      
      writeRaster(x = ctryRastCropped, filename = getCtryRasterOutputFname(ctryCode,nlYearMonth), overwrite=TRUE)
      
      message("Crop and mask using rasterize ... Done", base::date())
    }
    else if (cropMaskMethod == "gdal")
    {
      message("Crop and mask using gdalwarp ... ", base::date())
      
      #GDALWARP
      rstTmp <- paste0(tempfile(), ".tif")
  
      message("Writing merged raster to disk for gdal")
      
      writeRaster(ctryRastCropped, rstTmp)
      
      output_file_vrt <- paste0(ctryCode, "_", nlYearMonth, ".vrt")
      
      if (file.exists(output_file_vrt))
        file.remove(output_file_vrt)
      
      message("gdalwarp ",base::date())
      
      gdalwarp(srcfile=rstTmp, dstfile=output_file_vrt, s_srs=wgs84, t_srs=wgs84, cutline=getPolyFnamePath(ctryCode), cl= getCtryShpLyrName(ctryCode,0), multi=TRUE, wm=2048, wo="NUM_THREADS=ALL_CPUS")
      
      message("gdal_translate ", base::date())
      gdal_translate(co = "compress=LZW", src_dataset = output_file_vrt, dst_dataset = getCtryRasterOutputFname(ctryCode,nlYearMonth))
      
      message("Deleting the component rasters ", base::date())
      
      file.remove(rstTmp)
      file.remove(output_file_vrt)
      
      ctryRastCropped <- raster(getCtryRasterOutputFname(ctryCode, nlYearMonth))
      #GDALWARP
      message("Crop and mask using gdalwarp ... DONE", base::date())
    }
  }
  else
  {
    rastFilename <- getCtryRasterOutputFname(ctryCode, nlYearMonth)
    
    ctryRastCropped <- raster(rastFilename)
    
    crs(ctryRastCropped) <- CRS(wgs84)
  }
  
  message("Create web version of raster", base::date())
  
  #attempting to obtain QGIS display style grayscale stretch minmax of 2%-98% of values
  #message("calculating quantile 2 and 98 ", base::date())
  #system.time(qnts <- sapply(1:1000,FUN =  function(x) quantile(sampleRandom(ctryRastCropped,100), c(0.02,0.98)),simplify = T))

  #qnt2 <- mean(qnts[1,])
  #qnt98 <- mean(qnts[2,])
  
  #cmd <- paste0("gdal_translate -co TILED=YES -co COMPRESS=JPEG -ot Byte -scale ", qnt2, " ", qnt98," 0 255 ", getCtryRasterOutputFname(ctryCode,nlYearMonth), " ", dirRasterWeb, "/", ctryCode, "_", nlYearMonth, "_JPEG.tif")
  
  message("Create web raster ", base::date())
  system(cmd)
  
  message("Begin extracting the data from the merged raster ", base::date())
  
  if (extractMethod == "rast")
    sumAvgRad <- fnSumAvgRadRast(ctryPoly, ctryRastCropped)
  else if (extractMethod == "gdal")
    sumAvgRad <- fnSumAvgRadGdal(ctryCode, ctryPoly, nlYearMonth)
  
  #merge the calculated means for the polygon as a new column
  ctryNlDataDF <- cbind(ctryNlDataDF, sumAvgRad)
  
  #name the new column with the yearmonth of the data
  names(ctryNlDataDF)[ncol(ctryNlDataDF)] <- paste0("NL_VIIRS_", nlYearMonth)
  
  cols <- names(ctryNlDataDF)
  
  nlDataColIdx <- grep("^NL_", cols)
  
  nlDataColNames <- cols[nlDataColIdx]
  
  nlDataColNames <- nlDataColNames[order(nlDataColNames)]
  
  newNlDataColNames <- c(cols[-nlDataColIdx], nlDataColNames)
  
  #rearrange NL columns
  ctryNlDataDF <- ctryNlDataDF[,newNlDataColNames]
  
  message("DONE processing ", ctryCode, " ", nlYearMonth, " ", base::date())
  
  message("COMPLETE. Writing data to disk")
  write.table(ctryNlDataDF, getCtryNlDataFnamePath(ctryCode), row.names= F, sep = ",")
  
  rm (ctryRastCropped)
  
  removeTmpFiles(h = 0)
}


#' Get the full path to the file where the cropped VIIRS country raster is stored.
#'
#' Get the full path to the file where the cropped VIIRS country raster is stored. This file is created
#'     when processing the country before extracting the data. It can be used to re-process a country much faster
#'
#' @param ctryCode
#' 
#' @param nlYearMonth the year and month
#' 
#' @return Character full path to the cropped VIIRS country raster for a country and a given year and month
#'
#' @examples
#' getCtryRasterOutputFname("KEN","201412")
#'  
#' @export
getCtryRasterOutputFname <- function(ctryCode, nlYearMonth)
{
  return (paste0(dirRasterOutput, "/",ctryCode, "_", nlYearMonth,".tif"))
}


#' Check if a month number is valid for a given nightlight type
#'
#' Get the name of the data file. This function can be altered to name the file as required and consistently
#'     retrieve the name. Used in the function getCtryNlDataFnamePath to concat the directory path and this 
#'     filename. Currently all nlTypes are stored in one file. Can be altered to separate VIIRS and OLS data
#'     files for example.
#'
#' @param ctryCode
#' 
#' @return Character filename of the country data file
#'
#' @examples
#' ctryCode <- "KEN"
#' getCtryNlDataFname(ctryCode)
#'  #returns name of the ctry data file
#'  
#' @export
getCtryPolyUrl <- function(ctryCode)
{
  #Sample url: http://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
  basePolyUrl <- "http://biogeo.ucdavis.edu/data/gadm2.8/shp/"
  
  return (paste0(basePolyUrl, ctryCode, "_adm_shp.zip"))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Get the name of the data file. This function can be altered to name the file as required and consistently
#'     retrieve the name. Used in the function getCtryNlDataFnamePath to concat the directory path and this 
#'     filename. Currently all nlTypes are stored in one file. Can be altered to separate VIIRS and OLS data
#'     files for example.
#'
#' @param ctryCode
#' 
#' @return Character filename of the country data file
#'
#' @examples
#' ctryCode <- "KEN"
#' getCtryNlDataFname(ctryCode)
#'  #returns name of the ctry data file
#'  
#' @export
getCtryNlDataFname <- function(ctryCode)
{
  return (paste0(ctryCode, "_NLData.csv"))
}

#' Check if a country data file exists
#'
#' Check if a country data file exists. This will only be true if at least one month of data has been processed
#'     and saved
#' 
#' @param ctryCode
#' 
#' @return Character giving the full path to the data file of a country
#'
#' @examples
#' ctryCode <- "KEN"
#' ctryDF <- read.csv(getCtryNlDataFnamePath(ctryCode))
#'  returns DF with nightlight data for the country
#'  
#' @export
getCtryNlDataFnamePath <- function(ctryCode)
{
  return (paste0(dirNlData, "/", getCtryNlDataFname(ctryCode)))
}

#' Check if a country's data file exists
#'
#' Check if a country's data file exists
#'
#' @param ctryCode the ISO3 country code
#' 
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' ctryCode <- "KEN"
#' if(existsCtryNlDataFile(ctryCode))
#'  message("Data file for ", ctryCode, " found")
#'  
#' @export
existsCtryNlDataFile <- function(ctryCode)
{
  #for polygons look for shapefile dir
  return(file.exists(getCtryNlDataFnamePath(ctryCode)))
}

#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' Check if the decompressed country polygon has been downloaded and stored in the polygon folder
#'
#' @param ctryCode
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' polyFnamePathExists("KEN")
#'  TRUE/FALSE
#'  
#' @export
polyFnamePathExists <- function(ctryCode)
{
  #for polygons look for shapefile dir
  return(dir.exists(getPolyFnamePath(ctryCode)))
}

#' Check if the compressed country polygon has been downloaded and stored in the polygon folder
#'
#' Check if the compressed country polygon has been downloaded and stored in the polygon folder
#'
#' @param ctryCode
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' polyFnameZipExists("KEN")
#'  TRUE/FALSE
#'  
#' @export
polyFnameZipExists <- function(ctryCode)
{
  return(file.exists(getPolyFnameZip(ctryCode)))
}

#' Get the standard name of a polygon layer for a country
#'
#' Get the standard name of a polygon layer for a country. Used to refer to a polygon layer by name. 
#'     i.e. for CTRYCODE & lyrNum="0": lyrName="CTRYCODE_adm0", lyrNum="1": lyrName="KEN_adm1". Note this is
#'     different from the country official
#'     administration level name.
#'
#' @param ctryCode the ISO3 code for the country
#' 
#' @param lyrNum the order of the layer starting from 0 = country level, 1 = first admin level
#' 
#' @return Character layer name
#'
#' @examples
#' lyrName <- getCtryShpLyrName("KEN","0")) #top layer name
#'   #return "KEN_adm0"
#' @export
getCtryShpLyrName <- function(ctryCode, lyrNum)
{
  return(paste0(ctryCode, "_adm", lyrNum))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'  
#' @export
getCtryShpLowestLyrName <- function(ctryCode)
{
  layers <- ogrListLayers(getPolyFnamePath(ctryCode))
  
  admLayers <- layers[grep("adm", layers)]
  
  admLayerNums <- gsub("[^[:digit:]]", "", admLayers)
  
  lowestAdmLyrName <- admLayers[order(as.numeric(admLayerNums),decreasing = T)][1]
  
  return(lowestAdmLyrName)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getCtryPolyAdmLevelNames <- function(ctryCode)
{
  lowestLayer <- getCtryShpLowestLyrName(ctryCode)
  
  numLayers <- ctryShpLyrName2Num(lowestLayer)
  
  admLevels <- NULL
  
  if (numLayers > 0)
    for (lyrNum in 1:numLayers)
    {
      lyrPoly <- readOGR(getPolyFnamePath(ctryCode), getCtryShpLyrName(ctryCode, lyrNum))
      
      lvlTypeName <- paste0("TYPE_",lyrNum)
      
      lvlName <- as.character(unlist(lyrPoly@data[2,eval(lvlTypeName)]))
      
      lvlTypeEngName <- paste0("ENGTYPE_",lyrNum)
      
      lvlEngName <- as.character(unlist(lyrPoly@data[2,eval(lvlTypeEngName)]))
      
      if ((!is.na(lvlName) && !is.na(lvlEngName)) && lvlName != lvlEngName)
        lvlName <- paste0(lvlName, "_(", lvlEngName, ")")
      
      if (is.na(lvlName))
        lvlName <- "Unknown"
      
      admLevels <- c(admLevels, as.character(lvlName))
    }
  
  #admLevels <- as.data.frame(cbind(1:numLayers, admLevels))
  
  #names(admLevels) <- c("id", "name")
  
  return (admLevels)  
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
dnldCtryPoly <- function(ctryCode)
{
  fullPolyUrl <- getCtryPolyUrl(ctryCode)
  
  result <- NULL
  
  #if the path doesn't exist
  if (!polyFnamePathExists(ctryCode))
  {
    if (!polyFnameZipExists(ctryCode))
    {
      if(download.file(url = getCtryPolyUrl(ctryCode), destfile = getPolyFnameZip(ctryCode), method = "wget", mode = "wb", extra = "-c") == 0)
      {
        result <- unzip(getPolyFnameZip(ctryCode), exdir = getPolyFnamePath(ctryCode))
        file.remove(getPolyFnameZip(ctryCode))
      }
    }else
    {
      result <- unzip(getPolyFnameZip(ctryCode), exdir = getPolyFnamePath(ctryCode))
      file.remove(getPolyFnameZip(ctryCode))
    }
  }
  else
  {
    message("Polygon ", ctryCode, " already exists")
  }
  
  return (!is.null(result))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getAllNlYears <- function(nlType = "VIIRS")
{
  if (nlType == "OLS")
    return (1992:2013)
  else if (nlType == "VIIRS")
  {
    yrs <- 2012:year(now())
    mths <- c(paste("0",1:9, sep= ""),10:12)
    
    currYrMth <- paste0(year(now()), ifelse(month(now())<10,paste("0", month(now()), sep=""), month(now())))
    
    nlYrMths <- unlist(lapply(yrs, FUN = function(x) paste(x,mths,sep="")))
    
    nlYrMths <- nlYrMths[nlYrMths >= "201204" & nlYrMths <= currYrMth]
    
    return (nlYrMths)
  }
  else
    return()
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getAllNlCtryCodes <- function(omit="none")
{
  #omit is a vector and can contain "long", "missing" or "error"
  #if omit is "none" do not exclude any countries
  #if omit is "all", empty or NA set to default: omit=c("long", "missing", "error")
  
  omit <- tolower(omit)
  
  if(omit != "none" && (omit == "all" || !(omit %in% c("long", "missing", "error")) || is.na(omit)))
    omit <- c("long", "missing", "error")

  tooLongProcessing <- ""
  missingPolygon <- ""
  errorProcessing <- ""
  
  if ("long" %in% omit)
    tooLongProcessing <- c("RUS", "BRA", "USA", "ATA", "FRA")
  
  if ("missing" %in% omit)
    missingPolygon <- c("CYN",  "KOS", "Ashm", "Gaza", "IOA", "KAS")
  
  if ("error" %in% omit)
    errorProcessing <- c("ATF", "GNQ", "KIR", "NZL", "CAN", "MUS")
  
  #consolidate the list of countries to omit
  omitCountries <- unlist(c(tooLongProcessing, missingPolygon, errorProcessing))
  
  #rworldmap has more country codes in countryRegions$ISO3 than in the map itself
  #select ctryCodes from the map data itself
  map <- rworldmap::getMap()

  #some polygons have problems. use cleangeo package to rectify
  map <- clgeo_Clean(map)
  
  #get the list of country codes from the rworldmap
  ctryCodes <- as.character(map@data$ISO3)
  
  #remove all omitCountries from the list
  ctryCodes <- subset(ctryCodes, !(ctryCodes %in% omitCountries))
  
  #sort the country codes in ascending alphabetical order
  ctryCodes <- ctryCodes[order(ctryCodes)]

  return (ctryCodes)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNlType <- function(nlYear)
{
  if (nlYear < 1992 || nlYear > year(now()))
    return(NA)

  if (nlYear > 1992 && nlYear < 2012)
    return("OLS")
  else
    return("VIIRS")
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getPolyFname <- function(ctryCode)
{
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(ctryCode, "_adm_shp")
  
  return (polyFname)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getPolyFnamePath <- function(ctryCode)
{
  #check for the shapefile directory created with 
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFnamePath <- paste0(dirPolygon, "/", getPolyFname(ctryCode))

  return (polyFnamePath)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getPolyFnameZip <- function(ctryCode)
{
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(getPolyFnamePath(ctryCode),".zip")
  
  return (polyFname)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNlYearMonthTilesOLS <- function(nlYearMonth, tileList)
{
  success <- TRUE
  
  for (tile in tileList)
  {
    nlYear <- substr(nlYearMonth, 1, 4)
    
    nlMonth <- substr(nlYearMonth, 5, 6)
    
    nlTile <- 
    
    #download tile
    success <- success && getNtLtsOls(nlYear, nlMonth, nlTile)
  }
  
  return (success)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getNlYearMonthTilesVIIRS <- function(nlYearMonth, tileList)
{
  success <- TRUE
  
  #registerDoParallel(cores=2)

  #foreach(tile = iter(tileList)) %dopar%(
  
  #ensure we have all required tiles
  for (tile in tileList)
  { 
    nlYear <- substr(nlYearMonth, 1, 4)
    
    nlMonth <- substr(nlYearMonth, 5, 6)
    
    nlTile <- tileName2Idx(tile)
    
    print(paste0(nlYear, nlMonth, nlTile))
    
    #download tile
    success <- success && getNtLtsViirs(nlYear, nlMonth, nlTile)
  }
  
  return (success)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getAllNlYearMonthsTiles <- function(nlYearMonths, tileList)
{
  for (nlYearMonth in nlYearMonths)
  {
    getNlYearMonthTilesVIIRS(nlYearMonth, tileList)
  }
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
existsCtryCodeTiles <- function()
{
  return (exists("ctryCodeTiles") && class(ctryCodeTiles)=="data.frame" && !is.null(ctryCodeTiles))
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
getCtryCodeTileList <- function(ctryCodes, omitCountries="none")
{
  ctryTiles <- unlist(mapCtryPolyToTiles(ctryCodes, omitCountries)$tiles)
  
  return (ctryTiles)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
existsCtryNlDataVIIRS <- function(ctryCode, nlYearMonth)
{
  if (!existsCtryNlDataFile(ctryCode))
    return (FALSE)
  
  dt <- read.csv(getCtryNlDataFnamePath(ctryCode), nrow=1, header=TRUE)
  
  hd <- names(dt)
  
  if (length(grep(paste0("VIIRS_", nlYearMonth), hd)) > 0)
    return(TRUE)
  else
    return(FALSE)
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
processNtLts <- function (ctryCodes=getAllNlCtryCodes("all"), nlYearMonths=getAllNlYears(), nlType="VIIRS")
{
  #nlYearMonths is a character vector with each entry containing an entry of the form YYYYMM (%Y%m)
  #e.g. 201401 representing the month for which nightlights should be calculated
  #use provided list
  #if none given default to all year_months
  #TODO:
  #1. if years only given, infer months
  #2.verification & deduplication
  
  if (!existsNlTiles())
    nlTiles <- getNlTiles()
  
  if (is.null(nlYearMonths))
  {
    nlYears <- getAllNlYears(nlType)
  }

  #use supplied list of ctryCodes in ISO3 format else use default of all
  #TODO: 
  #1.accept other formats and convert as necessary
  #2.verification & deduplication
  if (is.null(ctryCodes))
  {
    #get list of all country codes
    ctryCodes <- getAllNlCtryCodes(omit = "all")
  }
  
  ##First step: Determine which tiles are required for processing. This is determined by the 
  #list of ctryCodes. Since theoretically we need the polygons of all countries to determine which
  #tiles to download we take the opportunity to download all required shapefiles
  #Practically, we can stop as soon as all 6 tiles are flagged for download.
  
  ##If any tiles cannot be found/downloaded then abort and try for next country
  #we probably need to flag failed downloads so we don't try to process them and report back to user
  
  for (ctryCode in ctryCodes)
  {
    dnldCtryPoly(ctryCode)
  }
  
  #for all nlYearMonths check if the tiles exist else download
  for (nlYearMonth in nlYearMonths)
  {
    message("Checking tiles required for ", nlYearMonth)
    
    nlType <- getNlType(substr(nlYearMonth,1,4))

    #init the list of tiles to be downloaded
    tileList <- NULL
    
    #determine tiles to download
    if (nlType == "VIIRS")
    {
      ctryTiles <- NULL
      
      tileList <- NULL
      
      #For each country
      for (ctryCode in unique(ctryCodes))
      {
        if (existsCtryNlDataVIIRS(ctryCode, nlYearMonth))
        {
          message ("Data exists for", ctryCode)
          
          next()
        }
        
        message("Adding tiles for ", ctryCode)
        
        ctryTiles <- getCtryCodeTileList(ctryCode)
        
        tileList <- c(tileList, setdiff(ctryTiles, tileList))
        
        if (length(tileList) == nrow(nlTiles))
        {
          message ("All tiles have been listed. No need to check other country tiles")
          
          break
        }
      }
    
      if (length(tileList) == 0)
      {
        message("No tiles needed for ", nlYearMonth, ". Process next nlYearMonth")
        
        next
      }

      if (!getNlYearMonthTilesVIIRS(nlYearMonth, tileList))
      {
        print("Something went wrong with the tile downloads. Aborting ...")
        
        break
      }
      
      #for all required countries
      for (ctryCode in unique(ctryCodes))
      {
        processNLCountryVIIRS(ctryCode, nlYearMonth, cropMaskMethod = cropMaskMethod)
      }
      
      for (tile in tileList)
      {
        nlYear <- substr(nlYearMonth, 1, 4)
        nlMonth <- substr(nlYearMonth, 5, 6)
        
        #del the tif file
        file.remove(getNtLtsTifLclNameVIIRS(nlYear, nlMonth, tileName2Idx(tile)))
        
        #del the zip file
        file.remove(getNtLtsZipLclNameVIIRS(nlYear, nlMonth, tileName2Idx(tile)))
      }
    }
    else if (nlType == "OLS")
    {
      if (!getNlYearMonthTilesOLS(nlYearMonth))
      {
        print("Something went wrong with the tile downloads. Aborting ...")
        
        break
      }
      
      #for all required countries
      for (ctryCode in unique(ctryCodes))
      {
        processNLCountryOls(ctryCode, nlYearMonth)
      }
      
    }
  }
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
initNtLts <- function(omitCountries="none")
{
  #the constructor
  
  #set directory paths (tiles, ctrypoly, output/cropped rasters, downloads/temp?)
  
  #create directories
  if(!dir.exists(dirPolygon))
    dir.create(dirPolygon)
  
  if(!dir.exists(dirRasterOLS))
    dir.create(dirRasterOLS)
  
  if(!dir.exists(dirRasterVIIRS))
    dir.create(dirRasterVIIRS)
  
  if(!dir.exists(dirNlData))
    dir.create(dirNlData)
  
  if(!dir.exists(dirRasterOutput))
    dir.create(dirRasterOutput)

  if(!dir.exists(dirRasterWeb))
    dir.create(dirRasterWeb)
    
  if(!dir.exists(dirZonals))
    dir.create(dirZonals)
  
  nlTiles <<- getNlTiles()
  
  tSpPolysDFs <<- createNlTilesSpPolysDF()
  
  #
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
cleanup <- function()
{
  #the destructor
  
  #del temp files
  
  #ensure files have been written that need to
  
  #
}

library(raster)

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
myZonal <- function (x, z, stat, digits = 0, na.rm = TRUE, ...) 
{ 
  #http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  
  fun <- match.fun(stat)
  
  vals <- NULL

  zones <- NULL
  
  blocks <- blockSize(x)
  
  result <- NULL
  
  for (i in 1:blocks$n)
  {
    message("Block: ", i)
    
    message("Reading X")
    vals <- getValues(x, blocks$row[i], blocks$nrows[i])

    vals[vals < 0] <- NA
    
    message("Reading Zones")
    zones <- round(getValues(z, blocks$row[i], blocks$nrows[i]), digits = digits)
    
    rDT <- data.table(vals, z=zones)
    
    #setkey(rDT, z)
    
    message("Calculating partial ", stat)
    result <- rbind(result, rDT[, lapply(.SD, fun, na.rm = TRUE), by=z])
  }
  
  result <- result[, lapply(.SD, fun, na.rm = TRUE), by=z]
  
  gc()
  
  return(result)
} 

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
ZonalPipe <- function (ctryCode, ctryPoly, path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stat)
{
  #http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  #path.in.shp: Shapefile with zone (INPUT)
  #path.in.r: Raster from which the stats have to be computed (INPUT)
  #path.out.r: Path of path.in.shp converted in raster (intermediate OUTPUT)
  #path.out.shp: Path of path.in.shp with stat value (OUTPUT)
  #zone.attribute: Attribute name of path.in.shp corresponding to the zones (ID, Country...)
  #stat: function to summary path.in.r values ("mean", "sum"...)
  
  # 1/ Rasterize using GDAL

  #Initiate parameter
  r<-stack(path.in.r)
  
  if (!file.exists(path.out.r))
  {
    message("Zonal file ", path.out.r, " doesn't exist. Creating")

    ext<-extent(r)
    ext<-paste(ext[1], ext[3], ext[2], ext[4])
    
    res<-paste(res(r)[1], res(r)[2])
    
    lowestLyrName <- getCtryShpLowestLyrName(ctryCode)
    lowestIDCol <- paste0("ID_", gsub("[^[:digit:]]", "", lowestLyrName))
    
    #Gdal_rasterize
    message("Creating zonal raster")
    command<-'gdal_rasterize'
    command<-paste(command, "--config GDAL_CACHEMAX 2000") #Speed-up with more cache (avice: max 1/3 of your total RAM)
    command<-paste(command, "-l", lowestLyrName)
    #command<-paste(command, "-where", paste0(lowestIDCol, "=", i))
    command<-paste(command, "-a", zone.attribute) #Identifies an attribute field on the features to be used for a burn in value. The value will be burned into all output bands.
    command<-paste(command, "-te", as.character(ext)) #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed in georeferenced units. If not specified, the extent of the output file will be the extent of the vector layers.
    command<-paste(command, "-tr", res) #(GDAL >= 1.8.0) set target resolution. The values must be expressed in georeferenced units. Both must be positive values.
    #command<-paste(command, "-a_nodata", 0)
    command<-paste(command, path.in.shp)
    command<-paste(command, "temprast.tif")
    
    system(command)
    
    message("Compressing zonal raster")
    gdal_translate(co = "compress=LZW", src_dataset = "temprast.tif", dst_dataset = path.out.r)
    
    file.remove("temprast.tif")
  }
  
  message("Zonal file ", path.out.r, " found")
  
  # 2/ Zonal Stat using myZonal function
  zone<-raster(path.out.r)
  
  message("Calculating zonal stats ...")
  Zstat<-data.frame(myZonal(r, zone, stat))
  
  message("Calculating zonal stats ... DONE")
  
  colnames(Zstat)[2:length(Zstat)]<-paste0("B", c(1:(length(Zstat)-1)), "_",stat)
  
  return(Zstat)
  
  # 3/ Merge data in the shapefile and write it
  #shp<-readOGR(path.in.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)))
  
  #shp@data <- data.frame(shp@data, Zstat[match(shp@data[,zone.attribute], Zstat[, "z"]),])
  
  #writeOGR(shp, path.out.shp, layer= sub("^([^.]*).*", "\\1", basename(path.in.shp)), driver="ESRI Shapefile")
}

#' Check if a month number is valid for a given nightlight type
#'
#' Check if a month number is valid for a given nightlight type. Note month num is only valid for 
#' "VIIRS" nightlight type 
#'
#' @param monthNum the month in "MM" format e.g. Jan="01", Feb="02"
#' 
#' @param nlType type of nightlight either "VIIRS" or "OLS"
#' 
#' @return TRUE/FALSE
#'
#' @examples
#' validNlMonthNum("01","VIIRS")
#'  returns TRUE
#'
#' validNlMonthNum("13","VIIRS")
#'  returns FALSE
#'  
#' validNlMonthNum("01","OLS")
#'  returns FALSE
#'  
#' @export
fnSumAvgRadGdal <- function(ctryCode, ctryPoly, nlYearMonth)
{
  #http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  path.in.shp<- getPolyFnamePath(ctryCode)
  path.in.r<- getCtryRasterOutputFname(ctryCode, nlYearMonth) #or path.in.r<-list.files("/home/, pattern=".tif$")
  path.out.r<- paste0(dirZonals, "/", ctryCode, "_zone.tif")
  
  path.out.shp<-"zone_withZstat.shp"
  
  zone.attribute<-paste0("ID_", gsub("[^[:digit:]]", "", getCtryShpLowestLyrName(ctryCode)))
  
  lowestLyrName <- getCtryShpLowestLyrName(ctryCode)
  
  lowestIDCol <- paste0("ID_", gsub("[^[:digit:]]", "", lowestLyrName))
 
  #ctryPoly <- readOGR(getPolyFnamePath(ctryCode), lowestLyrName)
  
  sumAvgRad <-ZonalPipe(ctryCode, ctryPoly, path.in.shp, path.in.r, path.out.r, path.out.shp, zone.attribute, stat="sum")
  
  ctryPolyData <- ctryPoly@data
  
  ctryPolyData[,lowestIDCol] <- as.integer(ctryPolyData[,lowestIDCol])
  
  ctryPolyData <- ctryPolyData[order(ctryPolyData[,lowestIDCol]),]
  
  #if there is only the country adm level i.e. no lower adm levels than the country adm level then we only have 1 row each but IDs may not match as seen with ATA. treat differently
  #since we do not have IDs to merge by, we simply cbind the columns and return col2
  if (lowestIDCol == "ID_0")
  {
    sumAvgRad <- cbind(ctryPolyData$ID_0, sumAvgRad[sumAvgRad$z!=0,"B1_sum"])
    
    sumAvgRad <- sumAvgRad[,2]
  }
  else
  {
    sumAvgRad <- merge(ctryPolyData, sumAvgRad, by.x=lowestIDCol, by.y="z", all.x=T, sort=T)

    sumAvgRad <- sumAvgRad$B1_sum
  }
  
  return(sumAvgRad)
}

#' Calculate the sum of the radiance of the pixels in a nightlight raster that fall within a polygon
#'
#' Calculate the sum of the radiance of the pixels in a nightlight raster that fall within a polygon and its
#'     subpolygons. Given a country polygon with subpolygons representing lower admin levels, it will 
#'     crop and mask the raster to each subpolygon and calculate the total radiance for the polygon and 
#'     return a vector of total radiances that matches the subpolygons
#'
#' @param ctryPoly The polygon of the admin level/region of interest. In general is a country polygon
#'     with sub-regions usually the lowest known admin level as given by the GADM polygons.
#' 
#' @param ctryRastCropped The raster containing nightlight radiances to sum. Usually will have already be
#'     cropped to the country outline
#' 
#' @return Integer Sum of radiances of all pixels in a raster that fall within a polygon region
#'
#' @examples
#' ctryPoly <- readOGR(getPolyFnamePath("KEN"), getCtryShpLowestLyrName("KEN")) #read the Kenya polygon downloaded from GADM
#'     and load the lowest admin level (ward)
#' ctryRastCropped <- getCtryRasterOutputFname("KEN","201401") # the VIIRS nightlight raster cropped 
#'     earlier to the country outline
#' sumAvgRadRast <- fnSumAvgRadRast(ctryPoly, ctryRastCropped) #calculate the sum of radiances for the wards
#'     in Kenya
#'  
#' @export
fnSumAvgRadRast <- function(ctryPoly, ctryRastCropped)
{
  registerDoParallel(cores=detectCores()-2)
  
  sumAvgRad <- foreach(i=1:nrow(ctryPoly@data), .combine=rbind) %dopar% 
  {
    message("Extracting data from polygon " , i, " ", base::date())

    dat <- masq_viirs(ctryPoly, ctryRastCropped, i)
    
    message("Calculating the NL sum of polygon ", i, " ", base::date())
    
    #calculate and return the mean of all the pixels
    data.frame(sum = sum(dat, na.rm=TRUE))
  }
  
  removeTmpFiles(h=0)
  
  gc()
  
  return(sumAvgRad)
}

getRastPercentiles <- function(rastFilename)
{
  rastInfo <- gdalinfo(rastFilename, hist=T)
  rastMinMax <- unlist(strsplit(gsub("[^[:digit:].-]", " ", rastInfo[30]), " "))
  rastMinMax <- as.numeric(rastMinMax[which(rastMinMax!="")])
  
  rastSeq <- seq(rastMinMax[2],rastMinMax[3],length.out = rastMinMax[1])
  
  rastHist <- as.numeric(unlist(strsplit(rastInfo[31]," ")))
  rastHist <- rastHist[!is.na(rastHist)]
  
  rastCumSum <- cumsum(rastHist)
  rastCumProbs <- rastCumSum/last(rastCumSum)
  
  q2 <- last(rastSeq[rastCumProbs<0.2])
  q3 <- first(rastSeq[rastCumProbs>0.2])  
  hist1 <- rastHist[]
  
  q98 <- last(rastSeq[rastCumProbs<0.98])
  q99 <- first(rastSeq[rastCumProbs>0.98])
  
  return (c(q2,q98))
}

writeNightlightsMap <- function()
{
  tplHead <- "
MAP
  IMAGETYPE  PNG
  EXTENT	-180 -90 180 90  
  SIZE		800 600
  SHAPEPATH      /var/www/cgi-bin
  IMAGECOLOR     \"#ffffffff\"
  TRANSPARENT 	TRUE
  SHAPEPATH	/btrfs/shiny_nightlights/outputrasters
  
  PROJECTION
    \"init=epsg:4326\"
  END
  
  OUTPUTFORMAT
    NAME GEOTIFF
    DRIVER GDAL/GTiff
    MIMETYPE image/tiff
    IMAGEMODE RGB
    EXTENSION tif
  END
  
  WEB
    METADATA
      \"wms_title\" \"Nightlight Rasters\"
      \"wms_onlineresource\" \"http://localhost/cgi-bin/mapserv?map=nightlights_wms.map\"
      \"wms_description\" \"nightlights\"
      \"wms_name\" \"Nightlights\"
      \"wms_label\" \"Nightlights\"
      \"wms_srs\" \"EPSG:3857\"
      \"wms_extent\" \"-180 -90 180 90\"
      \"wms_formats\" \"GEOTIFF\"
      \"wms_enable_request\" \"*\"
    END
  END"
  
  tplLayer <- "
  LAYER
    NAME	<VAL_NAME>
    
    METADATA
      \"wms_title\"		\"<VAL_NAME>\"
      \"wms_enable_request\"	\"*\"
      \"wms_srs\"			\"EPSG:4326\"
      \"wms_extent\"		\"<VAL_EXTENT>\"
      \"wms_include_items\"	\"all\"
      \"wms_dataurl_format\"	\"text/html\"
    END
    
    EXTENT	<VAL_EXTENT>
    DATA	\"<VAL_NAME>.tif\"
    #    TILEINDEX	/btrfs/shiny_nightlights/outputrasters/nightlights_201204.tif
    #    TILEITEM	\"location\"
    STATUS	OFF
    TYPE	RASTER
    
    DUMP TRUE
    PROJECTION
      \"init=epsg:4326\"
    END
    
    CLASSITEM \"[pixel]\"
    
    CLASS
      NAME \"NODATA\"
      EXPRESSION ([pixel] = <VAL_NODATA>)
      
      STYLE
      OPACITY 0
      END
    END
    
    CLASS
      NAME DEC0
      EXPRESSION ([pixel] > <VAL_NODATA> AND [pixel] < <VAL_DEC0>)
      
      STYLE
      OPACITY 100
      COLOR \"#000000\"
      END
    END
    
    CLASS
      NAME DEC1
      EXPRESSION ([pixel] >= <VAL_DEC0> AND [pixel] < <VAL_DEC1> )
      
      STYLE
      OPACITY 100
      COLOR \"#5A5A5A\"
      END
    END
    
    CLASS
      NAME DEC2
      EXPRESSION ([pixel] >= <VAL_DEC1>  AND [pixel] < <VAL_DEC2>  )
      
      STYLE
      OPACITY 100
      COLOR \"#7B7B7B\"
      END
    END
    
    CLASS
      NAME DEC3
      EXPRESSION ([pixel] >= <VAL_DEC2> AND [pixel] < <VAL_DEC3> )
      
      STYLE
      OPACITY 100
      COLOR \"#949494\"
      END
    END
    
    CLASS
      NAME DEC4
      EXPRESSION ([pixel] >= <VAL_DEC3> AND [pixel] < <VAL_DEC4> )
      
      STYLE
      OPACITY 100
      COLOR \"#A8A8A8\"
      END
    END
    
    CLASS
      NAME DEC5
      EXPRESSION ([pixel] >= <VAL_DEC4> AND [pixel] < <VAL_DEC5> )
      
      STYLE
      OPACITY 100
      COLOR \"#BABABA\"
      END
    END
    
    CLASS
      NAME DEC6
      EXPRESSION ([pixel] >= <VAL_DEC5> AND [pixel] < <VAL_DEC6> )
      
      STYLE
      OPACITY 100
      COLOR \"#CACACA\"
      END
    END
    
    CLASS
      NAME DEC7
      EXPRESSION ([pixel] >= <VAL_DEC6> AND [pixel] < <VAL_DEC7> )
      
      STYLE
      OPACITY 100
      COLOR \"#D9D9D9\"
      END
    END
    
    CLASS
      NAME DEC8
      EXPRESSION ([pixel] >= <VAL_DEC7> AND [pixel] < <VAL_DEC8>)
      
      STYLE
      OPACITY 100
      COLOR \"#E6E6E6\"
      END
    END
    
    CLASS
      NAME DEC9
      EXPRESSION ([pixel] >= <VAL_DEC9> AND [pixel] < <VAL_DEC10> )
      
      STYLE
      OPACITY 100
      COLOR \"#F3F3F3\"
      END
    END
    
    CLASS
      NAME DEC10
      EXPRESSION ([pixel] > <VAL_DEC10>)
      
      STYLE
      OPACITY 100
      COLOR \"#FFFFFF\"
      END
    END
  END # MODIS raster layer ends here"
  
  fList <- dir(path = dirRasterOutput, pattern = "*.tif$",full.names = T)
  
  layers <- NULL
  nodata <- "-1.69999999999999994e+308"

  tplMap <- tplHead  
  tplLayers <- ""

  for (f in fList)
  {
    tplLyr <- tplLayer
    
    message(f)
    rast <- raster(f)
    e <- extent(rast)
    ext <- paste0(e@xmin, " ", e@ymin, " ", e@xmax, " ", e@ymax )
    qs <- myquantile(rast)
    q2 <- qs[1]
    q98 <- qs[2]
    
    if (is.na(q2))
      q2 <- 0
    
    if (is.na(q98))
      q98 <- 0
    
    deciles <- seq(q2, q98, length.out = 11)
    
    fname <- unlist(stringr::str_split(f, "/"))
    fname <- last(fname)
    lyrName <-substr(fname,1,10)
    
    tplLyr <- stringr::str_replace_all(tplLyr,  "<VAL_NAME>", lyrName)
    tplLyr <- stringr::str_replace_all(tplLyr,  "<VAL_NODATA>", nodata)
    tplLyr <- stringr::str_replace_all(tplLyr,  "<VAL_EXTENT>", ext)
    
    for (i in 1:11)
    {
      decval <- paste0("<VAL_DEC", i-1,">")
      tplLyr <- stringr::str_replace_all(tplLyr,  decval, deciles[i])
    }
    
    tplLayers <- paste0(tplLayers, "\n", tplLyr)
  }
  
  tplMap <- paste0(tplMap, "\n", tplLayers, "\nEND #END MAP")
  
  write_file(tplMap, "test.map")
}

myquantile <- function (x) 
{ 
  #http://www.guru-gis.net/efficient-zonal-statistics-using-r-and-gdal/
  vals <- NULL
  
  zones <- NULL
  
  blocks <- blockSize(x)
  
  result <- NULL
  
  for (i in 1:blocks$n)
  {
    vals <- getValues(x, blocks$row[i], blocks$nrows[i])
    
    result <- rbind(result, quantile(vals, c(0.02,0.98),na.rm=T))
  }
  
  result <- colMeans(result, na.rm = T)
  
  gc()
  
  return(result)
} 