require(readr)
require(dplyr)

require(lubridate)

require(rgdal)
require(raster)

require(sp)
require(rgeos)
require(rworldmap)
require(cleangeo)

ntLtsIndexUrlViirs <- "https://www.ngdc.noaa.gov/eog/viirs/download_monthly.html"

#6 nightlight tiles named by top-left geo coordinate numbered from left-right & top-bottom
#creates columns as strings. createSpPolysDF converts relevant columns to numeric
nlTiles <- as.data.frame(cbind(id=c(1,2,3,4,5,6), name=c("75N180W","75N060W","75N060E","00N180W","00N060W","00N060E"), minx=c(-180, -60, 60, -180, -60, 60), maxx=c(-60, 60, 180, -60, 60, 180), miny=c(0, 0, 0, -75, -75, -75), maxy=c(75, 75, 75, 0, 0, 0)), stringsAsFactors=F)

#projection system to use
#can we use only one or does it depend on the shapefile loaded?
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

map <- rworldmap::getMap()
map <- clgeo_Clean(map)

#Set raster directory path
dirRasterOLS <- "./tiles"

#Set directory path
dirRasterVIIRS <- "./tiles"

dirPolygon <- "./polygons"

dirNlData <- "./data"

shpTopLyrName <- "adm0"

createNlTilesSpPolysDF <- function()
{
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

plotCtryWithTiles <- function(idx)
{
  map <- rworldmap::getMap()
  map <- clgeo_Clean(map)
 
  if (is.numeric(idx))
  {
    if(idx < 0 || idx > length(map@polygons))
    {
      return("Index out of range")
    }
  }
  else
  {
    ctryISO3 <- rwmGetISO3(idx)
    
    #print(ctryISO3)

    if (is.na(ctryISO3) || ctryISO3 == "")
      return("Unknown country")
    
    idx <- which(as.character(map@data$ISO3) == ctryISO3)
  }

  #print (idx)
   
  ctryPolys <- map@polygons[[idx]]
  
  #create a SpatialPolygons object with a list of 1 list of Polygons
  ctrySpPolys <- SpatialPolygons(Srl = list(ctryPolys))
  
  crs(ctrySpPolys) <- CRS(wgs84)
  
  ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")
  
  plot(tSpPolysDFs)
  plot(ctrySpPolysDF, add=TRUE)
  
  #ggplot(tSpPolysDFs, aes(x=long,y=lat))+geom_polygon(col="black", fill="white", alpha=0.5)#+geom_polygon(data=ctrySpPolysDF, alpha=0.5)
  #ggplot(ctrySpPolysDF, aes(x=long,y=lat, group=group))+geom_polygon(col="black", fill="white",alpha=0.5)
  
  #a <- spplot(tSpPolysDFs, main=map@polygons[[idx]]@ID)
  #b <- spplot(ctrySpPolysDF)
  
  #a+as.layer(b)
  
}

mapAllCtryPolyToTiles <- function()
{
  #get list of all country codes
  ctryCodes <- getAllNlCtryCodes()
  
  map <- rworldmap::getMap()
  map <- clgeo_Clean(map)
  
  ctryCodeTiles <- NULL
  
  for (i in 1:length(ctryCodes))
  {
    ctryPolys <- map@polygons[[i]]
    
    #create a SpatialPolygons object with a list of 1 list of Polygons
    ctrySpPolys <- SpatialPolygons(Srl = list(ctryPolys))
    
    crs(ctrySpPolys) <- CRS(wgs84)
    
    ctrySpPolysDF <- as(ctrySpPolys, "SpatialPolygonsDataFrame")
    
    ctryCodeTiles <- rbind(ctryCodeTiles, list(tilesPolygonIntersect(ctrySpPolys)))
  }

  ctryCodeTiles <- as.data.frame(cbind(code = as.character(map@data$ISO3), tiles = ctryCodeTiles))
  
  names(ctryCodeTiles) <- c("code", "tiles")
  
  ctryCodeTiles$code <- as.character(ctryCodeTiles$code)
  
  #plot(tSpPolysDFs, add=TRUE)
  #plot(ctrySpPolysDF, add=TRUE)
  #
    
  return(ctryCodeTiles)
}

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
  
  ctrySpPolysDF <<- as(ctrySpPolys, "SpatialPolygonsDataFrame")
  
  ctryCodeTiles <- tilesPolygonIntersect(ctrySpPolys)
  
  #plot(tSpPolysDFs, add=TRUE)
  #plot(ctrySpPolysDF, add=TRUE)
  
  return (ctryCodeTiles)
}

tileName2Idx <- function(tileName)
{
  return (which(nlTiles$name == tileName))
}

tileIdx2Name <- function(tileNum)
{
  return (nlTiles[tileNum, "name"])
}

tilesPolygonIntersect <- function(shp_polygon)
{
  #given a polygon this function returns a list of the names of the viirs tiles
  #that it intersects with
  #Input: a Spatial Polygon e.g. from a loaded shapefile
  #Output: a character vector of tile names as given in the nlTiles dataframe
  
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

getNtLtsUrlViirs <- function(inYear, inMonth, inTile)
{
  inYear <- as.character(inYear)
   
  nMonth <- as.character(inMonth)

  #Function to return the url of the file to download given the year, month, and nlTile index
  #nlTile is a global list
  
  #the page that lists all available nightlight files
  ntLtsPageHtml <- "https://www.ngdc.noaa.gov/eog/viirs/download_mon_mos_iframe.html"
  
  #the local name of the file once downloaded
  ntLtsPageLocalName <- "ntltspage.html"

  #if the file does not exist or is older than a week download it afresh
  #not working. download.file does not seem to update mtime
  if (!file.exists(ntLtsPageLocalName) || (date(now()) - date(file.mtime(ntLtsPageLocalName)) > as.difftime(period("1 day"))))
  {
    download.file(url = ntLtsPageHtml, destfile = ntLtsPageLocalName, method = "wget")
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

  fname <- stringr::str_extract(ntLtsPageUrl, "SVDNB.*.tgz")
  ntLtsPageUrl <- paste0("http://localhost/", fname)
  
  #****DELETE WHEN DONE****
  
  return (ntLtsPageUrl)
}

getNtLtsFileLcllNameVIIRS <- function(nlYear, nlMonth, tileNum)
{
  paste0(dirRasterVIIRS, "/viirs_", nlYear, "_", nlMonth, "_", tileIdx2Name(tileNum), ".tgz")
}

getNtLtsViirs <- function(nlYear, nlMonth, tileNum)
{
  rsltDnld <- NA
  
  ntLtsFileLocalName <- getNtLtsFileLcllNameVIIRS(nlYear, nlMonth, tileNum)
  
  if (!file.exists(ntLtsFileLocalName))
  {
    ntLtsFileUrl <- getNtLtsUrlViirs(nlYear, nlMonth, tileNum)
    
    rsltDnld <- download.file(ntLtsFileUrl, ntLtsFileLocalName, mode = "wb", method = "wget", extra = "-c")
  }
  else
  {
    #if the file is found we can return positive? Probably not unless there's an overwrite option
    #for our purposes return true
    print("File exists, set Overwrite = TRUE to overwrite")
    
    rsltDnld <- 0
  }
  
  return (rsltDnld == 0)
}

masq_viirs <- function(shp, rast, i)
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
  coords <- expand.grid(seq(extent@xmin,extent@xmax,(extent@xmax-extent@xmin)/(ncol(inner)-1)),
                        seq(extent@ymin,extent@ymax,(extent@ymax-extent@ymin)/(nrow(inner)-1)))
  #Convert raster into vector
  data <- as.vector(inner)
  
  ##THIS SECTION NEEDS WORK. confirm error and NA values and handling of the same
  
  #data <- data[!is.na(data)] #keep non-NA values only ... shall this affect mask values?
  #data[is.na(data)] <- 0
  data[data < 0] <- 0 #any negative values are either recording problems or error values as per: 
  
  return(data) 
}

getNtLtsUrlOls <- function(inYear, inMonth, inTile)
{
  
}

getNtLtsOls <- function(inputYear)
{
  download.file("")
}

masq_ols <- function(shp,rast,i)
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

processNLCountryOls <- function(cntryCode, nlYear, nlMonth)
{
  setwd(dirRasterOLS)
  
  #read in the kenya shapefile containing ward administrative boundaries
  ke_shp_ward <- readOGR("/btrfs/nightlights/KEN_adm_shp","KEN_adm3")
  
  #get the extent of the shapefile
  ke_extent <- extent(ke_shp_ward)
  
  ##Specify WGS84 as the projection of the raster file
  wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  projection(ke_shp_ward) <- CRS(wgs84)
  
  ##Obtain a list of TIF files, load in the first file in list
  #get a list of the tgzs available
  tar_fls <- list.files(dirRasterOLS, pattern = "^F.*.tar", ignore.case = T)
  
  tar_fls <- tar_fls[1]
  
  #pick the unique year month from the available files
  all_years_months <- unique(substr(tar_fls,1,7))
  
  #for missing year_months figure out how to process
  #1. either download immediately (can we do this asynchronously?)
  #2. ignore and download at the point of need
  
  if (file.exists("nightlight_means_dmspols.csv"))
  {
    message("Detected existing nightlight data. Reading ...")
    extract <- read.csv("nightlight_means_dmspols.csv",header = TRUE,sep = ",")
    
    existing_data_cols <- names(extract)
    
    existing_year_months <- existing_data_cols[grep("^F[:alphanum:]*", existing_data_cols)]
    
    #existing_year_months <- str_replace(existing_year_months, "F", "")
    
    #find the positions of the existing yearmonths in the filelist
    existing_pos <- sapply(existing_year_months, FUN = function(x) grep(x, all_years_months))
    
    #remove existing yearmonths from the filelist. the remainder will be processed
    all_years_months <- all_years_months[-existing_pos]
    
  } else #if the file is not found, initialize the extract dataframe
  {
    extract <- ke_shp_ward@data[,c("ID_1","NAME_1","ID_2","NAME_2","ID_3","NAME_3")]
    
    areas <- area(ke_shp_ward)
    
    extract <- cbind(extract, areas)
    
    names(extract) <- c("county_id", "county_name", "constituency_id", "constituency_name", "ward_id", "ward_name", "area")
  }
  
  if (length(all_years_months) > 0)
  {
    #get the nightlight tgzs listed in the tars
    for (tar_fl in tar_fls)
    {
      sat_year <- substr(tar_fl,1,7)
      
      if (length(intersect(sat_year, all_years_months)) == 0)
      {
        message(sat_year, " exists. Skipping to next file ...")
        next()
      }
      
      message("Extracting ", tar_fl, " ", date())
      
      message("Getting list of files in ", tar_fl, " ", date())
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
      
      message("Decompressing ", tgz_file, " ", date())
      
      gunzip(tgz_file)
      
      #no need to delete the gz since gunzip deletes the compressed version
      
      message("Reading in the raster " , date())
      rast_global <- raster(tif_file)
      
      projection(rast_global) <- CRS(wgs84)
      
      message("Cropping the raster ", date())
      rast_ke <- crop(rast_global, ke_shp_ward)
      
      message("Releasing the raster variables")
      rm(rast_global)
      
      message("Deleting the raster files ", date())
      
      unlink(tif_file)
      
      gc()
      
      message("Masking the merged raster ", date())
      #rast_ke <- mask(rast_ke, ke_shp_ward)
      rast_ke <- rasterize(ke_shp_ward, rast_ke, mask=TRUE) #crops to polygon edge & converts to raster
      
      message("Writing the merged raster to disk ", date())
      writeRaster(x = rast_ke, filename = paste0("outputrasters/",sat_year,".tif"), overwrite=TRUE)
      
      registerDoParallel(cores=2)
      
      message("Begin extracting the data from the merged raster ", date())
      sum_avg_rad <- foreach(i=1:nrow(ke_shp_ward@data), .combine=rbind) %dopar% {
        
        #message("Extracting data from polygon " , i, " ", date())
        dat <<- masq(ke_shp_ward,rast_ke,i)
        
        #message("Calculating the mean of polygon ", i, " ", date())
        
        #calculate and return the mean of all the pixels
        data.frame(mean = sum(dat, na.rm=TRUE))
      }
      
      #merge the calculated means for the polygon as a new column
      extract <- cbind(extract, sum_avg_rad)
      
      #name the new column with the yearmonth of the data
      names(extract)[ncol(extract)] <- paste0(sat_year)
      message("DONE processing ", sat_year, " ", date())
    }
    
    message("COMPLETE. Writing data to disk")
    write.table(extract, "nightlight_means_dmspols.csv", row.names= F, sep = ",")
    
  } else
  {
    message("No DMSP-OLS nightlights to process")
  }
}

processNLCountriesViirs <- function(ctryCodes, nlYearMonth)
{
  #Download all tiles
  getNtLtsViirs()
  
  for (nlCtryCode in nlCtryCodes)
    processNLCountryViirs(ctryCode, nlYearMonth)
}

ctryShpLyrName2Num <- function(layerName)
{
  return(as.numeric(gsub("[^[:digit:]]", "", layerName)))
}

processNLCountryViirs <- function(ctryCode, nlYearMonth)
{
  ctryPoly <- readOGR(getPolyFnamePath(ctryCode), getCtryShpLowestLyrName(ctryCode))

  ctryExtent <- extent(ctryPoly)

  projection(ctryPoly) <- CRS(wgs84)
  
  ##Obtain a list of TIF files, load in the first file in list
  #get a list of the tgzs available
  #tgzs <- list.files(dirRasterVIIRS, pattern = "^svdnb.*.*vcmcfg.*.tgz$", ignore.case = T)
  
  #pick the unique year month from the available files
  #all_years_months <- unique(substr(tgzs,11,16))
  
  if (existsCtryNlDataFile(ctryCode))
  {
    ctryNlDataDF <- read.csv(getCtryNlDataFnamePath(ctryCode),header = TRUE,sep = ",")
    
    existingDataCols <- names(ctryNlDataDF)
    
    existingYearMonths <- existingDataCols[grep("^NL_[:alphanum:]*", existingDataCols)]
    
    existingYearMonths <- stringr::str_replace(existingYearMonths, "NL_", "")
    
    #existing_pos <- sapply(existing_year_months, FUN = function(x) grep(x, all_years_months))
    
    #all_years_months <- all_years_months[-existing_pos]
  } else
  {
    ctryPolyAdmLevels <- getCtryPolyAdmLevelNames(ctryCode)
    
    ctryPolyAdmLevels$name <- tolower(ctryPolyAdmLevels$name) 
    
    nLyrs <- nrow(ctryPolyAdmLevels)
    
    nums <- c(paste(1:nLyrs,1:nLyrs))
    nums <- unlist(strsplit(paste(nums, collapse = " "), " "))
    
    ctryPolyAdmCols <- paste(c("ID_", "NAME_"), nums, sep="")
    
    ctryNlDataDF <-ctryPoly@data[,eval(ctryPolyAdmCols)]
    
    ctryPolyColNames <- paste(ctryPolyAdmLevels[nums, "name"], c("_id", "_name"), sep="")
    
    names(ctryNlDataDF) <- ctryPolyColNames
    
    #  c("county_id", "county_name", "constituency_id", "constituency_name", "ward_id", "ward_name")
  }
  
  message("Begin processing ", yearmonth, " ", date())
  
  tif_files <- vector()

  message("Extracting ", tgz_fl, " ", date())
  
  message("Getting list of files in ", tgz_fl, " ", date())
  tgz_file_list <- untar(tgz_fl, list = TRUE)
  #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
  
  tgz_avg_rad_filename <- tgz_file_list[grep("svdnb.*.avg_rade9.tif$",tgz_file_list, ignore.case = T)]

  message("Decompressing ", tgz_avg_rad_filename, " ", date())
  untar(tgz_fl, files = tgz_avg_rad_filename)
    
      
      #tifs = list.files(dirRasterVIIRS, pattern = "^svdnb.*.avg_rade9.tif$", ignore.case = T)
      
      message("Reading in the rasters " , date())
      #rast_upper <- raster(paste(dirRasterVIIRS,"/",tifs[1],sep=""))
      rast_upper_filename <- tif_files[grep(pattern = "00N060W", x = tif_files)]
      rast_upper <- raster(rast_upper_filename)
      
      #rast_lower <- raster(paste(dirRasterVIIRS,"/",tifs[2],sep=""))  
      rast_lower_filename <- tif_files[grep(pattern = "75N060W",x = tif_files)]
      rast_lower <- raster(rast_lower_filename)
      
      projection(rast_upper) <- CRS(wgs84)
      projection(rast_lower) <- CRS(wgs84)
      
      message("Cropping the rasters ", date())
      ke_rast_upper <- crop(rast_upper, ke_shp_ward)
      ke_rast_lower <- crop(rast_lower, ke_shp_ward)
      
      message("Releasing the raster variables")
      rm(rast_lower, rast_upper)
      
      message("Deleting the raster files ", date())
      unlink(tif_files)
      
      tif_files <- vector()
      
      gc()
      
      message("Merging the cropped rasters ", date())
      ke_rast_combined <- merge(ke_rast_upper, ke_rast_lower)
      
      message("Masking the merged raster ", date())
      #ke_rast_combined <- mask(ke_rast_combined, ke_shp_ward)
      ke_rast_combined <- rasterize(ke_shp_ward, ke_rast_combined, mask=TRUE) #crops to polygon edge & converts to raster
      
      message("Deleting the two component rasters ", date())
      rm(ke_rast_lower, ke_rast_upper)
      
      message("Writing the merged raster to disk ", date())
      writeRaster(x = ke_rast_combined, filename = paste0("outputrasters/",yearmonth,".tif"), overwrite=TRUE)
      
      registerDoParallel(cores=2)
      
      message("Begin extracting the data from the merged raster ", date())
      sum_avg_rad <- foreach(i=1:nrow(ke_shp_ward@data), .combine=rbind) %dopar% {
        
        message("Extracting data from polygon " , i, " ", date())
        dat <- masq(ke_shp_ward,ke_rast_combined,i)
        
        message("Calculating the mean of polygon ", i, " ", date())
        
        #calculate and return the mean of all the pixels
        data.frame(mean = mean(dat, na.rm=TRUE))
      }
      
      #merge the calculated means for the polygon as a new column
      extract <- cbind(extract, sum_avg_rad)
      
      #name the new column with the yearmonth of the data
      names(extract)[ncol(extract)] <- paste0("NL_", yearmonth)
      message("DONE processing ", yearmonth, " ", date())
    }
    
    message("COMPLETE. Writing data to disk")
    write.table(extract, "nightlight_means_viirs.csv", row.names= F, sep = ",")
  }else
  {
    message("No nightlights to process")
  }
}

getCtryPolyUrl <- function(ctryCode)
{
  #Sample url: http://biogeo.ucdavis.edu/data/gadm2.8/shp/AFG_adm_shp.zip
  basePolyUrl <- "http://biogeo.ucdavis.edu/data/gadm2.8/shp/"
  
  return (paste0(basePolyUrl, ctryCode, "_adm_shp.zip"))
}

getCtryNlDataFname <- function(ctryCode)
{
  return (paste0(ctryCode, "_NLData.csv"))
}

getCtryNlDataFnamePath <- function(ctryCode)
{
  return (paste0(dirNlData, "/", getCtryNlDataFname(ctryCode)))
}

existsCtryNlDataFile <- function(ctryCode)
{
  #for polygons look for shapefile dir
  return(file.exists(getCtryNlDataFnamePath(ctryCode)))
}

polyFnamePathExists <- function(ctryCode)
{
  #for polygons look for shapefile dir
  return(dir.exists(getPolyFnamePath(ctryCode)))
}

polyFnameZipExists <- function(ctryCode)
{
  return(file.exists(getPolyFnameZip(ctryCode)))
}

getCtryShpLyrName <- function(ctryCode, lyrNum)
{
  return(paste0(ctryCode, "_adm", lyrNum))
}

getCtryShpLowestLyrName <- function(ctryCode)
{
  layers <- ogrListLayers(getPolyFnamePath(ctryCode))
  
  admLayers <- layers[grep("adm", layers)]
  
  admLayerNums <- gsub("[^[:digit:]]", "", admLayers)
  
  lowestAdmLyrName <- admLayers[order(as.numeric(admLayerNums),decreasing = T)][1]
  
  return(lowestAdmLyrName)
}

getCtryPolyAdmLevelNames <- function(ctryCode)
{
  lowestLayer <- getCtryShpLowestLyrName(ctryCode)
  
  numLayers <- ctryShpLyrName2Num(lowestLayer)
  
  admLevels <- NULL
  
  for (lyrNum in 1:numLayers)
  {
    lyrPoly <- readOGR(getPolyFnamePath(ctryCode), getCtryShpLyrName(ctryCode, lyrNum))
    
    lvlTypeName <- paste0("TYPE_",lyrNum)
    
    lvlName <- unlist(lyrPoly@data[1,eval(lvlTypeName)])
    
    admLevels <- rbind(admLevels, as.character(lvlName))
  }
  
  admLevels <- as.data.frame(cbind(1:numLayers, admLevels))
  
  names(admLevels) <- c("id", "name")
  
  return (admLevels)  
}

dnldCtryPoly <- function(ctryCode)
{
  fullPolyUrl <- getCtryPolyUrl(ctryCode)
  
  #if the path doesn't exist
  if (!polyFnamePathExists(ctryCode))
  {
    if (!polyFnameZipExists(ctryCode))
    {
      if(download.file(url = getCtryPolyUrl(ctryCode), destfile = getPolyFnameZip(ctryCode), method = "wget", mode = "wb", extra = "-c") == 0)
      {
        result <- unzip(getPolyFnameZip(ctryCode), exdir = getPolyFnamePath())
      }
    }else
    {
      result <- unzip(getPolyFnameZip(ctryCode), exdir = getPolyFnamePath(ctryCode))
    }
  }
  
  return (!is.null(result))
}

getAllNlYears <- function()
{
  return (paste(1992:year(now()), c(paste("0",1:9, sep= ""),10:12), sep=""))
}

getAllNlCtryCodes <- function()
{
  #rworldmap has more country codes in countryRegions$ISO3 than in the map itself
  #select ctryCodes from the map data itself
  map <- rworldmap::getMap()

  #some polygons have problems. use cleangeo package to rectify
  map <- clgeo_Clean(map)
  
  return (as.character(map@data$ISO3))
}

getNlType <- function(nlYear)
{
  if (nlYear < 1992 || nlYear > year(now()))
    return(NA)

  if (nlYear > 1992 && nlYear < 2014)
    return("OLS")
  else
    return("VIIRS")
}

getPolyFname <- function(ctryCode)
{
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(ctryCode, "_adm_shp")
  
  return (polyFname)
}

getPolyFnamePath <- function(ctryCode)
{
  #check for the shapefile directory created with 
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFnamePath <- paste0(dirPolygon, "/", getPolyFname(ctryCode))

  return (polyFnamePath)
}

getPolyFnameZip <- function(ctryCode)
{
  #format of shapefiles is CTR_adm_shp e.g. KEN_adm_shp
  polyFname <- paste0(getPolyFnamePath(ctryCode),".zip")
  
  return (polyFname)
}

getNlYearMonthTiles <- function(nlYearMonth, tileList)
{
  success <- TRUE
  
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

getAllNlYearMonthsTiles <- function(nlYearMonths, tileList)
{
  for (nlYearMonth in nlYearMonths)
  {
    getNlYearMonthTiles(nlYearMonth, tileList)
  }
}

processNtLts <- function (ctryCodes, nlYearMonths)
{
  #nlYearMonths is a character vector with each entry containing an entry of the form YYYYMM (%Y%m)
  #e.g. 201401 representing the month for which nightlights should be calculated
  #use provided list
  #if none given default to all year_months
  #TODO:
  #1. if years only given, infer months
  #2.verification & deduplication
  
  if (length(nlYearMonths) == 0)
  {
    nlYears <- getAllNlYears()
  }

  #use supplied list of ctryCodes in ISO3 format else use default of all
  #TODO: 
  #1.accept other formats and convert as necessary
  #2.verification & deduplication
  if (length(ctryCodes) == 0)
  {
    #get list of all country codes
    ctryCodes <- getAllNlCtryCodes()
  }
  
  ##First step: Determine which tiles are required for processing. This is determined by the 
  #list of ctryCodes. Since theoretically we need the polygons of all countries to determine which
  #tiles to download we take the opportunity to download all required shapefiles
  #Practically, we can stop as soon as all 6 tiles are flagged for download.
  
  ##If any tiles cannot be found/downloaded then abort and try for next country
  #we probably need to flag failed downloads so we don't try to process them and report back to user
  
  #init the list of tiles to be downloaded
  tileList <- NULL
  
  #For each country
  for (ctryCode in unique(ctryCodes))
  {
    ctryTiles <- unlist(ctryCodeTiles[which(ctryCodeTiles$code == ctryCode), "tiles"])
    
    tileList <- c(tileList, setdiff(ctryTiles, tileList))
  }
  
  #determine tiles to download
  
  #for all nlYearMonths check if the tiles exist else download
  for (nlYearMonth in nlYearMonths)
  {
    nlType <- getNlType(substr(nlYearMonth,1,4))

    if (nlType == "VIIRS")
    {
      if (!getNlYearMonthTiles(nlYearMonth, tileList))
      {
        print("Something went wrong with the tile downloads. Aborting ...")
        
        break
      }
      
      #for all required countries
      for (ctryCode in unique(ctryCodes))
      {
        processNLCountryViirs(ctryCode)
      }
    }
    else if (nlType == "OLS")
    {
      #for all required countries
      for (ctryCode in unique(ctryCodes))
      {
        processNLCountryOls(ctryCode)
      }
      
    }
  }
}

initNtLts <- function()
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
  
  tSpPolysDFs <<- createNlTilesSpPolysDF()
  
  ctryCodeTiles <<- mapAllCtryPolyToTiles()
  #
}

cleanup <- function()
{
  #the destructor
  
  #del temp files
  
  #ensure files have been written that need to
  
  #
}