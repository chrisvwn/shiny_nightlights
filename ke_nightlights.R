library(doParallel) #Allows for parallel processing using multiple cores
library(foreach) #Enables for-each statements to be used for parallel processing
library(raster) # interface to raster (e.g. TIFF) data
library(sp) #interface to spatial data processing
library(rgdal) #interface for manipulating spatial data

#library(lubridate)
library(reshape2)
library(stringr)
library(lattice)

#ref: https://commercedataservice.github.io/tutorial_viirs_part1/ #code to process nightlights
#http://www.gadm.org - for administrative boundary vector maps

#Ultimately to be converted to a library/service that will allow users to interact with nightligt data from #both snpp/viirs and dmsp/ols without having to download and reinvent the wheel

# Supply a country code/polygon/multipolygon and optionally a year/month range and the program will:
# 1. download all the appropriate nightlights tiles : TODO
# 2. crop and mask the tiles to the polygon outer area
# 3. alternatively crop the tiles to the multipolygon subpolygons
# 4. calculate the radiances per subpolygon
# 5. return matrix/dataframe

masq <- function(shp,rast,i)
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

#Set directory path
#raster_dir = "/btrfs/nightlights/"
raster_dir = "/media/NewVolume/Biz/Ishara/Projects/shiny_nightlights/tiles/"

#change directory to the path
setwd(raster_dir)

ke_shp_ward <- readOGR("KEN_adm_shp","KEN_adm3")

ke_extent <- extent(ke_shp_ward)

##Specify WGS84 as the projection of the raster file
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

projection(ke_shp_ward) <- CRS(wgs84)

##Obtain a list of TIF files, load in the first file in list
#get a list of the tgzs available
tgzs <- list.files(raster_dir, pattern = "^svdnb.*.*vcmcfg.*.tgz$", ignore.case = T)

#pick the unique year month from the available files
all_years_months <- unique(substr(tgzs,11,16))

if (file.exists("nightlight_means.csv"))
{
  extract <- read.csv("nightlight_means.csv",header = TRUE,sep = ",")
  
  existing_data_cols <- names(extract)
  
  existing_year_months <- existing_data_cols[grep("^NL_[:alphanum:]*", existing_data_cols)]
  
  existing_year_months <- stringr::str_replace(existing_year_months, "NL_", "")
  
  existing_pos <- sapply(existing_year_months, FUN = function(x) grep(x, all_years_months))
  
  all_years_months <- all_years_months[-existing_pos]
} else
{
  extract <- ke_shp_ward@data[,c("ID_1","NAME_1","ID_2","NAME_2","ID_3","NAME_3")]
  
  names(extract) <- c("county_id", "county_name", "constituency_id", "constituency_name", "ward_id", "ward_name")
}

if (length(all_years_months)>0)
{
  for (yearmonth in all_years_months)
  {
    message("Begin processing ", yearmonth, " ", date())
    
    #get the tgzs that were collected in the period
    year_month_tiles_idx <- grep(paste0("SVDNB_npp_", yearmonth), tgzs)
    
    year_month_tiles_tgz <- tgzs[year_month_tiles_idx]
    
    tif_files <- vector()
    
    for (tgz_fl in year_month_tiles_tgz)
    {
      message("Extracting ", tgz_fl, " ", date())
      
      message("Getting list of files in ", tgz_fl, " ", date())
      tgz_file_list <- untar(tgz_fl, list = TRUE)
      #tgz_file_list <- stringr::str_replace(tgz_file_list,"./","")
      
      tgz_avg_rad_filename <- tgz_file_list[grep("svdnb.*.avg_rade9.tif$",tgz_file_list, ignore.case = T)]
      tif_files <- c(tif_files, tgz_avg_rad_filename)
      
      message("Decompressing ", tgz_avg_rad_filename, " ", date())
      untar(tgz_fl, files = tgz_avg_rad_filename)
    }
    
    #tifs = list.files(raster_dir, pattern = "^svdnb.*.avg_rade9.tif$", ignore.case = T)
    
    message("Reading in the rasters " , date())
    #rast_upper <- raster(paste(raster_dir,"/",tifs[1],sep=""))
    rast_upper_filename <- tif_files[grep(pattern = "00N060W", x = tif_files)]
    rast_upper <- raster(rast_upper_filename)
    
    #rast_lower <- raster(paste(raster_dir,"/",tifs[2],sep=""))  
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
  write.table(extract, "nightlight_means.csv", row.names= F, sep = ",")
}else
{
  message("No nightlights to process")
}

#Exploration
nl_cols <- names(extract)[grep("^NL", names(extract))]

nl_dates <- stringr::str_replace(nl_cols, "NL_", "")

#plot boxplots of nightlights aggregated by county in sequence
for (x in nl_cols) {print(x);plot(extract[,"county_name"],extract[,x], main=x,las=2, cex.axis=0.6); Sys.sleep(1)}

nl_dates <- as.Date(paste0(nl_dates,"01"), format = "%Y%m%d")

#plot nightlights by ward in sequence
for (x in 1:nrow(extract)) {plot(nl_dates, extract[x , nl_cols], type="l", main = paste0(extract[x, "county_name"],":",extract[x, "ward_name"]), xaxt="n"); axis.Date(1, at=nl_dates,labels = format(nl_dates, "%b-%y"), las=2);abline(v=nl_dates,col = "lightgray", lty = "dotted");Sys.sleep(1)}

#is there an overall trend? means across all wards in the data month by month with a loess line
nl_means <- sapply(extract[,nl_cols], mean)

ggplot(as.data.frame(nl_dates,nl_means), aes(x=nl_dates, y=nl_means))+geom_line()+geom_smooth()


#do the yearly averages show a trend?
yearly_means <- aggregate(nl_means, by=list(as.integer(substr(nl_dates,1,4))), mean)

plot(yearly_means, type="l")

#is there a difference across constituencies? counties?

#plot boxplots of nightlights aggregated by constituency in sequence
for (x in nl_cols) {print(x);plot(extract[,"constituency_name"], sapply(extract[,x], mean, na.rm=T), main=x,las=2, cex.axis=0.6); Sys.sleep(1)}

extractA <- aggregate(extract[,nl_cols], by=list(extract[,"county_name"]), mean)

#melt the yearmonth columns into a single column to facilitate panel plotting
extractMelted <- melt(data = extractA, id.vars = "county_name", measure.vars = nl_cols, variable.name = "NL")

extractMelted$date <- as.Date(paste0(str_replace(extractMelted$NL,"NL_",""),"01"), format="%Y%m%d")

xyplot(value ~ date | county_name, data = extractMelted, type="l", par.strip.text=list(cex=0.7))

xyplot(value ~ date | county_name, data = extractMelted, type=c("l","smooth"), par.strip.text=list(cex=0.7), scales=list(y=list(log=T, equispaced.log = FALSE)))

ggplot(data = extractMelted, aes(x=date, y=value))+geom_line()+scale_y_log10()+ geom_smooth(method = "lm", na.rm =T, lwd=0.4)+facet_wrap(facets = ~ county_name)

ggplot(data = extractMelted, aes(x=date, y=value))+geom_line()+scale_y_log10()+facet_wrap(facets = ~ county_name)+ geom_smooth(method = "lm", na.rm =T, lwd=0.4)+stat_poly_eq(aes(label=..eq.label..), formula = extractMelted$value ~ extractMelted$date, parse=T)

#cloud cover

#county expenditure

#county wealth/poverty ranking

#population/population density

#cdf/constituency

#country gdp