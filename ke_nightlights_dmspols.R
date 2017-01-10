library(doParallel) #Allows for parallel processing using multiple cores
library(foreach) #Enables for-each statements to be used for parallel processing
library(raster) # interface to raster (e.g. TIFF) data
library(sp) #interface to spatial data processing
library(rgdal) #interface for manipulating spatial data
library(stringr)
library(R.utils)

#Modified to deal with dmsp/ols data. Ideally this script will be merged with the viirs script to provide #services from one location

masq <- function(shp,rast,i){
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
  
  #data <- data[!is.na(data)] #keep non-NA values only
  data[which(data == 255)] <- NA #in DMSP-OLS 255 == NA
  data[data < 0] <- 0
  
  return(data)
}

#Set directory path
raster_dir = "/btrfs/nightlights/"

setwd(raster_dir)

#read in the kenya shapefile containing ward administrative boundaries
ke_shp_ward <- readOGR("/btrfs/nightlights/KEN_adm_shp","KEN_adm3")

#get the extent of the shapefile
ke_extent <- extent(ke_shp_ward)

##Specify WGS84 as the projection of the raster file
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

projection(ke_shp_ward) <- CRS(wgs84)

##Obtain a list of TIF files, load in the first file in list
#get a list of the tgzs available
tar_fls <- list.files(raster_dir, pattern = "^F.*.tar", ignore.case = T)

tar_fls <- tar_fls[1]

#pick the unique year month from the available files
all_years_months <- unique(substr(tar_fls,1,7))


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

if (length(all_years_months)>0)
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