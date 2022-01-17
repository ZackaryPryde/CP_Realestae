# This script follows the basic web scraping exercise demonstrated in the other
# two scripts, only now I have specified a subset of suburbs within the Cape Peninsula 
# of the Western Cape (South Africa). 

# 1. Package installations ----

# install.packages("rvest")
# install.packages("dplyr")
# install.packages("tmap")
# install.packages("leaflet")
# install.packages("tmaptools")

# 2. Loading the required libraries ----

library(rvest)
library(dplyr)
library(tidyr)
library(rgdal)
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)
library(tmap)
library(leaflet)
library(tmaptools)


# 3. Scraping Page 1 of the property24 Website ----

link = "https://www.property24.com/for-sale/advanced-search/results?sp=cid%3d615%2c479%2c478%2c652%2c475%26s%3d11742%2c9044%2c15183%2c9057%2c9040%2c9034%2c9025%2c9047%2c10211%2c12788%2c16635%2c10180%2c9036%2c10213%2c10174%2c9039%2c9067%2c9069%2c9097%2c10178%2c11740%2c12840%2c15132%2c16720%2c10094%2c10195%2c10052%2c10189%2c10207%2c10096%2c10170%2c10204%2c10212%2c10090%2c10179%2c10109%2c10102%2c8778%2c8792%2c8812%2c9118%2c11014%2c11012%2c8661%2c14225%2c14224%2c8669%2c11741%2c8682%2c8679%2c8806%2c8787%2c8734%2c8783%2c8779%2c10157%2c8017%2c9153%2c8683%2c8677%2c8686%2c8717%2c8754%2c9163%2c9145%2c9149%2c9155%2c9166%2c10166%2c9138%2c11021%2c11016%2c11013%2c11015%2c11017%2c9169%2c11018%2c9136%2c9141%2c9143%2c10164%2c10163%2c10161%2c16541%2c10158%2c9119%2c8731%2c8736%2c8730%2c8716%2c8800%2c9029%2c12195%2c17446%2c17447%2c15627%2c9103%2c9101%2c9106%2c15622%2c9104%2c16197%2c9095%2c7851%2c9110%2c9102%2c9105%2c9108%2c9085%2c9107%2c10603%2c9062%2c17448%2c9094%2c16008%2c10203%2c8025%2c8010%2c8005%2c8019%26so%3dNewest#SortOrder"
page = read_html(link)

area = page %>% html_nodes(".p24_regularTile .p24_location") %>% html_text()

price = page %>% html_nodes(".p24_regularTile .p24_price") %>% html_text()

price <- gsub(" ","", price)
price <- gsub("\r\n","", price)
price <- gsub("R","", price)

dat0 = data.frame(area, price, stringsAsFactors = F)
dat0

# Note

# We could continue this for 143 pages of the website, but this is time consuming and makes the code long.
# We need to find a way to do this for all pages, while adding new data to the data frame.
# This can be done using a for-loop.

# 4. Scrape data from all pages ----

dat = data.frame()

for (page_result in seq(from = 1, to = 500, by = 1)) {
  link = paste0("https://www.property24.com/for-sale/advanced-search/results/p",page_result ,"?sp=cid%3d615%2c479%2c478%2c652%2c475%26s%3d11742%2c9044%2c15183%2c9057%2c9040%2c9034%2c9025%2c9047%2c10211%2c12788%2c16635%2c10180%2c9036%2c10213%2c10174%2c9039%2c9067%2c9069%2c9097%2c10178%2c11740%2c12840%2c15132%2c16720%2c10094%2c10195%2c10052%2c10189%2c10207%2c10096%2c10170%2c10204%2c10212%2c10090%2c10179%2c10109%2c10102%2c8778%2c8792%2c8812%2c9118%2c11014%2c11012%2c8661%2c14225%2c14224%2c8669%2c11741%2c8682%2c8679%2c8806%2c8787%2c8734%2c8783%2c8779%2c10157%2c8017%2c9153%2c8683%2c8677%2c8686%2c8717%2c8754%2c9163%2c9145%2c9149%2c9155%2c9166%2c10166%2c9138%2c11021%2c11016%2c11013%2c11015%2c11017%2c9169%2c11018%2c9136%2c9141%2c9143%2c10164%2c10163%2c10161%2c16541%2c10158%2c9119%2c8731%2c8736%2c8730%2c8716%2c8800%2c9029%2c12195%2c17446%2c17447%2c15627%2c9103%2c9101%2c9106%2c15622%2c9104%2c16197%2c9095%2c7851%2c9110%2c9102%2c9105%2c9108%2c9085%2c9107%2c10603%2c9062%2c17448%2c9094%2c16008%2c10203%2c8025%2c8010%2c8005%2c8019%26so%3dNewest")
  page = read_html(link)
  
  area = page %>% html_nodes(".p24_regularTile .p24_location") %>% html_text()
  
  price = page %>% html_nodes(".p24_regularTile .p24_price") %>% html_text()
  
  dat = rbind(dat, data.frame(area, price, stringsAsFactors = F))
  
  print(paste("page:", page_result))
}

dat$price <- gsub(" ","", dat$price)
dat$price <- gsub("\r\n","", dat$price)
dat$price <- gsub("R","", dat$price)

View(dat)

# 5. Initial extra steps ----

# Here I change the two variables to factor and numeric for ease of use.
dat$area <- as.factor(dat$area)
dat$price <- as.numeric(dat$price)
View(dat)

# Lets now drop the rows containing NA values. 
# These have come from listings that do not include the house price.
dat = dat %>% drop_na()
View(dat)

# Here is a quick test summary of the property value for houses listed in Fish Hoek
summary(dat$price[dat$area == "Fish Hoek"])

# 6. Lets summarize price by area... ----

# This method uses the ddplyr package

dat1 = dat %>%
  group_by(area) %>%
  summarise_at(vars(price), list(name = median))

# Add nrows column for each area - number of properties for sale per area (Add to dataset later)

datx <- dplyr::count(dat, area) 
datx

head(dat1)
# View(dat1)

# 7. Aligning the realestate data with a shape file for use in GIS ----

# Now we need to work with the Cape Peninsula listing area as a geospatial layer.
# One would typically use these files in GIS software, however they may be 
# visualized in R.

# I have used a spatial extent of the Cape Peninsula from a previous project to
# clip the suburban classification layer obtained from the government open data portal.
# We could theoretically choose our own extent of any size, but when using a google api this may 
# lead to more error in geocoding the "area" titles, and the google api only allows a certain amount
# of entries per day... Thus I have chosen just the Cape Peninsula

CPsub = readOGR("CP.shp")
plot(CPsub)

CPsub$NAME
dat1$area

# Between the two sets of area names, one is in upper case and one is in lower case... 
# We need to check the similarities, and hence they must both be in upper case to do so.

dat1 = dat1 %>% 
  mutate(across(where(is.factor), toupper))

dat2 = data.frame("place" = dat1$area, "price" = dat1$name, "n" = datx$n)
dat2$place = paste0(dat2$place, ", WESTERN CAPE, SOUTH AFRICA")
dat2

# Note that some of these listing areas are within the same suburb when overlaid to the government
# municipal classification. This is a realestate strategy for selling houses when there are both high and 
# low income regions in a given suburb thus they list the names as different.
# Eg: Rondebosch east and Rondebosch.

# Here is some code that demonstrates this:
sort(CPsub$NAME)
sort(dat1$area)

# P24 has 199 areas where the government recognizes far less. 
# There are also some areas from p24 that I have scraped from but are outside of the 
# catchment area (extent) that I have chosen to use.

# Also note that I havent discarded the differing areas because names arent exactly matching at this point.
# we need to overlay the areas to our municiple map then keep those under a given suburb.

# 8. Obtaining gps coordinates for the "areas" that we have scraped ----

# Very cool demonstration @ https://www.jessesadler.com/post/geocoding-with-r/

cities_df = data.frame("place" = dat2$place)

# To use the ggmap package, we need to have registered with google cloud services, and have an API key

api = "xxxx"
register_google(key = api)

locations_df <- mutate_geocode(cities_df, place)
locations_df$Price <- dat2$price
locations_df$n <- dat2$n
locations_df

# We now have the median prices, number of listings and gps coordinates for each listing in the web data

# Lets look at the data on an interactive map
locations_sf <- st_as_sf(locations_df, coords = c("lon", "lat"), crs = 4326)
mapview(locations_sf)

# We can see that there are some points the google engine has not processed correctly.
# additionally, there are some area names from property24 that have not got a unique geocode. 
# In the case of this project, I will opt to discard those that are incorrect. 
# This may skew the data slightly, but the bulk of points should account for the general property 
# value trends. 

# Lets now save this data and export to QGIS.
# This allows for an interactive data filtering process, however, the same result can be achieved in R too.

# For QGIS ease of use, I will change the order of the columns, and discard the p24 "area" column
# since we dont need it anymore.
locations = data.frame("lon" = locations_df$lon, "lat" = locations_df$lat, "Price" = locations_df$Price, "n" = locations_df$n)
locations
write_csv(locations, "locations.csv")

# What did I do in QGIS? ----

# I first loaded in our p24 data points and the Cape Peninsula shapefile. 

# I clipped the p24 data to the extent of the CP shape, and removed points in districts that
# were cut off by the clip process.

# I alligned the CRS's of the two layers, then intersected the data to get a points layer 
# with gps locations according to the suburbs of the gov shapefile. 

# Now the shapefile has multiple points in suburbs of the study extent, and I need to import the file back
# to R such that I can create a new data frame that summarizes the property data to one gps point
# in a given suburb.

# - - - - - - - - - - - - - - -

# 9. Importing the QGIS spatial data we have just created ----

CPintersect <- st_as_sf(st_read("CPintersect.shp"))

CP_new_n <- aggregate(x = CPintersect$n, by = list(CPintersect$NAME), FUN = sum)
CP_new_p <- aggregate(x = CPintersect$Price, by = list(CPintersect$NAME), FUN = mean)
CP_new_lon <- aggregate(x = CPintersect$lon, by = list(CPintersect$NAME), FUN = median)
CP_new_lat <- aggregate(x = CPintersect$lat, by = list(CPintersect$NAME), FUN = median)


(CP_new = data.frame("lon" = CP_new_lon$x,
                    "lat" = CP_new_lat$x,
                    "Area" = CP_new_n$Group.1,
                    "price" = CP_new_p$x,
                    "n" = CP_new_n$x))

(CP_new_no_gps = data.frame("Area" = CP_new_n$Group.1,
                            "Price" = CP_new_p$x,
                             "n" = CP_new_n$x))

# The above looks good! I have one problem though... 
# The "Cape Farms district" is geospatially split over two regions, hence I cannot use 
# a standard statistical function (i.e., median lat & lon) to get a single point that
# represents the data for this area as I have done with the other suburbs.
# In this special case, I have to manually assign the lat and lon so that our point doesnt
# intrude into another suburb. Is this alright?

# I have realized that assigning gps coords by a collective median may not be 
# the best way to proceed. Lets see what we can do in QGIS

# In QGIS, I have taken a random point within each polygon and imported that shapefile to R

Rand_gps <- st_as_sf(st_read("RTN.shp"))
# View(Rand_gps)

Rand_gps_new <- data.frame(Rand_gps$geometry, "Area" = Rand_gps$NAME, "Price" = Rand_gps$locations2)
Rand_gps_new = Rand_gps_new %>% drop_na()
Rand_gps_new = Rand_gps_new[order(Rand_gps_new$Area),]
Rand_gps_new

CP_new_no_gps$geometry = Rand_gps_new$geometry
CP_Final = CP_new_no_gps

write_csv(CP_Final, "CP_Final.csv")
write_csv(CP_new_no_gps, "CP_Final2.csv")

# In some regards, we could have discarded the gps coordinates by now, which would
# make things MUCH simpler. This is because QGIS allows us to attribute the median price to
# the CP shapefile by name, rendering missing values as NA. 

# I opted to take the complicated route and keep a separate file for the data with gps coordinates per suburb
# in case they would become useful at some point.

# 10. Playing around in QGIS ----

# In QGIS I combined data via the suburb names, completely leaving out the hard work I did to have 
# a spatial point per district. Note that the spatial points are worth keeping though.

# The next section uses this shapefile for plotting. 

# Testing the visualization in R ----

CPM = st_read("CapePenMap.shp")
CPMn = st_as_sf(CPM)
CPMn = data.frame("Suburb" = CPM$NAME, "Median Price" = CPM$CP_Final2_, "No. listed" = CPM$CP_Final_1, CPM$geometry)
CPMn = st_as_sf(CPMn)

#View(CPM)

# Using Mapview and ggmap

mapview(CPMn)

# using ggplot (Credits to https://www.youtube.com/watch?v=GMi1ThlGFMo)

ggplot(CPMn) + 
  geom_sf(aes(fill = Median.Price)) + 
  scale_fill_gradient2()

ggplot(CPMn) + 
  geom_sf(aes(fill = No..listed)) + 
  scale_fill_gradient2()


# using tmap

tmap_mode("plot")

(test_map <- tm_shape(CPMn) + 
  tm_polygons("Median.Price", id = "Suburb", palette = "Greens"))

tmap_mode("view")
tmap_last()

tmap_save(test_map, "test_map.html")

tmap_mode("plot")
