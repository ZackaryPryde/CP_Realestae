# install.packages("rvest")
# install.packages("dplyr")

library(rvest)
library(dplyr)
library(tidyr)

# Page 1 ----

link = "https://www.property24.com/for-sale/advanced-search/results?sp=cid%3d432%2c401%2c652%2c475%2c479%2c478%2c615%26so%3dNewest#110831808"
page = read_html(link)

area = page %>% html_nodes(".p24_regularTile .p24_location") %>% html_text()
price = page %>% html_nodes(".p24_regularTile .p24_price") %>% html_text()
content = page %>% html_nodes(".p24_regularTile .p24_content") %>% html_text()


price <- gsub(" ","", price)
price <- gsub("\r\n","", price)
price <- gsub("R","", price)

dat0 = data.frame(area, price, stringsAsFactors = F)
dat0

# Note ----

# We could continue this for 143 pages of the website, but this is time consuming and makes the code long.
# We need to find a way to do this for all pages, while adding new data to the data frame.
# This can be done using a for-loop.

# Scrape data from all pages ----

dat = data.frame()

for (page_result in seq(from = 1, to = 2278, by = 1)) {
  link = paste0("https://www.property24.com/for-sale/western-cape/9/p",page_result ,"?sp=so%3dNewest#SortOrder")
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


# Exploring the simple dataset ----

dat$area <- as.factor(dat$area)
dat$price <- as.numeric(dat$price)

View(dat)
levels(dat$area)

# Lets now drop the rows containing NA values.
dat = dat %>% drop_na()
View(dat)

summary(dat$price[dat$area == "Fish Hoek"])



# Lets summarize price by area... ----

# Method 1 - using ddplyr

dat1 = dat %>%
  group_by(area) %>%
  summarise_at(vars(price), list(name = mean))

head(dat1)
View(dat1)

# Method 2 - using the R base function "aggregate()"

# dat2 = aggregate(x = dat$price,
#           by = list(dat$area),
#           FUN = mean)

# head(dat2)

