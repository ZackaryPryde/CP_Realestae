# install.packages("rvest")
# install.packages("dplyr")

library(rvest)
library(dplyr)


dat = data.frame()

for (page_result in seq(from = 1, to = 143, by = 1)) {
  link = paste0("https://www.property24.com/houses-for-sale/cape-town/western-cape/432/p",page_result ,"?sp=so%3dNewest#SortOrder")
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

test = paste0(dat$area, ", South Africa")
