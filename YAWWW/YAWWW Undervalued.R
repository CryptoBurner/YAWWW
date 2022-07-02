ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
pkg <- c("here","httr","jsonlite","readxl","rvest","tidyverse","googlesheets4","devtools","crypto2","ggrepel","coinmarketcapr","plotly")
ipak(pkg)


source(here::here("cmc_api.R"))
#Read google sheets data into R
YAW <- read_sheet(
  'https://docs.google.com/spreadsheets/d/1h59caOY4AKRkioO7N2w4-VShrTLqJNYZIgxWev6BHvY/edit#gid=1179917078',
  skip = 1)  %>% 
  rename(tokenAddress = `Token Mint Address`,
         YAW = `Estimated Remaing Staking Yield (since 4/24/2022)`) %>% 
  select(tokenAddress, `Total Rank`, Name, YAW,Image) 

#Read in Magic Eden Listing Prices
ss.dat <- lapply(as.list(seq(0,200,20)), function(x){
  a=GET(paste0("https://api-mainnet.magiceden.dev/v2/collections/solstein/listings?offset=",x[1],"&limit=20"))
  b=fromJSON(rawToChar(a$content))
  tibble(tokenAddress = b$tokenMint, Price = b$price)
}
)
qt.dat <- lapply(as.list(seq(0,200,20)), function(x){
  a=GET(paste0("https://api-mainnet.magiceden.dev/v2/collections/quantum_traders/listings?offset=",x[1],"&limit=20"))
  b=fromJSON(rawToChar(a$content))
  tibble(tokenAddress = b$tokenMint, Price = b$price)
  
  
}
)
yawww.dat <- do.call("bind_rows",ss.dat) %>% mutate(nft = "SS") %>% 
  bind_rows(do.call("bind_rows",qt.dat) 
            
            
  )


toSymbol = "USD"
amount= 1

#Current YAW Price
fromSymbol = "YAW"
url = paste0("https://pro-api.coinmarketcap.com/v1/tools/price-conversion",
             "?amount=",amount,"&symbol=",fromSymbol,"&convert=",toSymbol)
# GET request
pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                      `X-CMC_PRO_API_KEY` = PASS$apikey))
dt<- fromJSON(rawToChar(pg$content))
YAW.price <-  dt$data$quote$USD$price



#Current SOL Price
fromSymbol = "SOL"
url = paste0("https://pro-api.coinmarketcap.com/v1/tools/price-conversion",
             "?amount=",amount,"&symbol=",fromSymbol,"&convert=",toSymbol)
# GET request
pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                      `X-CMC_PRO_API_KEY` = PASS$apikey))
dt<- fromJSON(rawToChar(pg$content))
sol.price <-  dt$data$quote$USD$price

#YAW Per SOL
y.s <- sol.price/YAW.price


dat.all <- yawww.dat %>% left_join(YAW) %>% 
  mutate(NFT.cost = (Price*sol.price)/YAW.price,
         YAW.num = as.numeric(gsub("[^0-9.-]", "", YAW))) %>% 
  mutate(diff = YAW.num - NFT.cost) 

g.dat <- dat.all %>% filter(diff > 0) %>% 
  #filter(Price < 20) %>% 
  dplyr::arrange(-diff) %>% mutate(best = ifelse(row_number() <=10 | nft == "SS","Top 10",""))




