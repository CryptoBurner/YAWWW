library(httr)
library(jsonlite)
library(readxl)
library(rvest)
library(tidyverse)
library(googlesheets4)
library(devtools)
#devtools::install_github("jessevent/crypto")
library(crypto2)
library(ggrepel)
library(coinmarketcapr)

#source("C:/Users/Michael/Pictures/nft/cmc_api.R")
#Read google sheets data into R
YAW <- read_sheet(
  'https://docs.google.com/spreadsheets/d/1h59caOY4AKRkioO7N2w4-VShrTLqJNYZIgxWev6BHvY/edit#gid=1179917078',
  skip = 1)  %>% 
  rename(tokenAddress = `Token Mint Address`,
         YAW = `Estimated Remaing Staking Yield (since 4/24/2022)`) %>% 
  select(tokenAddress, `Total Rank`, Name, YAW) 

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


write_sheet(dat.all, sheet ='https://docs.google.com/spreadsheets/d/16AchG9VJxp6_hOKxfrbs81NZjwUNEqLgfxrAWlkmLPE/edit#gid=1179917078')

g.dat <- dat.all %>% filter(diff > 0) %>% 
  #filter(Price < 20) %>% 
  dplyr::arrange(-diff) %>% mutate(best = ifelse(row_number() <=10 | nft == "SS","Top 10",""))

g.dat %>% 
  ggplot(aes(Price, YAW.num, colour = best)) +
  geom_point() +
  theme_bw() +
  xlab(paste0("NFT Price (SOL)")) +
  ylab(paste0("Total $YAW Yield Remaining")) + 
  geom_abline(intercept = 0, slope = y.s, color="red",linetype="dashed", size=1.5)  + 
  geom_label_repel(data=subset(g.dat,best == "Top 10"),
                   aes(label=Name),size = 3, color = "blue") +
  geom_label(aes(x = 11.5,y = 10*y.s,
                label = paste0("YAW price in SOL",
                "\nSOL @ $",round(sol.price,2),
                "\nYAW @ $",round(YAW.price,4))  
                ), colour = "red") +
  theme(legend.position="none") +
  ggtitle("Undervalued YAWWWW NFTs","Top Ten in Blue")


dat.all %>% filter(!is.na(Price)) %>%
  mutate(price2 = as.factor(round(Price/.5)*.5)) %>% 
  group_by(price2) %>% 
  summarise(n = n(), sol.total = sum(Price)) %>% 
  mutate(total = cumsum(n), sol.total = cumsum(sol.total)) %>% 
  ggplot() + 
  geom_bar(aes(x = price2, y = total),stat="identity", fill = "blue") +
  geom_line(aes(x = price2, y = sol.total/10),size = 1, color="red", group = 1) + 
  theme_classic() +
  ylab("Total Sales Needed") +
  xlab("Floor Price") +
  geom_text(aes(x = price2, y = total,label=total), vjust=-0.5, color="black", size=3.5) +
  ggtitle("Number of YAWWW NFT Sells on ME Needed to Move Up Floor Price") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Total SOL Sale Volume Needed"))


dat.all %>% filter(!is.na(Price)) %>%
  mutate(price2 = as.factor(round(Price/.5)*.5)) %>% 
  group_by(price2) %>% 
  summarise(n = n(), sol.total = sum(Price)) %>% 
  mutate(total = cumsum(n), sol.total = cumsum(sol.total)) %>% 
  ggplot() + 
  geom_bar(aes(x = price2, y = n),stat="identity", fill = "blue") +
  #geom_line(aes(x = price2, y = sol.total/10),size = 1, color="red", group = 1) + 
  theme_classic() +
  ylab("# Listed") +
  xlab("Price") +
  geom_text(aes(x = price2, y = n,label=n), vjust=-0.5, color="black", size=3.5) +
  ggtitle("Listing Prices") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Total SOL Sale Volume Needed"))





