#Set data and location
setwd("/Users/Fr333y3d3a/Desktop")
cont <- read.csv("NH_Contribution_v2.csv")

#Stock libraries
library(dplyr)
library(Hmisc)
library(DataCombine)
library(rvest)
library(stringr)

##Clean
data <- select(cont, "cand_nm":"contbr_city", "ZipCode":"contb_receipt_dt", "election_tp")

zap <- data$ZipCode
zap0 <- paste0("0", zap)
data$ZipCode <- zap0

##Reference and scrape
zip_url <- "https://www.unitedstateszipcodes.org/nh/#zips-list"
html <- xml2::read_html(zip_url)

zips <- html %>% 
  html_nodes(".panel-prefixes .col-xs-12") %>% 
  html_text()

##Clean rvest
zips <- str_remove_all(zips,"\r")
zips <- str_remove_all(zips,"\n")
zips <- str_remove_all(zips,"\t")

##Structure to df
zhead <- zips[5:0]
def (skel()){
  z <- seq(1,1425,by=5)
  y <- z + 1
  x <- y + 1
  w <- x + 1
  v <- w + 1
  z <- zips[z]
  y <- zips[y]
  x <- zips[x]
  w <- zips[w]
  v <- zips[v]
  lizt <- list(v,w,x,y,z)
}
zip_df <- as.data.frame(lizt, col.names = zhead)
zip_df <- slice(zip_df,2:285)

##Create separate counties 
common <- zip_df %>% 
  select(County, ZIP.Code)
def {
hills <- common %>% 
  filter(common$County == "Hillsborough County")
hills <- (hills$ZIP.Code)

rock <- common %>% 
  filter(common$County == "Rockingham County")
rock <- (rock$ZIP.Code)

graf <- common %>% 
  filter(common$County == "Grafton County")
graf <- (graf$ZIP.Code)

belk <- common %>% 
  filter(common$County == "Belknap County")
belk <- (belk$ZIP.Code)

carr <- common %>% 
  filter(common$County == "Carroll County")
carr <- (carr$ZIP.Code)

merr <- common %>% 
  filter(common$County == "Merrimack County")
merr <- (merr$ZIP.Code)

sull <- common %>% 
  filter(common$County == "Sullivan County")
sull <- (sull$ZIP.Code)

dic <- list(
  Hillsborough = hills,
  Rockingham = rock,
  Grafton = graf,
  Belknap = belk,
  Carroll = carr,
  Merrimack = merr,
  Sullivan = sull
)
}

##Dictionary 4 county
data$county <- data$ZipCode
replaces <- data.frame(from = common$ZIP.Code, to = common$County)
data <- FindReplace(data = data, Var = "county", replaceData = replaces, from = "from", to = "to", exact = TRUE, vector = FALSE)

##Organizing by county
data <- select(data, cand_nm, ZipCode, contb_receipt_amt:county)
by_county <- data %>% 
  filter(county != "00") %>% 
  filter(county != "01006") %>% 
  filter(county != "065201") %>% 
  filter(county != "099999") %>% 
  group_by(county)

gen16 <- by_county %>% 
  filter(election_tp == "G2016")

pri20 <- by_county %>% 
  filter(election_tp == "P2020") %>% 
  select(cand_nm:contb_receipt_amt,county) %>% 
  filter(contb_receipt_amt > 0) %>% 
  filter(contb_receipt_amt < 2801)

pri20 %>% 
  summarise(n = n(),
            mean = mean(contb_receipt_amt),
            sd = sd(contb_receipt_amt),
            min = min(contb_receipt_amt),
            q1 = quantile(contb_receipt_amt, probs = 0.25),
            median = median(contb_receipt_amt),
            q3 = quantile(contb_receipt_amt, probs = 0.75),
            max = max(contb_receipt_amt)
            )

pri20$county <- str_remove_all(pri20$county, " County")

library(beanplot)
options(scipen = 6)
beanplot(contb_receipt_amt ~ county,
        data=pri20,
        main="Bean Plots of NH County Contributions",
        xlab = "NH Counties",
        ylab = "Contribution Amount in $",
        col = "pink",
        border = "red",
        what = c(1,1,1,0)
        )



