library(rvest)
library(httr)
library(dplyr)
library(lubridate)
library(stringr)
library(magrittr)
library(highcharter)
library(timetk)
library(readr)
library(plotly)
library(DT)
library(RSelenium)
library(tidyverse)
library(jsonlite)
library(ggthemes)
library(ggplot2)
library(tableHTML)
library(xml2)
library(data.table) 

#1, Shiller Cape index"
url = 'https://www.multpl.com/shiller-pe/table/by-month'
data = GET(url)

data_table = data %>% read_html() %>% html_table() %>% .[[1]]
head(data_table)

data_table = data_table %>%
  mutate(Date = as.Date(Date, format = "%B %d, %Y")) %>%
  set_colnames(c('Date', 'Value')) %>%
  tk_xts()

highchart(type = 'stock') %>%
  hc_add_series(data_table) %>%
  hc_scrollbar(enabled = TRUE)

#2, investing.com# #Major Indices#
url1 = 'https://www.investing.com/indices/major-indices'
data = GET(url1)
data_tableI = data %>% read_html() %>% html_table()
data_tableI[[1]] %>% select(Index, Last, `Chg. %`, Time) %>% datatable()

#3, ETFs#
url2 <- "https://www.investing.com/etfs/major-etfs"
data = GET(url2)
data_tableE = data %>% read_html() %>% html_table()
data_tableE[[1]] %>% select(Name, Symbol, Last, `Chg. %`, `Vol.`) %>% datatable()

#4, ETFs performance#
rD <- rsDriver(port=4800L, chromever="101.0.4951.41")
remDr <- rD$client
URL <- "https://www.investing.com/etfs/major-etfs"
remDr$navigate(URL)

pattern <- "#filter_performance"
element <- remDr$findElement(using = "css", pattern)
element$clickElement()

etfp <- remDr$getPageSource()[[1]]
res <- read_html(etfp)

tab <- res %>% 
  html_table() %>%
  .[[1]] %>% as.data.frame()

#save for later use in Markdown
#write_tableHTML(tableHTML(tab), file = 'tabetf.html')
write_csv(tab,"tabetf.csv")

#extract data from the html file saved <- Dashboard용 코드
tabetf <-read.csv("tabetf.csv")
colnames(tabetf)[5] <- "1 Month"
colnames(tabetf)[8] <- "3 Years"

tabetf  %>%  select(Name, Daily, "1 Month", YTD, "3 Years")%>% datatable()

#5, ETF fundflows#
rD <- rsDriver(port=4900L, chromever="101.0.4951.41")
remDr <- rD$client
URL1 <- "https://www.etf.com/etfanalytics/etf-fund-flows-tool"
remDr$navigate(URL1)

data1 <- remDr$getPageSource()[[1]]
res <- read_html(data1)

#Top 10 Creations (All ETFs) weekly basis
tab1 <- res %>% 
  html_table()%>%
  .[[1]] %>% as.data.frame()
write_csv(tab1,"Topcreation.csv")

#read csv
tab1 <-read.csv("Topcreation.csv")
colnames(tab1)[3] <- "Net Flow"
tab1 %>%
  ggplot(aes(x = reorder(Ticker,`Net Flow`), y = `Net Flow`, label=`Net Flow`)) + 
  ggtitle("Top 10 Creations")+
  geom_bar(stat = 'identity', fill='skyblue') +
  geom_text(color = 'black', size = 3, hjust = -0.5) +
  xlab("Ticker") +
  ylab("(m USD)") +
  coord_flip() + 
  theme_classic()
ggsave("Top 10 creation weekly.png")

#Top 10 Redemptions (All ETFs) weekly basis
tab2 <- res %>% 
  html_table() %>% 
  .[[2]] %>% as.data.frame()

write_csv(tab2,"Topredemption.csv")

tab2 <-read.csv("Topredemption.csv")
colnames(tab2)[3] <- "Net Flow"

tab2 %>% 
  ggplot(aes(x = reorder(Ticker,`Net Flow`), y = `Net Flow`, label=`Net Flow`)) + 
  ggtitle("Top 10 Redemption")+
  geom_bar(stat = 'identity', fill='skyblue') +
  geom_text(color = 'black', size = 3, hjust = -0.5) +
  xlab("Ticker") +
  ylab("(m USD)") +
  coord_flip() + 
  theme_classic()
ggsave("Top 10 redemption weekly.png")
#조교님께 물을것, 축 설정. 왜 안되는지, reorder 왜 안되는지..)

