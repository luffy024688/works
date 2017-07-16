##Gomaji 
http://www.gomaji.com/index.php?city=Taipei&ref=396&gclid=Cj0KEQiAx7XBBRCdyNOw6PLHrYABEiQAJtyEQ4edEthTWJmdHtk7Bng95CAAkHaY5TAQf5yUQ2KrBdAaAg_V8P8HAQ
http://www.gomaji.com/index.php?city=Taipei&ref=396&gclid=Cj0KEQiAx7XBBRCdyNOw6PLHrYABEiQAJtyEQ4edEthTWJmdHtk7Bng95CAAkHaY5TAQf5yUQ2KrBdAaAg_V8P8HAQ&page=1
library(XML)
library(xml2)
library(rvest) # must require xml2
library(httr)
## 第一頁
url <- "http://www.gomaji.com/index.php?city=Taipei&ref=396&gclid=Cj0KEQiAx7XBBRCdyNOw6PLHrYABEiQAJtyEQ4edEthTWJmdHtk7Bng95CAAkHaY5TAQf5yUQ2KrBdAaAg_V8P8HAQ&page=1"

# 可能解碼才會用到
res = GET("http://www.gomaji.com/index.php?city=Taipei&ref=396&gclid=Cj0KEQiAx7XBBRCdyNOw6PLHrYABEiQAJtyEQ4edEthTWJmdHtk7Bng95CAAkHaY5TAQf5yUQ2KrBdAaAg_V8P8HAQ&page=1")
res$content
resText = content(res,"text",encoding = "utf8") ;resText 

#法一 library(httr)
doc <- htmlParse(url,encoding = "utf8")#doc <- htmlParse(resText,encoding = "utf8")
title <-  xpathSApply(doc, '//div[@class="boxc"]//h2[@class="ref_name_2"]', xmlValue);title

# 法二 library(xml2)
doc2 <- read_html(url, encoding = "utf8")#doc2 <- read_html(resText, encoding = "utf8")
title2 <- xml_text(xml_find_all(doc2,'//div[@class="boxc"]//h2[@class="ref_name_2"]'));title2

#法三 library(rvest)
doc3 <-read_html(url,encoding = "UTF-8") #doc3 <-read_html(resText,encoding = "UTF-8")
title3 <- html_text(html_nodes(doc3,"#lb_deal .ref_name_2"));title3
#storename<-read_html(gomaji,encoding = "UTF-8")%>%html_nodes("#lb_deal .ref_name_2")%>%html_text()

#法四 library(rvest) x-path 
doc4 <-read_html(url,encoding = "UTF-8") #doc3 <-read_html(resText,encoding = "UTF-8")
title4 <- html_text(html_nodes(doc4,xpath='//div[@class="boxc"]//h2[@class="ref_name_2"]'));title4

##共幾頁
place <- "Taichung"
i <- 1
url<-paste0("http://www.gomaji.com/index.php?city=",place,"&ch=7&page=", i)
doc <-  read_html(url, encoding = "utf8")

while(grepl("img/error404.jpg", doc)==F){
  print(i)
  i <- i + 1
  url<-paste0("http://www.gomaji.com/index.php?city=",place,"&ch=7&page=", i)
  doc <-  read_html(url, encoding = "utf8")
  page <- i-1
}
page


##所有頁 
allurl <- c()
for(i in c(1:page)){
  url<-paste0("http://www.gomaji.com/index.php?city=",place,"&ch=7&page=", i)
  allurl <- c(allurl, url)
}
allurl

# for(i in c(1:10)){print(paste0("http://www.gomaji.com/index.php?city=",place,"&ch=7&page=", i))}

#===========
###法一 各欄位抓完再合成data_frame
##取得所有連結
allhref <- c()
for(i in c(1:page)){
  doc   <- read_html(allurl[i])
  href <- html_attrs(html_nodes(doc,xpath="//*[@id='lb_deal']//li/a"))
  href <- sapply(href,"[",1)
  href <- paste0("http://www.gomaji.com/",href)
  allhref <- c(allhref, href)
  
}
allhref
#檢查
length(allhref)
unique(allhref)
length(unique(allhref))


##每頁title
alltitle <- c()
for(i in c(1:page)){
  doc   <- read_html(allurl[i])
  title <- html_text(html_nodes(doc,"#lb_deal .ref_name_2"))
  alltitle <- c(alltitle, title)
}
alltitle
##組data
df <- data_frame(alltitle,allhref)


###法二 先建個dataframe 在開始抓個個
Sys.time() # 顯示資料讀取時間
df <- data_frame()

for(i in c(1:page)){
  doc   <- read_html(allurl[i])
  
  href <- doc %>% html_nodes(xpath="//*[@id='lb_deal']//li/a") %>% html_attrs() %>%
    sapply("[",1) %>%
    paste0("http://www.gomaji.com/",.)
  title <- doc %>% html_nodes("#lb_deal .ref_name_2") %>% html_text()
  
  
  
  #組data
  dftest <- data_frame(href,title)
  df <- rbind(df,dftest)

}


gomachi2 = load("Taipei.RData")




