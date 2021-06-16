library(foreign)
library(rvest)
library(xml2)
library(httr)
library(stringr)

firms <- read.csv("d:/users/andrey/dropbox/data/sicss 2021/firms2.csv")[,1]

hq <- c()

for(i in 1:10){
  
  # Recovering form
  html <- read_html("http://www.google.com")
  search <- html_form(html)[[1]]
  # Filling out form
  search <- search %>% html_form_set(q = paste(firms[i], "bloomberg",sep=" "), 
                                     hl = "en")
  # Sending form
  resp <- html_form_submit(search,submit="btnG")
  site <- read_html(resp)
  
  # Locating HTML elements
  
  links <- html_elements(site,xpath='//a[contains(@href, "bloomberg.com/profile")]')[1] %>%
    html_attr("href")
  
  # Error condition and cleaning up link text
  
  if(length(links)==0){hq[i] <- NA 
  } else {
  s <- str_extract(links,"(?<=q=)(.*?)(?=&sa)")
    
  # Extracting address
  
  h <- handle('')
  b <- read_html(s,handle=h)
  hq[i] <- html_text(xml_siblings(html_element(b,xpath='//h2[contains(text(),"ADDRESS")]'))) %>%
    str_replace_all("\n"," ")
  
  }
  # Set pause time
  t <- runif(1,5,10)
  Sys.sleep(t)
}

library(RSelenium)

## Initialize browser
  
rD <- rsDriver(browser="firefox", port=4548L, verbose=T)
remDr <- rD[["client"]]

coord <- c()
remDr$navigate("https://www.google.com/maps")
sbox<- remDr$findElement(using = "id", value = "searchboxinput")

for(i in 1:10){
  if(is.na(hq[i])==T){coord[i]<-NA}
    else{
  
  ## Send input to search box
  sbox$sendKeysToElement(list(hq[i],key="enter"))
  
  Sys.sleep(5)
  
  # Get URL and extract coordinates
  u <- sbox$getCurrentUrl()[[1]]
  u <- str_extract(u,"(?<=@)(.*?)(?=z)")
  coord[i] <- substr(u,1,nchar(u)-3)
  sbox$clearElement()
    }
}

df <- data.frame(firms[1:10],coord)

