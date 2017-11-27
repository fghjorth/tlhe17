#setwd("C:/Users/kzc744/Dropbox/teaching/tlhe/tlheproject")
setwd("~/Dropbox/teaching/tlhe/tlheproject")

require(dplyr)
require(googlesheets)
require(magrittr)
require(tidyr)
require(lubridate)

#write function
getratings<-function(gurl,textnames,u4=FALSE){
  udt<-gurl %>% 
    gs_url() %>% 
    gs_read()
  
  #if week 4, remove some special responses which we don't need
  if(u4==TRUE){
    udt<-udt[,-c(4:7)]
  }
  
  #fix names
  names(udt)<-c("timestamp0","idno",textnames)
  
  #fix time stamp - don't really need to do this, but ignoring it bugs me
  udt<-udt %>% 
    mutate(timestamp=mdy_hms(timestamp0)) %>% 
    dplyr::select(-timestamp0)
  
  #gather
  udtl<-gather(udt,text,rating,2:(ncol(udt)-1))
  
  udtl$idno<-as.character(udtl$idno)
  
  return(udtl)
}

#set input
u2url<-"https://docs.google.com/spreadsheets/d/16Hm7GHknOLOjwKm-zFb6on2GjDoooA7SVhZwFpGKaZA/pubhtml"
u2textnames<-c("adcock","petersen","sonderskov1")

#try importing week 2 data
u2dt<-getratings(u2url,u2textnames)

#week 3
u3url<-"https://docs.google.com/spreadsheets/d/1TQcASYlqkAwcLaGhLBKAcOXfBmWcy5VzKguKxZs-DgQ/pubhtml"
u3textnames<-"agrestifinlay1"
u3dt<-getratings(u3url,u3textnames)

#week 4 has some additional questions, so needs custom treatment
u4url<-"https://docs.google.com/spreadsheets/d/18FEURRiXLOM3dES2ZXfnQk82stx9_9aQ3sXTKdLKqqI/pubhtml"
u4textnames<-"agrestifinlay2"
u4dt<-getratings(u4url,u4textnames,u4=T)

#week 5
u5url<-"https://docs.google.com/spreadsheets/d/1OZWlo7xPbcGbHKE-9PjLO1FDBkL7pAfCPkSgTUw4KPQ/pubhtml"
u5textnames<-c("agrestifinlay3","sonderskov2")
u5dt<-getratings(u5url,u5textnames)

#week 7
u7url<-"https://docs.google.com/spreadsheets/d/1-yAP59jBsQ6LJZotDFm3fj4ns0lcuTkLmYOfaH1K3TY/edit?usp=sharing"
u7textnames<-"egerod"
u7dt<-getratings(u7url,u7textnames)

#collect all the data!
allratings<-bind_rows(u2dt,u3dt,u4dt,u5dt,u7dt)

saveRDS(allratings,"tlhe_allratings.rds")
