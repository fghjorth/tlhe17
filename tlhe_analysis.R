#setwd("C:/Users/kzc744/Dropbox/teaching/tlhe/tlheproject")
setwd("~/Dropbox/teaching/tlhe/tlheproject")

require(dplyr)
require(ggplot2)
require(lme4)
require(merTools)
require(tidyr)
require(broom)
require(forcats)
require(googlesheets)
require(magrittr)

#sim data for two types of students
dislikevec<-rpois(1000,2)
likevec<-(dislikevec-7)*-1+2

#new sims
like7lo<-rnorm(1000,7,.6)
like6lo<-rnorm(1000,6,.6)
like4lo<-rnorm(1000,4,.6)
like3lo<-rnorm(1000,3,.6)
like4hi<-rnorm(1000,5.1,1.2)
like6hi<-rnorm(1000,4.9,1.2)

#sim id's - types are unobserved, but we observe id's
set.seed(123)
idnos<-base::sample(1000:9999,2000,replace=F)
t1ids<-idnos[1:1000]
t2ids<-idnos[1001:2000]

simdat1<-data.frame(rating=c(like7lo,like6lo,like4lo,like3lo),
                   week=rep(rep(c("Reading 1","Reading 2"),each=1000),2),
                   studenttype=c(rep(c("Student type 1","Student type 2"),each=1000),
                                 rep(c("Student type 2","Student type 1"),each=1000)),
                   idno=factor(c(t1ids,t2ids,t2ids,t1ids)))

simdat2<-data.frame(rating=c(like7lo,like6lo,like4lo,like3lo),
                    week=rep(rep(c("Reading 1","Reading 2"),each=1000),2),
                    studenttype=c(rep(c("Student type 1","Student type 1"),each=1000),
                                  rep(c("Student type 2","Student type 2"),each=1000)),
                    idno=factor(c(t1ids,t1ids,t2ids,t2ids)))

#check dist
simdat1 %>%
  group_by(week,studenttype) %>% 
  summarise(meanrating=mean(rating))

#plots

ggplot(simdat1,aes(x=rating,group=studenttype,fill=studenttype)) +
  theme_minimal() +
  geom_density(alpha=.3,adjust=2) +
  facet_grid(week~.) +
  labs(x="Rating on 0-10 scale",y="") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("firebrick3","orange")) +
  scale_y_continuous(breaks=0,labels="") +
  xlim(c(0,10))

ggsave("studenttypes1.pdf",width=6,height=2)

ggplot(simdat2,aes(x=rating,group=studenttype,fill=studenttype)) +
  theme_minimal() +
  geom_density(alpha=.3,adjust=2) +
  facet_grid(week~.) +
  labs(x="Rating on 0-10 scale",y="") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("firebrick3","orange")) +
  scale_y_continuous(breaks=0,labels="") +
  xlim(c(0,10))

ggsave("studenttypes2.pdf",width=6,height=2)

#plot how the same data looks without panel data
simdat1$studenttype2<-"All students"

ggplot(simdat1,aes(x=rating,fill=studenttype2)) +
  theme_minimal() +
  geom_density(alpha=.3,adjust=2) +
  facet_grid(week~.) +
  labs(x="Rating on 0-10 scale",y="") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values="darkorange2") +
  scale_y_continuous(breaks=0,labels="")

#  scale_y_continuous(breaks=c(0,.1,.2),labels=c("0",".1",".2"),limits=c(0,.2))

ggsave("studenttypes3.pdf",width=6,height=2)

#we try k-means clustering 
head(simdat1)
simdat1w<-spread(simdat1,week,rating)
simdat2w<-spread(simdat2,week,rating)
set.seed(123456)
kmeans1<-kmeans(simdat1w[,4:5],2,nstart=20)
kmeans2<-kmeans(simdat2w[,3:4],2,nstart=20)

#assign each student to estimated cluster
simdat1<-simdat1 %>% 
  left_join(.,data.frame(idno=simdat1w$idno,estclust=factor(kmeans1$cluster)),by="idno")

simdat2<-simdat2 %>% 
  left_join(.,data.frame(idno=simdat2w$idno,estclust=factor(kmeans2$cluster)),by="idno")

summary(sim1clustmod<-lm(rating~estclust,data=simdat1))
summary(sim2clustmod<-lm(rating~estclust,data=simdat2))

#read in the actual data
allratings<-readRDS("tlhe_allratings.rds")

#convert to wide form
arw<-allratings %>% 
  group_by(text,idno) %>% 
  summarise(idno2=unique(idno),rating=mean(rating,na.rm=T)) %>% 
  spread(text,rating) 

#replace NA's with column means
for(i in 3:10){
  arw[which(is.na(arw[,i])), i] <- mean(arw[,i][[1]], na.rm = TRUE)
}

arkmeans<-kmeans(na.omit(arw[,3:10]),2,nstart=20)

#assign each student to estimated cluster
allratings<-allratings %>% 
  left_join(.,data.frame(idno=arw$idno,estclust=factor(arkmeans$cluster)),by="idno")

#check coefficient on assigned cluster
summary(arclustmod<-lm(rating~estclust,data=allratings))

ratingsrem<-lmer(rating~1+(1|text)+(1|idno),data=allratings)
summary(ratingsrem)

#how many texts?
length(unique(allratings$text))

#how many unique students
length(unique(allratings$idno))

#get cluster coef for each model
clustdf<-data.frame(data=factor(rep(c("Figure 1","Figure 2","Real data"),each=1)),
                  varsource=rep(c("Coefficient on assigned cluster"),3),
                  coef=NA,se=NA)

clustdf[1,3:4]<-tidy(sim1clustmod)[2,2:3]
clustdf[2,3:4]<-tidy(sim2clustmod)[2,2:3]
clustdf[3,3:4]<-tidy(arclustmod)[2,2:3]

#flip coefficient for easier presentation (sign is arbitrary)
clustdf[2,3]<-clustdf[2,3]*-1

#plot
ggplot(clustdf,aes(x=coef,y=fct_rev(data))) +
  geom_vline(xintercept=0,linetype="dashed",color="grey70") +
  geom_point(size=2.8) +
  geom_errorbarh(aes(xmin=coef-1.96*se,xmax=coef+1.96*se),height=0) +
  geom_errorbarh(aes(xmin=coef-1.65*se,xmax=coef+1.65*se),height=0,size=1.2) +
  theme_minimal() +
  scale_x_continuous(breaks=0:3) +
  labs(x="Coefficient on assigned cluster",y="") 

ggsave("clustdots.pdf",width=6,height=2)

#merge in cluster to show differences
arw$estcluster<-arkmeans$cluster

#plot
ggplot(arw,aes(x=agrestifinlay1,y=egerod,color=factor(estcluster))) +
  geom_jitter() +
  theme_bw() +
  scale_color_manual(values=c("firebrick3","orange"),name="Cluster") +
  labs(x="Rating of 'Agresti & Finlay'",y="Rating of 'Egerod'")

ggsave("clustex.pdf",width=5,height=2)

#get the icc for each model
iccdf<-data.frame(data=factor(rep(c("Figure 1","Figure 2","Real data"),each=1)),
                  varsource=rep(c("Between-student variation"),3),
                  icc=NA)
iccdf[1,3]<-ICC("rating","idno",simdat1)
iccdf[2,3]<-ICC("rating","idno",simdat2)
iccdf[3,3]<-ICC("rating","idno",allratings)

#plot
ggplot(iccdf,aes(x=icc,y=fct_rev(data))) +
  geom_vline(xintercept=0,linetype="dashed",color="grey70") +
  geom_point(size=2.8) +
  theme_minimal() +
  scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1")) +
  labs(x="Between-student variation",y="") 
  
ggsave("iccdots.pdf",width=6,height=2)


#calculate birthday problem style quantity
bdayprob<-function(n,days){
  pn<-1 - exp((-n*(n-1))/(2*days))
  return(pn)
}

#test
bdayprob(23,365)

#calculate
1-bdayprob(100,36500)

