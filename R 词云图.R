z=1
myc=c()
for(i in 1:20){
  z=z*i
  myc=c(myc,z)
  
}
myc[3]
myc=c(2,4,1)
sum(myc)
for(i in seq(from 1 to 500 by 1)){
  z=z+i
  print(z)
}
myc=c()
for(i in 1:100){
  z=z+i
  myc=c(myc,z)
  
}
myc 
rep(2,times=10)
x=rep(2,times=5)[2]
x
y=rep(c(2,3),times=3)
print(y[2])
rep(c(2,3),each=6)
print("hello world")
print(hello world)
paste("hello","world",sep=" ")
myf=rep(1,times=20)
z=1
for(i in 2:20){
  myf[i]=myf[i-1]*i
}
print(myf)
new_sum=function(x){
  for(i in 1:x-1){
    x=x+i
  }
  return(x)
}
print(new_sum(3))
5>9
p_test=function(x){
  if(x==1){result=FALSE}
  else if(x==2){result=TRUE}
  else{
    result = TRUE
    for(i in 2:(x-1)){
      if(x%%i==0){result=FALSE}
    }}
  return(result)}

p_test(13)
data=read.csv("Car04.txt", header=TRUE, stringsAsFactors=FALSE)
head(data)
dim(data)
str(data)
Advertising=read.csv("Advertising.csv",header=TRUE, stringsAsFactors=FALSE)
dim(Advertising)
str(Advertising)
car=data
view(car)
car=view()
rm(data)
out=car$Make.Model[which(car$Horsepower>400)]
print(out)
library(dplyr)
3 %>% 
+4 %>% 
+5
data_new=car %>% filter(Horsepower>400)
data_new
glimpse(car)
install.packages('data.table')
library(data.table)
data2=fread('Car04.txt',header=TRUE,stringsAsFactors = FALSE)
data2
data2$`Make/Model`
data2[1,1]
data2[Horsepower>400 & EPA_Class %in% c("two_seater")]
data2[Horsepower>400][,list(`Make/Model`,Horsepower)]
write.csv(data2,file="sorted_data2")
rm(data2)
rm(list=ls())

library(ggplot2)
library(dplyr)
setwd("/Users/zoey/Desktop/DS Resources")
car=read.csv("Car04.txt", header=TRUE, stringsAsFactors = FALSE)
plot(car$Horsepower,car$Displacement)
abline(a=0.8,b=0.1,lwd=4,col="red")
car %>% ggplot(aes(x=Horsepower, y=Displacement))+geom_point()+geom_abline(intercept=0.8,slope=0.01,col="red")
install.packages("ggthemes")
library(ggthemes)
 car %>% ggplot(aes(x=Weight.lb.,y=Horsepower))+geom_point()+facet_wrap(~EPA_Class)
setwd("/Users/zoey/Desktop/DS Resources")
data_a=read.csv("Advertising.csv",header=TRUE,stringsAsFactors=FALSE)
model_1=lm(sales~radio,data=data_a)
summary(model_1)
plot(model_1,1)
plot(model_1,2)
new=data_a[1,]
new[2]=100
predict(model_1,new,interval="prediction",se.fit=TRUE,level=0.95)
install.packages("GGally")
library(dplyr)
library(GGally)
data_a %>% select(TV,radio,newspaper,sales) %>% ggpairs()
model_2=lm(sales~TV+radio+newspaper,data=data_a)
summary(model_2)

install.packages("car")
library(car)
Anova(model_2)


view(data_a)


setwd("/Users/zoey/Desktop/DS Resources")
data_a
adv=read.csv("Advertising.csv", header=TRUE, stringsAsFactors = FALSE)
adv
model_4=lm(sales~TV+I(TV^2),data=adv)
summary(model_4)
plot(adv$ ,adv$sales)

cc=adv
cc[,2]=seq(0,300,length.out = 200)
dd=predict(model_4,cc)
lines(cc[,2], dd, col="red")

plot(model_4,1)
plot(model_4,2)
library(dplyr)
adv=adv %>% mutate(lnsales=log(sales))
install.packages("ISLR")
library(ISLR)
fix(Hitters)
Hitters=ISLR::Hitters
view(Hitters)
View(Hitters)
is.na()
summary(Hitters)
drop=which(is.na(Hitters[,19]))
new_H=Hitters[-drop,]

library(ggplot2)
ggplot(new_H,aes(x=Salary))+geom_histogram(bins=40,fill="red")+ggtitle("Hist of salary")+ylab("Frequency")
library(dplyr)
new_H=new_H %>% mutate(lnsalary=log(Salary))
ggplot(new_H,aes(x=lnsalary))+geom_histogram(bins=40,fill="green")+ggtitle("Hist of lnsalary")+ylab("Frequency")
library(GGally)
new_H %>% select(lnsalary,AtBat,Hits,HmRun,Runs,RBI) %>% ggpairs()
installed.packages("tidyverse")
library(tidyverse) 
install.packages("tidyverse")
library(tidyverse)  
install.packages("reshape2")  
library(reshape2)  
plotData <-melt(cor(new_H[sapply(new_H,is.numeric)]))
ggplot(plotData, aes(x=Var1, y=Var2,fill=value))+geom_tile()+ylab("")+xlab("")+scale_fill_gradient(low="#56B1F7",high="#132B43")+guides(fill=guide_legend(title= "Correlation"))
new_H=new_H[,-19]
install.packages("leaps")
library(leaps)
fit1=regsubsets(lnsalary~.,new_H,nvmax=20,method="exhaustive")
summary(fit1)$which
summary(fit1)$cp
summary(fit1)$bic
plot(summary(fit1)$cp,xlab="Num of predicts", ylab="CP",col="red")
plot(summary(fit1)$bic,xlab="Num of predicts", ylab="bic",col="green")
install.packages("revest")
install.packages("rvest")
library(rvest)
install.packages("xml2")
library(xml2)
library(rvest)  
lego=xml2::read_html("http://www.imdb.com/title/tt1490017/")
lego %>% 
  html_node("strong span") %>% 
  html_text() %>% 
  as.numeric()
install.packages("stringr")
library(stringr)
data=c('16 oct 150.00 (TSLA1607j150)','17 jan 234.00 (jd1709A234)')
str_detect(data,'TSLA')
str_subset(data,"TSLA")
str_extract(data,"^\\d\\d")
str_extract(data,"[[:alpha:]]+")
str_extract(str_extract(data,"\\([[:alpha:]]+"),'\\w+$')
str_extract(data,"\\d\\d\\d.\\d\\d")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm")
library(tm)
install.packages("NLP")
library(NLP)
library(tm)
install.packages("SnowballC")
library(SnowballC)
stopwords("english")
library(tm)
library(NLP)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
number1=c("one","two","three","four","five","six")
number2=1:6
cor.special=brewer.pal(6,"Dark2")
wordcloud(number1,number2,colors=cor.special,ordered.colors=F,random.order = F,random.color = F)
library(data.table)
setwd("/Users/zoey/Desktop/DS Resources")
data_all=fread("yelp_subset.csv",stringsAsFactors = F)
data=data_all[1:1000,]
data$rating=c(0)
data$rating[data$stars>=4]=1
data$rating=as.factor(data$rating)
table(data$rating)/nrow(data)
data_text=data$text
print(data_text[1:5])
library(tm)
library(SnowballC)
myc1=VCorpus(VectorSource(data_text))
myc2=tm_map(myc1,content_transformer(tolower))
myc3=tm_map(myc2,removeWords,stopwords("english"))
myc4=tm_map(myc3,removePunctuation) 
myc5=tm_map(myc4,removeNumbers)  
myc6=tm_map(myc5,stemDocument,lazy=TRUE)  
dtm1=DocumentTermMatrix(myc6)
as.matrix(dtm1[1,1:50])
dim(as.matrix(dtm1))  
threshold=0.05*length(myc6 )  
words_10=findFreqTerms(dtm1,lowfreq = threshold)  
length(words_10)
dtm_10=DocumentTermMatrix(myc6,control = list(dictionary=words_10))
data1_temp=data.frame(data,as.matrix(dtm_10))
data2_temp=data1_temp[,c(11,14:ncol(data1_temp))]
data2_temp$rating=as.numeric(data2_temp$rating)
View(data2_temp)
model1=lm(rating~.,data=data2_temp)
coefficient=model1$coefficients
positive_coeff=coefficient[which(coefficient>0)][-1]
name=names(positive_coeff)
cor_special=brewer.pal(8,"Dark2")
wordcloud(name,positive_coeff,colors=cor_special,ordered.colors = F)
install.packages("igraph")
library(igraph)
g1=graph(edges=c(1,2,2,3,3,1),n=3,directed=F)
plot(g1)
degree(g1)
transitivity(g1)
constraint(g1)
install.packages("devtools")

library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(ISLR)
Hitters=ISLR::Hitters
View(Hitters)
str(Hitters)
summary(Hitters)
newhitters=na.omit(Hitters)
dim(newhitters)
library(dplyr)
newhitters=newhitters %>% mutate(lnsalary=log(Salary))
newhitters=newhitters[,-19]
install.packages("glmnet")
library(glmnet)
x=newhitters[,20]
y=model.matrix(lnsalary~.,data=newhitters[,-1])
lasso1.10=glmnet(x,y,alpha=1,lambda=10)
library(devtools)
library(wordcloud2)
example=wordcloud2::demoFreq
wordcloud2(data=example)
















