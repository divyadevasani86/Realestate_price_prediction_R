library(dplyr)			
library(car)

setwd("E:/vishnu_data_files/data")			
setwd("E:/vishnu_data_files/data")			
setwd("E:/vishnu_data_files/data")			

file_names=list.files(getwd(),	pattern = "data_dictionary.TXT")		
files=lapply(file_names,	read.csv,	header=F,	stringAsFactors = F)

train=read.csv("housing_train.csv",stringsAsFactors = F)		
test=read.csv("housing_test.csv",	stringsAsFactors = F)		

head(train)
test$Price= NA

train$data = 'train'
test$data = 'test'

View(train)			
var(train$Price)			
glimpse(train$SellerG)			
summary(train)

all= rbind(train,test)

glimpse(all)
all$Postcode=as.character(all$Postcode)

apply(all,2,function(x) length(unique(x)))

Price=na.omit(train)
Others=na.omit(test)

u=filter(Price, Type %in% c("h","t"))
View(u)

View(Price)			
View(Others)			

library(psych)			
describe(Price)			
describe(Others)

s=describe(Price)
t=describe(Others)

class(s)			
rownames(s)			
colnames(s)			

View(s)			

apply(Price,2,mean)	
glimpse(Price)			

table(Price$Type)			
table(Price$Postcode)			
var(Price$Price)			
View(table(Price$SellerG))			

library(ggplot2)			
p=ggplot(ld_Price,aes(x=Price,	y=SellerG))	
p			
p+geom_point()			
View(p+geom_point())			

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

all=all %>% 
  select(-SellerG,-Address,-Suburb)

head(all)
for_dummy_vars=c('Postcode','CouncilArea','Method','Type')

for(var in for_dummy_vars){
  all=CreateDummies(all,var,100)
}

for(col in names(all)){
  
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","Price"))){
    
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
  
}

head(all)

trainf = all %>% filter(data == 'train') %>% select(-data) 
testf= all %>% filter(data == 'test') %>% select(-Price, -data) 

any(is.na(trainf))
any(is.na(testf))

library(randomForest)
fit = randomForest(Price ~ ., data = trainf) 

library(rfUtilities)
train.predictions = predict(fit, newdata = trainf)

test.predictions = predict(fit, newdata = testf)

write.csv(test.predictions,file = "realestate_finalprediction.csv", row.names = F)
