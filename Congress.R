########DataViz Final Projects#########

rm(list=ls())
if(!require(rjson)){
  install.packages("rjson")
  library(rjson)
}
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}

#####---------convert json file into data frame--------#####

#for read in data, generate all the paths
path <- "C:/Users/Rongyao/Documents/GitHub/DataVizFinal/Release_2012"
filenames <- paste0(path, "/",list.files(path, pattern = "json"))

#extract the release date from file name
Release<-list.files(path,pattern="json")
Release<-sapply(Release,function(x)str_sub(x,13,20))

#read in json file, convert into list
mydata = lapply(filenames, function(t) {
  dum=fromJSON(paste(readLines(t), collapse=""))
  return(dum)}) 

#convert each release file into a data frame with tags as columns and articles as rows
#the date information extracted previously will be appended to the end of the columns
#the resulting list will have 366 elements, each of which a data frame  
d=list()
for (q in c(1:366)){
  d[[q]]=data.frame()
  date<-Release[q]
   for (i in c(1:length(mydata[[q]]))){ 
     for (j in c(1:14)){
       if (length(unlist(mydata[[q]][[i]][j]))==1){
        d[[q]][i,j]=unlist(mydata[[q]][[i]][j])
         }else{
        d[[q]][i,j]=paste(mydata[[q]][[i]][j],collapse="@")
         }
    }
    d[[q]][i,15]<-date
 }
}

#stack all the list together
#the resulting data frame will have each article has a row and tags as columns
#tag names are added as column name
final.raw=rbindlist(d)
name=c(names(mydata[[1]][[1]]),"date")
setnames(final.raw,name)
final.raw=as.data.frame(final.raw)

#save the data frame
save(final.raw,file="final_raw.Rda")

#####---------clean the data frame--------#####

load("final_raw.Rda")
str(final.raw)
View(final.raw[6:10,])

#check the number of tags in each source column
length<-numeric()
for (i in 1:nrow(final.raw)){
  length[i]<-length(str_locate_all(final.raw[i,"source"],",")[[1]][,1])
}
unique(length)   #0 7 8 9 10 

final.raw[which(length==10)[1],]
length(which(length==0))
#Source<-data.frame(nrow=20,ncol=11)
#for (i in 1:nrow(final.raw)){
#  index<-c(1,str_locate_all(final.raw[i,"source"],",")[[1]][,1],nchar(final.raw[i,"source"])+1)
#  if (length(index)==12){
#    for (j in 1:11){
#      Source[i,j]<-str_sub(final.raw[i,"source"],index[j]+1,index[j+1]-1)
#    }
#  }else if(length(index)==11){
#    for (j in 1:10){
#      Source[i,j]<-str_sub(final.raw[i,"source"],index[j]+1,index[j+1]-1)
#      Source[i,11]<-""
#    }     
#  }else if(length(index)==10){
#    for (j in 1:9){
#      Source[i,j]<-str_sub(final.raw[i,"source"],index[j]+1,index[j+1]-1)
#      Source[i,10:11]<-""                                   
#    }
#  }else if(length(index)==9){
#    for (j in 1:8){
#      Source[i,j]<-str_sub(final.raw[i,"source"],index[j]+1,index[j+1]-1)
#      Source[i,9:11]<-""
#    }
#  }else{
#    Source[i,1]<-str_sub(final.raw[i,"source"],index[1]+1,index[1+1]-1)
#    Source[i,2:11]<-""
#  }
#}

#test on a short example
example<-final.raw$source[c(which(length==10)[1],which(length==9)[1],which(length==8)[1],which(length==7)[1],which(length==0)[1])]
class(example)
x<-str_split(example,pattern="\",")
class(x)

#deal with different length
x<-sapply(x,FUN=function(i){
  i.v<-unlist(i)
  if (length(i.v)==6){
    i.v<-c(i.v,i.v[6])
    i.v[6]<-NA
  }
  return(i.v)
})

get.source.info <- function(object){
  data.frame(s1=object[1],s2=object[2],s3=object[3],s5=object[4],s6=object[5],
             s7=object[6],s8=object[7])
}

require(plyr)
source.split <- ldply(x,get.source.info)
View(source.split)

#split the columns that have two variables
emptyindex<-which(is.na(source.split$s1=="NA"))
source.split<-source.split[-emptyindex,]
source.split$s1<-unlist(str_extract_all(source.split$s1,"name = .*"))

View(source.split)
new<-ldply(str_split(source.split$s3,pattern=", "),function(x)data.frame(s3=x[1],s4=x[2]))
source.split<-cbind(source.split[,1:2],new,source.split[,5:7])

names(source.split)<-c("name","locationName","current","government","orgType","stateDelegation","gender")

x<-str_replace_all(as.matrix(source.split),"[[:alpha:]]+ = ","")
x<-str_replace_all(x,"\"","")
x[,"gender"]<-str_replace_all(x[,"gender"],")","")
x<-str_trim(x,side="both")
View(x)


















vect <- c("Model: Cadilac,Year: 2014", "Model: GMC,Year: 2013,Color: red", "Model: Volt,Year: 2014")

vectList <- str_split(vect, pattern=",")
vectList

rep(NA, 5-length(x))



#
#########------------------------------------------------
final.raw.save<-final.raw
final.raw<-final.raw[1:10,]
Source<-data.frame()
for (i in 1:nrow(x)){
  index<-c(5,str_locate_all(x[i,"source"],",")[[1]][,1],nchar(x[i,"source"]))
 
    for (j in 1:11){
      Source[i,j]<-str_sub(x[i,"source"],index[j]+1,index[j+1]-1)
    }
}

index<-c(5,str_locate_all(x[1,"source"],",")[[1]][,1],nchar(x[1,"source"]))

str_sub(x[1,"source"],index[])  
    
    
  for (j in 1:length(index))
}

for (i in 1:length(index)){
  <-str_sub(example[1,"source"],6,index[i]-1)
}

length<-numeric()
for (i in 1:nrow(final.raw)){
  length[i]<-length(str_locate_all(final.raw[i,"source"],",")[[1]][,1])
}
unqiue(length)





#####-----------the rest is just for checking-------------#####


ff=list()
for (q in c(1:213)){
  ff[[q]]=data.frame()
  for (i in c(1:length(mydata[[q]]))){ 
    for (j in c(1:3)){
    ff[[q]][i,j]=unlist(mydata[[q]][[i]][15])[j]
   
    
  }
  ff[[q]][i,4]=unlist(mydata[[q]][[i]][15])[length(unlist(mydata[[q]][[i]][15]))]
}
}
metadata=rbindlist(ff)
metadata=data.frame(metadata)
clean.data=function(x){
  x=gsub("<[^>]*>",",",x)
  x=gsub("\\/|[0-9]|\\:|;","",x)
}
final.refine=apply(final.raw,c(1,2),clean.data)
party=sapply(final.refine[,11],function(x)strsplit(x,","))
meta2=sapply(c(2:7),function(x)sapply(c(1:8813),function(y)(party[[y]][x])))
metadata.new=cbind(final.refine[,10],meta2)
metadata.new[,c(2:7)]=apply(metadata.new[,c(2:7)],c(1,2),function(x)unlist(strsplit(x,"="))[2])

##metadata.refine=apply(metadata,c(1,2),clean.data)
metadata.refine[,3]=sapply(metadata.refine[,3],function(x)gsub("[[:alpha:]]","",x))
metadata.refine[,4]=sapply(metadata.refine[,4],function(x)gsub("\\/|[0-9]|\\:|;","",x))

library(stm)
temp<-textProcessor(documents=final.refine[,13],metadata=final.refine)
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab,meta)
docs<-out$documents
 vocab<-out$vocab
meta <-out$meta
CongressPrevFit <- stm(docs,vocab,K=5,prevalence=~meta[,11])
library(lda)
length(poliblog.documents)
length(poliblog.vocab)
