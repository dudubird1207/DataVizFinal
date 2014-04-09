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

#####-----------the rest is just for checking-------------#####

final.raw[200000,]




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
