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
unique(length)   #7,8,9,10

#####test on a short example with all possible situations####
#############################################################
example<-final.raw$source[c(which(length==7)[1],which(length==8)[1],which(length==9)[1],which(length==10)[1])]
class(example)
View(example)
x<-str_split(example,pattern="\",")    #we have sublist of either length 6 or 7 
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
class(x)
View(x)

#reformat the data, so that each column is a tag
x<-sapply(1:nrow(x),function(i)x[i,])

#split the columns that have two variables
#but before that, pick out those that are completely missing
x[,1]<-unlist(str_extract_all(x[,1],"name = .*"))
new<-ldply(str_split(x[,3],pattern=", "),function(i)data.frame(s3=i[1],s4=i[2]))
x<-cbind(x[,1:2],new,x[,5:7])
View(x)

#change column names
names(x)<-c("name","locationName","current","government","orgType","stateDelegation","gender")

#remove name tags in the content
x.1<-str_replace_all(as.matrix(x),"[[:alpha:]]+ = ","")
View(x.1)
#further cleaning, remove \",), and leading and trailing white spaces
x.1<-str_replace_all(x.1,"\"","")
x.1[,7]<-str_replace_all(x.1[,7],")","")
x.1<-str_trim(x.1,side="both")

#Now this is clean
View(x.1)

#####apply to the whole dataset ####
#############################################################

s<-final.raw$source
length(s)
class(s)
View(s)
s.1<-str_split(s,pattern="\",")    #we have sublist of either length 6 or 7 
class(s.1)

#deal with different length
s.2<-sapply(s.1,FUN=function(i){
  i.v<-unlist(i)
  if (length(i.v)==6){
    i.v<-c(i.v,i.v[6])
    i.v[6]<-NA
  }
  return(i.v)
})
class(s.2)   #weired now it's a list


#reformat the data, so that each column is a tag
get.source.info <- function(object){
  data.frame(s1=object[1],s2=object[2],s3=object[3],s5=object[4],s6=object[5],
             s7=object[6],s8=object[7])
}

require(plyr)
s.3 <- ldply(s.2,get.source.info)
head(s.3)   #wonderful!!!!

#split the columns that have two variables
s.3[,1]<-unlist(str_extract_all(s.3[,1],"name = .*"))
new<-ldply(str_split(s.3[,3],pattern=", "),function(i)data.frame(s3=i[1],s4=i[2]))
s.4<-cbind(s.3[,1:2],new,s.3[,5:7])
head(s.4)

#change column names
names(s.4)<-c("name","locationName","current","government","orgType","stateDelegation","gender")

#remove name tags in the content
s.5<-str_replace_all(as.matrix(s.4),"[[:alpha:]]+ = ","")
head(s.5)
#further cleaning, remove \",), and leading and trailing white spaces
s.6<-str_replace_all(s.5,"\"","")
s.6[,7]<-str_replace_all(s.6[,7],")","")
s.7<-str_trim(s.6,side="both")
s.8<-as.data.frame(s.7)
#Now this is clean
View(s.8[1:10,])

save(s.8,file="source.Rda")

load("source.Rda")
View(s.8[1:20,])

######---clean text column-----####
expression="<[^>]*.>"
clean=function(x){
  t=gsub(expression,"",x)
  t2=gsub("(\n)","",t)
  return(t2)
}
text=sapply(final.raw[,13],clean)
length(text)
text[1:2]
names(text)<-"text"
text1<-as.data.frame(text)

######---clean other column-----####
example<-final.raw$keywords[1:10]
k.1<-str_replace_all(example,"c\\(","")
k.2<-str_replace_all(k.1,")","")
k.3<-str_replace_all(k.2,"\"","")
View(k.3)

cleanother<-function(x){
  x<-str_replace_all(x,"c\\(","")
  x<-str_replace_all(x,")","")
  x<-str_replace_all(x,"\"","")
}

names(final.raw)
pp<-sapply(final.raw[,c("keywords","names","organisations","places")],cleanother)
View(pp)
head(pp)
pp<-as.data.frame(pp)
names(pp)<-c("keywords","names","organisations","places")
or<-str_replace_all(pp$organisations,"^list\\($","")
pp$organisations<-or

#####combine cleaned data with the original dataset
View(final.raw[1:6,])
names(final.raw)

final.raw1<-final.raw[,c(1,14,15)]
final.raw2<-cbind(final.raw1,s.8,pp)

save(final.raw2,file="final_clean.Rda")

##-----clean key words-----##

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
