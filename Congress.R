path <- "F:/dataviz_data/Congress Gov_2012"
filenames <- paste0(path, "/",list.files(path, pattern = "json"))
library(rjson)

mydata = lapply(filenames, function(t) {
  dum=fromJSON(paste(readLines(t), collapse=""))
  dum$filenames = t
  return(dum)})


example=mydata[[1]]
name=names(example[[1]])

d=list()
for (q in c(1:213)){
  d[[q]]=data.frame()
   for (i in c(1:length(mydata[[q]]))){ 
     for (j in c(1:16)){
       if (length(unlist(mydata[[q]][[i]][j]))==1){
        d[[q]][i,j]=unlist(mydata[[q]][[i]][j])
         }else{
        d[[q]][i,j]=paste(mydata[[q]][[i]][j],collapse="@")
         }
   }
 }
}

library(data.table)
final.raw=rbindlist(d)
setnames(final.raw,name)
final.raw=as.data.frame(final.raw)

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
