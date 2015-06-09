#building dictionary
positive<-c("party","chillin","congrats","great","cool","sunshine","passion","relax","launch","enjoying","fantastic","success","cute","thank","Gorgeous","beautiful","inspire","cheaper","cheapest","cheap travel","cheap flight","check this out","cheap airfare","best","good","attraction","beauty","love","photogenic","luxury","low fare","low prices","celebrate","happy")
negative<-c("nostagia","wtf","sad","poor","bad","accident","worst","worse","fraud","chaos","strikes","fuck","dead","death","costly","broken","kill","delay","cancel","sorry","crash","attack")

tweets<-read.csv("file_name",header=F)
tweets<-as.data.frame(tweets[start_row:end:row,])
tweets<-mutate(tweets,sentiment="e")
colnames(tweets)<-c("sentence","sentiment")


count=1
for(i in tweets$sentence)
{
  if(grepl(paste(positive,collapse="|"),ignore.case = T,i))
    tweets$sentiment[count]<-"p"
  else if(grepl(paste(negative,collapse="|"),ignore.case = T,i))
    tweets$sentiment[count]<-"n"
  else
    tweets$sentiment[count]<-"m"
  count<-count+1
}


#Creating df of HTTP containing rows

withhttp<- data.frame(sentence=character(),sentiment=character(),serial=numeric()) 
count=1
for(i in tweets$sentence)
{
  if(grepl("http",i))
  {
    withhttp<-rbind(withhttp,tweets[count,])
  }
count<-count+1
}
withhttp$sentence<-as.character(withhttp$sentence)
rowNumbers<-as.data.frame(as.numeric(row.names(withhttp)))
colnames(rowNumbers)<-c("RowNum")
#Removing hyperlinks from rows so that duplicacy can be identified.
count=1
for(i in withhttp$sentence)
{
  strings<-strsplit(i,split=c("http","https"))
  withhttp$sentence[count]<-strings[[1]][1]
  count<-count+1
}
colnames(withhttp)<-c("sentence","IsDuplicate")
withhttp$IsDuplicate<-"nd"


#Marking duplicates as d
for(i in 1:(length(withhttp$sentence)-1))
{
  if(identical(withhttp$sentence[i],withhttp$sentence[i+1]))
    withhttp$IsDuplicate[i+1]<-"d"
}

withhttp<-cbind(rowNumbers,withhttp)
reqRows<-filter(withhttp,IsDuplicate=="d") %>% select(RowNum)

for(i in 1:length(reqRows$RowNum))
{
  tweets$sentiment[reqRows$RowNum[i]]<-"d"
}

#Writing result to a file.
write.csv(x=tweets,file = "result.csv",row.names = F)
