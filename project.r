a<-read.csv("PROJECT.csv")

place<- function(){
  print("Enter the region you want to dine today")
  print("1. North Delhi")
  print("2. South Delhi")
  print("3. East Delhi")
  print("4. West Delhi")
  print("5. Central Delhi")
  q=scan()
  if(q==1){
    w<- a%>%filter(District== "NORTH DELHI")
    w
  }else if(q==2){
    w<- a%>%filter(District== "SOUTH DELHI")
    w
  }else if(q==3){
    w<- a%>%filter(District== "EAST DELHI")
    w
  }else if(q==4){
    w<- a%>%filter(District== "WEST DELHI")
    w
  }else if(q==5){
     w<- a%>%filter(District== "CENTRAL DELHI")
     w
    }
}

cusine<-function(r)
{
  print("Enter the cuisine you want to have today")
  print(" 1. NORTH INDIAN")
  print(" 2. CHINESE")
  print(" 3. AMERICAN")
  print(" 4. ITALIAN")
  c<-scan()
  if(c==1){ 
    e<-r%>%filter(CUISINE=="NORTH INIDAN")
    e
  }        
  else if(c==2){ 
    e<-r%>%filter(CUISINE=="CHINESE")
    e
  }
  else if(c==3){ 
    e<-r%>%filter(CUISINE=="AMERICAN")
    e
  }        
  else if(c==4){ 
    e<-r%>%filter(CUISINE=="ITALIAN")
    e
  }
  else{
    print("choice not available")
  }
  
}


stars<-function(t)
{
  print("Enter the range of stars you want to dine in")
  print(" 1. 2 STAR AND BELOW")
  print(" 2. 3 STAR AND BELOW")
  print(" 3. 3-4 STARS")
  print(" 4. 4-5 STARS")
  print(" 5. 5 STARS")
  y<-scan()
  if(y==1){ 
    u<-t%>%filter(STARS<=2)
    u
  }else if(y==2){ 
     u<-t%>%filter(STARS<=3)
     u
  }else if(y==3){ 
    u<-t%>%filter(STARS>=3 & STARS<=4)
    u
  }else if(y==4){ 
    u<-t%>%filter(STARS>=4 & STARS<=5)
    u
  }else if(y==5){ 
    u<-t%>%filter(STARS==5)
    u
  }else{
    print("choice not available")
  }
  
}
review<-function(){
  print('Enter the restaurant whose review you want to see')
  ch=scan(what='character')
  fg1=".txt"
  ch123=paste(ch,fg1)
  ch123=gsub(" ", "", ch123, fixed = TRUE)
  tr1=readLines(ch123)
  tr1
  tr2=paste(tr1,collapse = " ")
  tr2
  tr2=gsub(pattern = "\\W",replace=" ",tr2)
  tr2
  tr2=gsub(pattern = "\\d",replace=" ",tr2)
  tr2
  tr2=tolower(tr2)
  tr2
  tr2=removeWords(tr2,stopwords())
  tr2
  tr2=gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",tr2)
  tr2
  tr2=stripWhitespace(tr2)
  tr2
  tb1=str_split(tr2,pattern = "\\s+")
  tb1
  tb1=unlist(tb1)
  wordcloud(tb1,random.order = FALSE,min.freq = .45,col=rainbow(7))
  print("Do you want a sentiment analysis? If yes, press y. If no, press n")
  o=scan(what='character')
  if(o=='y'|| o=='Y' ){
    pw1=scan("positive-words.txt",what = 'character',comment.char = ";")
    pw1
    nw1=scan("negative-words.txt",what = 'character',comment.char = ";")
    nw1
    positive=sum(!is.na(match(tb1,pw1)))
    negative=sum(!is.na(match(tb1,nw1)))
    match(tb1,pw1)
    score=positive-negative
    if(score>0){
      print(paste("It is a positive review and score is:",score))
    }else{
      print(paste("It is a negativereview and score is:",score))
    }
    sentiments=cbind(positive,negative)
    sentiments
    barplot(sentiments,col= c("sky blue"),beside= TRUE, legend=TRUE,ylim=c(0,40),xlim=c(0,10), xlab= "Positive & Negative Review", ylab= "Score", main= "Sentiment Analysis")
    abline(h=0)
  
    
  }else(o=='n' || o== 'N') 
  {
     
  }
}
compare<-function(){
  print("Do you want to compare 2 restaurants?")
  ch12=scan(what='character')
  i <- 1
  flag=0
  if(ch12 == 'y'|| ch12 == 'Y') {
    print("Enter the names of the restuarant to be compared")
    l <- list()
    new_element=scan(what='character')
    l <- new_element
    i<-i+1
    flag<-flag+1
    
  }else{
    
  }
  
  fg1=".txt"
  ch124=paste(l,fg1)
  ch124=gsub(" ", "", ch124, fixed = TRUE)
  print(ch124)
  mtext12=lapply(ch124,FUN=readLines)
  mtext12
  mtext21=lapply(mtext12,FUN=paste,collapse=" ")
  mtext21
  mtext21=gsub(pattern = "\\W",replace= " ",mtext21)
  mtext21
  mtext21=gsub(pattern = "\\d",replace= " ",mtext21)
  mtext21
  mtext21=tolower(mtext21)
  mtext21
  mtext21=removeWords(mtext21,stopwords())
  mtext21
  mtext21=gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",mtext21)
  mtext21
  mtext21=stripWhitespace(mtext21)
  mtext21
  tb123=str_split(mtext21,pattern = "\\s+")
  tb123
  tb123=unlist(tb123)
  #wordcloud(tb123,random.order = FALSE,min.freq = .45,col=rainbow(4))
  Corpus21=Corpus(VectorSource(mtext21))
  print(Corpus21)
  tdm234=TermDocumentMatrix(Corpus21)
  print(tdm234)
  mat12=as.matrix(tdm234)
  mat12
  colnames(mat12)
  # colnames(mat12)=c(l)
  mat12
  comparison.cloud(mat12)
  mtext321=str_split(mtext21,pattern="\\s+")
  mtext321
  print("\n")
  print("Do you want a sentiment analysis? If yes, press y. If no, press n")
  o=scan(what='character')
  if(o=='y'|| o=='Y' ){
    posword=scan("positive-words.txt",what = 'character',comment.char = ";")
    negword=scan("negative-words.txt",what = 'character',comment.char = ";")
    positive=unlist(lapply(mtext321,function(x){sum(!is.na(match(x,posword)))}))
    negative=unlist(lapply(mtext321,function(x){sum(!is.na(match(x,negword)))}))
    score1=positive-negative
    paste("The score is: ", score1)
    sentiments=cbind(positive,negative)
    sentiments
    barplot(sentiments,col=rainbow(2),beside=TRUE,ylim=c(0,40),xlim=c(0,10),las=1)
    legend("topright",title="Sentiments" ,fill=rainbow(2),c("Restuarant 1","Restuarant 2"))
    abline(h=0)
    title(xlab="review-positive vs negative",ylab="Score",main="Sentiment Analysis")
  }else{
    
  }
}




repeat{
 print('Welcome')
 print('please select your category')
 print('1. Recommendations of restuarants')
 print('2. Reviews of restuarants of your choice')
 print(" Enter your choice")
 c=scan()
 if(c==1){
  r = place()
  print(r)
  t= cusine(r)
  print(t)
  i=stars(t)
  print(i)
  
  
  }else if(c==2){
  review()
  compare()
  }
}
