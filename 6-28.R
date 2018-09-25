install.packages(c("tm", "Matrix", "SnowballC","wordcloud", "lsa", "pdftools","qdapTools", "stringr"))

library(tm)
library(Matrix)
library(SnowballC)
library(wordcloud)
library(lsa)
library(pdftools)
library(qdapTools)
library(stringr)

filepath = "C:/Users/Hannan Qazi/Desktop/ClassDemonstration"
setwd(filepath)
dir(filepath)

file = "CBS.txt"
file =file(file, open= "r")
text.dec = readLines(file)

corpus_txt = Corpus( VectorSource(text.dec))
corpus_CBS = tm_map(corpus_txt, PlainTextDocument)
corpus_CBS = tm_map(corpus_CBS, tolower)
corpus_CBS = tm_map(corpus_CBS, removeNumbers)
corpus_CBS = tm_map(corpus_CBS, removePunctuation)
corpus_CBS = tm_map(corpus_CBS, stripWhitespace)
corpus_CBS = tm_map(corpus_CBS, stemDocument)

stopwords("english")
mystopwords = c("cbs", "watch" , "app","shows","show")
corpus_CBS_clean <- tm_map(corpus_CBS, removeWords, c(stopwords("english"),mystopwords))

corpus.tdm = TermDocumentMatrix(corpus_CBS_clean)
write.csv(as.matrix(corpus.tdm), file=file.path("tdm22.csv"))

freq <- rowSums(as.matrix(corpus.tdm))
freq <- sort(freq, decreasing = TRUE)
head(freq)

words <- names(freq)
wordcloud(words[1:40], freq[1:40], scale = c(2,0.8), colors = brewer.pal(8,"Dark2"))

corpus.tdm.weight=weightTfIdf(corpus.tdm,normalize=TRUE)
write.csv(as.matrix(corpus.tdm.weight),file=file.path("tdm_w22.csv"))

mydimensions=5

corpus.tm.weight.lsa=lsa(corpus.tdm.weight,dims = mydimensions)
summary(corpus.tm.weight.lsa)

tk=as.matrix(corpus.tm.weight.lsa$tk)
#corpus.tm.weigh.lsa$sk
sk=Diagonal(n=mydimensions,as.matrix(corpus.tm.weight.lsa$sk))
dk=as.matrix(corpus.tm.weight.lsa$dk)

#term loading
termloading=tk%*%sk
write.csv(as.matrix(termloading),file=file.path("term_loadingCBS.csv"))

docloading=dk%*%sk
write.csv(as.matrix(docloading),file=file.path("doc_loadingCBS.csv"))

y=corpus.tm.weight.lsa$sk
x=seq(1,mydimensions,1)
plot(x,y,xlab="numbers of factors",
     ylab="singular values",main="Let's go Home",
     ylim=c(0,10),xlim = c(0,50),pch=10,col="blue")

termloading.rotation=varimax(as.matrix(termloading),normalize=TRUE)

summary(termloading.rotation)
write.csv(termloading.rotation$loadings,file=file.path("termloadingrotationCBS.csv"))
