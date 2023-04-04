library(tm)  #For Text Mining
library(ggplot2)#For visualization
library(lsa)

#Creating data
text<-c("transporting food by cars will cause global warming.so we should go local.",
        "We should try to convince our parents to stop using cars because it will cause global warming.",
        "some food,such as mango,requires a warm weather to grow.so they have to be transported to canada.",
        "a typical electronic circuit can be built with a battery, a bulb,and a switch.",
        "electricity flows from batteries to the bulb,just like watere flows through a tube.",
        "batteries have chemical energy in it.Then electrons flow through a bulb to light it up.",
        "birds can fly because they have feather and they are light.",
        "why some birds like pigeon can fly while some other like chicken cannot?",
        "feather is important for bird's fly.if feather on a bird's wings is removed,this bird cannot ")
  
text

#Factoring
View<-factor(rep(c("view 1","view 2","view 3"),each=3))
View

#CONVERTING IT TO DATAFRAME
df<-data.frame(text,View,stringsAsFactors = FALSE)
df

#preparing corpus
corpus<-Corpus(VectorSource(df$text))
corpus<-tm_map(corpus,tolower)
Corpus
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,function(x) removeWords(x,stopwords("english")))
corpus<-tm_map(corpus,stemDocument,language="english")
corpus

#TERM DOCUMENT MATRIX
td.mat<-as.matrix(TermDocumentMatrix(corpus))
td.mat
View(td.mat)

#Distance Matrix
dist.mat<-dist(t(as.matrix(td.mat)))
dist.mat

#Classical Multidimensional scaling
fit<-cmdscale(dist.mat,eig=TRUE,k=2)
fit

#converting to dataframe
points<-data.frame(x=fit$points[,1],y=fit$points[,2])
View(points)

#Distance Matrix Plot
ggplot(points,aes(x=x,y=y))+
   geom_point(data=points,aes(x=x,y=y,color=df$view))+
  geom_text(data=points,aes(x=x,y=y-0.2,label=row.names(df)))

#weighting
td.mat.lsa<-lw_bintf(td.mat)*gw_idf(td.mat)
View(td.mat.lsa)

#Lsa
lsaSpace<-lsa(td.mat.lsa)
lsaSpace

#COMPUTING DISTANCE MATRIX FOR LSA
dist.mat.lsa<-dist(t(as.textmatrix(lsaSpace)))
dist.mat.lsa

#CLASSICAL MULTIDIMENSIONAL SCALING FOR LSa
fit_lsa<-cmdscale(dist.mat.lsa,eig=TRUE,k=2)
fit_lsa

#converting to dataframe
points_lsa<-data.frame(x=fit$point[,1],y=fit$points[,2])
View(points_lsa)

#DISTANCE MATRIX PLOT LSA
ggplot(points_lsa,aes(x=x,y=y))+
  geom_point(data=points,aes(x=x,y=y,color=df$view))+
  geom_text(data=points,aes(x=x,y=y-0.2,label=row.names(df)))

#IMPORTING 3D VISUALIZATION LIBRARY
library(scatterplot3d)

#Classic Multidimensional scaling for lsa
fit2<-cmdscale(dist.mat.lsa,eig=TRUE,k=3)
fit2

colors<-rep(c("blue","green","red"),each=3)
colors
  
  #3D PLOT
  scatterplot3d(fit2$points[,1],fit2$points[,2],fit2$points[,3],
                color=colors,pch=16,main="semantic space scaled to 3d",
                xlab="x",ylab="y",zlab="z",type="h")
  
  

