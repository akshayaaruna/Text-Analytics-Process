#importing NLP based library
library(udpipe)

#Importing Dataset
data(brussels_listings,packages='udpipe')

#Viewing Dataset
View(brussels_listings)

#segragation a column
x<-table(brussels_listings$neighbourhood)
View(x)

#sorting
x<-sort(x)
View(x)

#IMPORTING TEXT VISUALIZATION LIBRARY
library(textplot)

#word frequency bar chart
textplot_bar(x,panel="location",col.panel="darkgrey",
             xlab="listings",cextext=0.75,addpct=TRUE,
             cexpct=0.5)

#IMPORTING DATASet
data(brussels_reviews_anno,package='udpipe')
View(brussels_reviews_anno)

#Segregation Data
y<-subset(brussels_reviews_anno,
          xpos %in%"NN"&language %in% "nl"& !is.na(lemma))
View(y)

#Document Term Frequency
y<-document_term_frequencies(y,document = "doc_id",term="lemma")
View(y)

#Document Term Matrix
dtm<-document_term_matrix(y)
dtm

#Removing Low frequency Words
dtm<-dtm_remove_lowfreq(dtm,maxterms = 60)
dtm

#Correlation Matrix
cor<-dtm_cor(dtm)
View(cor)

#Importing Libraries
library(glasso)#for graphical lasso:estimation of gaussian graphical

#word correlation plot
#textplot_correlation_glasso(cor,exclude_zero=TRUE)

#Word  cooccurrence graph
#segregating Data
w<-subset(brussels_reviews_anno,xpos %in% "JJ" & language %in%
            "fr")
View(w)

#cooccurring Terms
w<-cooccurrence(w,group="doc_id",term="lemma")
View(w)

#COocurring plot
textplot_cooccurrence(w,top_n=25,subtitle="showing only top 25")

#Dependency PARSING
#CREATING DATA
sentence="Hey friend,Welcome to the class."

#Tokenize and pos tag for each word in data
z<-udpipe(sentence,"english")
View(z)

#IMPORTING Relational Data VIsualization Library
library(ggraph)

#Dependency Parser Plot
textplot_dependencyparser(z)





        
    

