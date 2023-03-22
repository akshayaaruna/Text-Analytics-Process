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

#IMPORTING Relational Data Visualization Library
library(ggraph)

#Dependency Parser Plot
textplot_dependencyparser(z)



require(readtext)

data_mobydick<-texts(readtext("http://www.gutenberg.org/cache/epub/2701/pg2701.txt"))
data_mobydick

names(data_mobydick)<-"Moby DICK"

textplot_xray(
  kwic(tokens(data_mobydick),pattern="whale"),
  kwic(tokens(data_mobydick),pattern="ahab"))

library(qyanteda.textmodela)

data(data_corpus_irishbudget2010,package="quanteda.textmodels")

dt_dfm<-dfm(tokens(data_corpus_irishbudget2010))
dt_dfm

refscores<-c(rep(NA,4),1,-1,rep(NA,8))
refscores

ws<-textmodel_wordscores(dt_dfm,y=refscores,smooth=1)
ws

textplot_scaleld(ws,highlighted=c("minister","have","our","budget"),
                 highlighted_color="red")

pred<-predict(ws,se.fit=TRUE)
pred

textplot_scaleld(pred,margin="documents",
                 groups=docvars(data_corpus_irishbudget2010,"party"))

pred_lbg<-predict(ws,se.fit=TRUE,rescaling="lbg")
pred_lbg

textplot_scaleld(pred_lbg,margin="documents",
                 groups=docvars(data_corpus_irishbudget2010,"party"))

wf<-textmode_wordfish(dfm(tokens(data_corpus_irishbudget2010)),dir=c(6,5))
wf

textplot_scaled(wf,margin="features",
                highlighted=c("government","global","children",
                              "bank","economy","the","citizenship",
                              "productivity","deficit"),
                highlighted_color="red")

textplot_scleld(wf,groups=data_corpus_irishbudget2010$party)

ca<-textmodel_ca(dt_dfm)
ca

summary(ca)

textplot_scaleld(ca,margin="documents",
                 groups=docvars(data_corpus_irishbudget2010,"party"))


