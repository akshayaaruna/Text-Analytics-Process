# Importing NLP based Library
library(udpipe) # For data prep in NLP

# Dependency Parsing
# Creating data
sentence = "Hey guys, Welcome to the class. Lets learn about text analytics"

# Tokenize and POS tag for each word in data
z <- udpipe(sentence, "english")
View(z)

# Importing Relational Data Visualization Library
library(ggraph)
library(textplot)
# Dependency Parser Plot
textplot_dependencyparser(z)

# Importing Required Library
require(readtext)

# Getting Data
data_mobydick <- text(readtext("http://www.gutenberg.org/cache/epub/2701/pg2701.txt"))
data_mobydick

# Renaming
names(data_mobydick) <- "Moby Dick"

# Lexical Dispersion Plot for Multiple Words in Single Document
textplot_xray(
  kwic(tokens(data_mobydick), pattern = "whale"),
  kwic(tokens(data_mobydick), pattern = "ahab")) 

# Importing Library
library(quanteda.textmodels) # For Scaling models and classifiers for sparse matrix objects representing textual data
library(quanteda.textplots)
library(quanteda)
# Importing Dataset
data(data_corpus_irishbudget2010, package = "quanteda.textmodels")

# Transform corpus to dfm
dt_dfm <- dfm(tokens(data_corpus_irishbudget2010))
dt_dfm

# Setting Reference Scores
refscores <- c(rep(NA, 4), 1, -1, rep(NA, 8))
refscores

# Predicting Wordscores Model
ws <- textmodel_wordscores(dt_dfm, y = refscores, smooth = 1)
ws

# Plot Estimated Word Positions
textplot_scale1d(ws, highlighted = c("minister", "have", "our", "budget"), 
                 highlighted_color = "red")

# Predictions
pred <- predict(ws, se.fit = TRUE)
pred

# Plot estimated document positions
# Group by "party"
textplot_scale1d(pred, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))

# Predictions by Rescaling based on lbg
pred_lbg <- predict(ws, se.fit = TRUE, rescaling = "lbg")
pred_lbg

# Plot estimated document positions based on LBG transformation
# Group by "party"
textplot_scale1d(pred_lbg, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))

# Estimate Wordfish model
wf <- textmodel_wordfish(dfm(tokens(data_corpus_irishbudget2010)), dir = c(6, 5))
wf

# Plot estimated word positions based on Wordfish model
textplot_scale1d(wf, margin = "features", 
                 highlighted = c("government", "global", "children", 
                                 "bank", "economy", "the", "citizenship",
                                 "productivity", "deficit"), 
                 highlighted_color = "red")

# Plot estimated document positions based on wordfish model
textplot_scale1d(wf, groups = data_corpus_irishbudget2010$party)


# Correspondence Analysis
ca <- textmodel_ca(dt_dfm)
ca

# Summary of Correspondence Analysis
summary(ca)

# Plot estimated positions based on Correspondence Analysis
# Group by "party"
textplot_scale1d(ca, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))