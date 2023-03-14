#IMMPORTING LIBRARIES
library(robotstxt)
library(rvest)

#scraping website
url<-  "https://www.espncricinfo.com/cricketers/axar-patel-554691"
  
#Allowability
  path=paths_allowed(url)

#HTML of the website
web=read_html(url)
View(web)

#SEgregate content
content<-web %>% html_nodes(".ci-player-bio-content p:nth-child(1)")%>%html_text()
View(content)

#lower case conversion
lowc=tolower(content)
View(lowc)

#upper case conversion
highc=toupper(content)
View(highc)

#character Replacement
chartr(old="a",new="A",lowc)

#splitting strings
spl=strsplit(highc,split="")
View(spl)

#Extract as list
spl[[1]]

spl2=unlist(strsplit(highc,split=""))
View(spl2)

#Accessing single element
spl2
spl2[1]
spl2[1000]

#Importing Library
library(stringr)

#length of string
str_length(content)
str_length(spl)
str_length(spl2)

#detecting string
str_detect(spl2,"B")
str_detect(spl2,"AT")

#detecting String Indexes
str_which(spl2,"AT")
spl2[2855]

#COUNTING MATCHES IN STRING
str_count(spl2,"A")

#fLATTENING STRINGS
a=str_flatten(spl," ")
View(a)

#converting to title format
str_to_title(lowc[1])

#converting to sentence
str_to_sentence(lowc[1])
