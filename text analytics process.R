devtools::install_github("bradleyboehmke/harrypotter")

library(tidyverse)
library(stringr)
library(tidytext)
library(harrypotter)

#viewing raw data
View(philosophers_stone[1:2])

#Tibbling
text_tb<-tibble(chapter=seq_along(philosophers_stone),text=philosophers_stone)
View(text_tb)

#Applying Unnest Token
text_tb%>% unnest_tokens(word,text)
text_tb

#books names
titles<-c("Philosophers's stone","Chamber of Secerts","Prisoner of Azkaban","Goblet of Fire","Order of Phoenix","Half-Blood Prince","Deathly Hallows")
titles

#books list
books<-list(philosophers_stone,chamber_of_secrets,prisoner_of_azkaban,goblet_of_fire,order_of_the_phoenix,half_blood_prince,deathly_hallows)
View(books)

#tibbling & unnest tokens for all books
series<-tibble()

for(i in seq_along(titles)){
  clean<-tibble(chapter=seq_along(books[[i]]),text=books[[i]])%>%
    unnest_tokens(word,text)%>% mutate(book=titles[i])%>%
    select(book,everything())
  series<-rbind(series,clean)}
  View(clean)
  View(series)
  
#Setting books in order
  series$book<-factor(series$book,levels=rev(titles))
  View(series)

          