#IMPORTING LIBRARIES
library(tidyverse)
library(stringr)
library(tidytext)
library(harrypotter)
library(textdata)

#sentiment dataset
View(sentiments)

#lexicons
gets_sentiments("afinn")
gets_sentiments("bing")
gets_sentiments("nrc")

View(gets_sentiments("afinn"))
View(gets_sentiments("bing"))
View(gets_sentiments("nrc"))

#Books Name
titles<-c("Philosophers's stone","Chamber of Secerts","Prisoner of Azkaban","Goblet of Fire","Order of Phoenix","Half-Blood Prince","Deathly Hallows")
titles

#books list
books<-list(philosophers_stone,chamber_of_secrets,prisoner_of_azkaban,goblet_of_fire,order_of_the_phoenix,half_blood_prince,deathly_hallows)
View(books) 

#Tokening
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

#nrc sentiment for books
senti<-series%>% right_join(get_sentiments("nrc")) %>% filter(!is.na(sentiment))%>%
count(sentiment,sort=TRUE)
View(senti)

#Visualize sentiment across each book
series%>% group_by(book)%>%
  mutate(word_count=1:n(),index=word_count %/% 500+1)%>%
  inner_join(get_sentiments("bing"))%>%
  count(book,index=index,sentiment) %>% ungroup() %>%
 spread(sentiment,n,fill=0)%>%
  mutate(sentiment=positive-negative,
         book=factor(book,levels=titles)) %>%
  ggpplot(aes(index,sentiment,fill=book))+
  geom_bar(alpha=0.5,stat="identity",show.legend=FALSE)+
  facet_wrap(~book,ncol=2,scales="free_x")

#comparing sentiments
afinn<-series %>% group_by(book) %>% mutate(word_count =1:n(),
                                            index=word_count %/% 500+1)%>%
  inner_join(get_sentiments("afinn"))%>% group_by(book,index)%>%
  summaries(sentiment=sum(value))%>% mutate(method="AFINN")
View(afinn)

bing_and_nrc<-bind_rows(series %>%group_by(book) %>%
                          mutate(word_count=1:n(),index=word_count %/% 500+1)%>%
                          inner_join(get_sentiments("bing"))%>%
                          mutate(method="Bing"),series %>%group_by(book)%>%
                          mutate(word_count=1:n(),index=word_count %/% 500+1)%>%
                          inner_join(get_sentiments("nrc"))%>%
                          filter(sentiment %in% c("positive","negative"))) %>%
                          mutate(method="NRC"))%>%
                          count(book,method,index=index,sentiment) %>%
                         ungroup() %>% spread(sentiment,n,fill=0)%>%
                         mutate(sentiment=positive-negative) %>%
                         select(book,index,method,sentiment)
View(bing and nrc)

#Visualizing sentiment comparison
bind_rows(afinn,bing_and_nrc) %>% ungroup() %>%
  mutate(book=factor(book,levels=titles))%>%
  ggplot(aes(index,sentiment,fill=method))+
  geom_bar(alpha = 0.8,stat ="identity", show.legend=FALSE)+
  facet_grid(book~method)

#common sentiment words
bing_word_counts<-series %>%inner_join(get_sentiments("bing"))%>%
  count(word,sentiment,sort=TRUE)%>%ungroup()
View(bing_word_counts)

#Visualize top 10 bing words
bing_word_counts %>% group_by(sentiment) %>%top_n(10) %>%
  ggplot(ase(reorder(word,n),n,fill=sentiment))+
  geom_bar(alpha =0.8,stat="identity",show.legend=FALSE)+
  facet_wrap(~sentiment,scales="free_y")+
  labs(y="CONTRIBUTION TO SENTIMENT",X=NULL)+coord_flip()


  
                          
                          
                          

                                 



