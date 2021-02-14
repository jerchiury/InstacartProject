setwd('C:/Users/Jerry/Desktop/Jerry/projects/instacart')
library(ggplot2)
library(dplyr)
library(reshape2)
library(textclean)
library(textstem)
library(stringr)
library(stopwords)
library(tm)
library(openNLP)
library(wordcloud)
library(arules)
library(cluster)
library(treemap)
library(RColorBrewer)
library(ngram)
library(data.table)
library(plotly)
library(ggrepel)
library(textmineR)
library(factoextra)
library(NbClust)
library(dbscan)
library(fpc)
library(tidyr)
library(tidyverse)
library(dtw)
library(grid)
library(gridExtra)

set.seed(1)

aisles=read.csv('aisles.csv')
departments=read.csv('departments.csv')
products=read.csv('products.csv')
orders=read.csv('orders.csv')
order_pp=read.csv('order_products__prior.csv')

products=read.csv('products_clean.csv')
products.clust=read.csv('products_clust.csv')
clusters=read.csv('cluster.csv')
aisle.dep=read.csv('aisle_dep.csv')
order_size=read.csv('order_size.csv')
#################################### cleaning and exploring ###########################################

#########################check data integrity
white.space=function(data){
  return(sum(grepl('^[ ]*$',data)))}
##### aisles
colSums(is.na(aisles)) #0
sapply(aisles, white.space) #0

##### departments
colSums(is.na(departments)) #0
sapply(departments, white.space) #0

##### orders
colSums(is.na(orders)) #206209, all from days_since_prior_order
sapply(orders, white.space) #0
# check what is causing the problem,
head(orders[which(is.na(orders$days_since_prior_order)),])
# we see that the order_id is quite random, but the user_id goes up and is in order, and order_number is always 1
# let's guess that the relationship is that days_since_prior_order is NA exactly when order_number is 1
nrow(filter(orders, is.na(days_since_prior_order), order_number!=1))
# we get nothing such that order number is not 1 and days_since_prior_order is NA, confirming the conjecture

##### products
colSums(is.na(products)) #0
sapply(products, white.space) #0

################################### explore data ################################

#--------------------------------- aisles -----------------------------------
str(aisles) # aisles_id looks like just counting integers from 1 to 134, aisle is characters
head(aisles) # aisle 6 looks peculiar as it is labeled "other"
# let's see if any aisle number is repeated
sum(duplicated(aisles$aisle_id)) #0, therefore aisle_id is unique 

#--------------------------------- departments ---------------------------------
str(departments) # about the same as aisles, just less rows
head(departments) # department 2 is peculiar as it is labeled "other"
# let's see if any dep number is repeated
sum(duplicated(departments$department_id)) #0, therefore department_id is unique

#--------------------------------- orders ---------------------------------
str(orders) #eval_set is character and everyting else looks like integers
# let's go in further and check each column
#------------------order_id, lets see if there are any repeated ones
sum(duplicated(orders$order_id)) #0, order_id is unique

#------------------ user_id, we see from the str that user_id is repeated, my hunch is that (user_id, order_number) form an unique ordered pair,
# meaning order_number is tied to user_id
sum(duplicated(orders[,c('user_id','order_number')])) #0, my hunch was right, each user has a set of unique order_numebrs

#------------------ eval_set, character, let's see its table
table(orders$eval_set) # looks like 3 levels, prior, test, and train
# as described in the data set description, the orders are split into prior orders, test orders, and train orders, 
# the number of orders in each set agrees with the numbers in the table. Not much else here

#------------------ order_number, we already know that order_number is unique for each user_id, but are the numbers consecutive?
# we can check it by checking the range of the order_numbers for each user and compare it with the number of orders
test=orders%>%group_by(user_id)%>%summarise(eq=(max(order_number)==n()))
all(test$eq) # all true!, that means that the max value agrees with the number of orders,
range(orders$order_number) # only positive order numbers
# along with knowing that order_number is unique per user, this mean that the order numbers start at 1 and increases without gaps
# lets check the stats for number of orders for each user
test=orders%>%group_by(user_id)%>%summarise(max=max(order_number))
summary(test$max) # minimum of 4 orders, max of 100 orders! avg of 16.59
sd(test$max/mean(test$max)) #1.0038, pretty decent variation
hist(test$max) # looks like a small outlier of supershoppers with 100 orders

#------------------ order_dow is described as being day of week, lets check it
range(orders$order_dow)# 0 to 6, 
# a little histogram will show us the stats
hist(orders$order_dow) # looks like day of week 0 and 1 are the most popular
# does the avg user order at a particular dow?
# let's calculate the max freqs
test1=orders%>%group_by(user_id, order_dow)%>%summarise(n=n())
test=test1%>%group_by(user_id)%>%summarize(max=1/max(n/sum(n)))
summary(test$max) #mean of 2.923
hist(test$max)
# the result is such that the avg user shops about 3 main days of the week, reasonably spread

#------------------ order_hour_of_day is the hour of day the order is placed, lets check
summary(orders$order_hour_of_day) # 0 to 23
hist(orders$order_hour_of_day) # peak shopping around 10 to 15
# do ppl have a tendency to order at certain time of day?
test=orders%>%group_by(user_id)%>%summarise(mean=mean(order_hour_of_day), sd=sd(order_hour_of_day))
summary(test$mean) #mean of 13.59. about 1 oclock
hist(test$mean)
summary(test$sd) # 3.631
hist(test$sd) # 3.5
# the avg user usually orders within 3.5 hours of previous orders, somewhat consistent

orders.heat=orders%>%group_by(order_dow, order_hour_of_day)%>%summarise(n=n())

ggplot(data=orders.heat)+
  geom_tile(aes(x=order_dow,y=order_hour_of_day, fill=n))+
  scale_fill_gradient(high='red', low='green')


#------------------ days_since_prior_order is self explanatory. we already know from cleaning step that the NA's are tied to each user's 1st order
# let's check other things such as range and avg days, etc
summary(orders$days_since_prior_order, na.rm=T) 
# 0 to 30 days it seems, either there is a hard limit on max days or these shoppers are quite good at keeping up shopping every 30 days
# mean days of 11.11
# let's see if 30 days is a hard limit or just some very regular shoppers
hist(orders$days_since_prior_order) # an odd spike at 30, 
median(filter(orders, days_since_prior_order!=30)$days_since_prior_order, na.rm=T) # median of 7 days! a week!
# if the system allows monthly re-orders, then we would expect some months to be 31 days, looks like the hard cap is 30
# do ppl have a tendency to order in certain day intervals?
no30=filter(orders, days_since_prior_order!=30 & !is.na(days_since_prior_order)) #removing 30 days since we think it is hard cap, and NA
# we look at each user and their tendencies
test=no30%>%group_by(user_id)%>%summarise(mean=mean(days_since_prior_order, na.rm=T), sd=sd(days_since_prior_order, na.rm=T)/mean)
hist(test$mean)
summary(test$mean) # median days of about 11 days
hist(test$sd)
summary(test$sd) # sd median of about 6 days, quite spread. 
test=hist(no30$days_since_prior_order)
plot(test$mids, log(test$count), type='h', lwd=15, lend=2)
#--------------------------------- products ---------------------------------
#---------------- the first interest here is the relationship between aisle and department, let's see what it is
# hunch is that each aisle belong to only 1 dep
aisle.dep=unique(products[c('aisle_id','department_id')])
length(which(duplicated(test$aisle_id))) # there are no duplicated aisle_id, therefore each aisle is within only 1 dep
# since we know that each aisle is only in 1 dep, lets do some tree map to visualize it
test=products%>%group_by(department_id, aisle_id)%>%summarise(n=n())
test$department_id=departments[test$department_id,'department']
test$aisle_id=aisles[test$aisle_id,'aisle']
png(filename="aisle_dep_treemap.png",width=1920, height=1080)
treemap(test, index=c('department_id', 'aisle_id'), vSize='n', title='Departments and Aisles', overlap.labels = 1, fontsize.title = 24, fontsize.labels=c(20,15), ymod.labels=c(0.3,0))
dev.off()

#---------------- product_name, another interest is frequent words or phrases within product names
# we have to clean the names first and lemmatize everything. 
products=inner_join(products, aisles)
products=inner_join(products, departments)
mystop=stopwords::stopwords(language='en')
product.names=products$product_name
product.names=gsub('[^A-z]', ' ', product.names)
product.names=tolower(product.names)
product.names=removeWords(product.names, mystop)
product.names=lemmatize_strings(product.names)
tokens=Boost_tokenizer(product.names)
token.table=table(tokens)
token.table=token.table/max(token.table)

summary(as.vector(token.table)) 
wordcloud(words=names(token.table), freq=token.table, min.freq=0.25) #99.9th percentile
# hmm, we see some noise such as chocolate and chicken, obviously they are not common descriptors
# like organic, free or whole. We should do something about it.
# we go through each aisle, and we aim to find common words between the aisles, we don't really care about the 
# number of items with a particular word since that is why we get chicken or cheese in the first place.
unique.words=function(data){
  temp=data$product_name
  temp=Boost_tokenizer(temp)
  temp=unique(temp)
  
  return(data.frame(temp))}

product.names=products
product.names$product_name=gsub('[^A-z]', ' ', product.names$product_name)
product.names$product_name=removePunctuation(product.names$product_name)
product.names$product_name=lemmatize_strings(product.names$product_name)
product.names$product_name=gsub('[^A-z]', ' ', product.names$product_name)
product.names$product_name=gsub(" *\\b[[:alpha:]]{1}\\b *", " ", product.names$product_name)
product.names$product_name=trimws(product.names$product_name)
product.names$product_name=gsub(' +',' ',product.names$product_name)
product.names$product_name=tolower(product.names$product_name)
product.names$product_name=removeWords(product.names$product_name, mystop)
aisle.product.names=product.names%>%group_by(aisle)%>%group_modify(~unique.words(.x))
# now we have all the unique words of every aisle, we can tally
aisle.token.table=table(aisle.product.names$temp)
aisle.token.table=aisle.token.table/max(aisle.token.table)
aisle.token.table=aisle.token.table[order(aisle.token.table, decreasing=T)]

# we see that free, natural, organic, original are the our biggest common words between all the aisles. 
# we can filter them out along with some others when we get to the analysis part.
# however, we only want to ilter out the non-nouns
# we use NLP tagging to ffind all the non-nouns in the descriptor 
top=data.frame(word=tolower(names(aisle.token.table)), freq=as.vector(aisle.token.table))

annotate.word=function(word){
  temp=annotate(word, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  temp=annotate(word, Maxent_POS_Tag_Annotator(), temp)
  temp=subset(temp, type=='word')
  tags=sapply(temp$features, '[[', 'POS')
  return(tags)
}

tags=annotate.word(top$word)
top=cbind(top, tags)
top=top[order(top$freq, decreasing=T),]
# since the NLP tags are not perfect (orange got tagged as adj but we do have some oranges in produce), we manually filter

top[which(top$word%in%c('pack', 'style', 'flavor', 'mix','light','blend','size','ct','mini','oz')),'tags']='JJ'
top[which(top$word%in%c('orange','almond','french','olive')),'tags']='NN'

topJJ=subset(top, !tags%in%c('NN','DT','TO'))
topJJ=topJJ[1:50,]

aisle.token.table=subset(aisle.token.table, names(aisle.token.table)%in%topJJ$word)
png(filename="product_name_cloud.png",width=1920, height=1080)
par(bg="black")
wordcloud(words=topJJ$word, freq=topJJ$freq, scale=c(10,-1), rot.per=F, random.order=F, use.r.layout=T, colors=brewer.pal(8,"Accent"), random.color=F) #top words
dev.off()


count.ngrams=function(data, n, nword='all'){
  temp=data$product_name
  temp=removeWords(temp, top10)
  temp=gsub(" *\\b[[:alpha:]]{1}\\b *", " ", temp)
  temp=trimws(temp)
  temp=gsub(' +', ' ',temp)
  temp=temp[which(sapply(strsplit(temp, " "), length)>=n)]
  temp=ngram(temp, n=n)
  temp=get.phrasetable(temp)
  if(is.numeric(nword)){
    temp=temp[1:nword,]}
  return(temp)}

ngram.treemap=function(data, n, nword){
  ngram.dep=data%>%group_by(department_id)%>%group_modify(~count.ngrams(.x, n=n, nword=nword))
  ngram.dep$department_id=str_to_title(departments[ngram.dep$department_id,'department'])
  ngram.dep$size=1
  png(filename=paste0("dep_", n, "gram_treemap.png"),width=1920, height=1080)
  treemap(ngram.dep, index=c('department_id', 'ngrams'), algorithm='pivotSize', vSize='size', title=paste0('Top ', nword,' Frequent ', n, '-grams of each Department'), overlap.labels = 1, 
          fontsize.title = 20, fontsize.labels=c(25,18), bg.labels=0, fontcolor.labels=c('white','black'), force.print.labels=T, ymod.labels=c(0.3,0))
  dev.off()
  }

for(i in c(1,2,3,4)){ngram.treemap(product.names, i, 10)}
# variance of product per aisle based on product name only, the higher the variance, the lower the name diversity
prod.var=product.names%>%group_by(department_id, aisle_id)%>%group_modify(~count.ngrams(.x, n=1))
prod.var=prod.var%>%group_by(department_id, aisle_id)%>%summarise(var=1/(var(prop)+0.00000001))
prod.var$department_id=str_to_title(departments[prod.var$department_id,'department'])
prod.var$aisle_id=aisles[prod.var$aisle_id,'aisle']

png(filename="aisle_name_var.png",width=1920, height=1080)
treemap(prod.var, index=c('department_id', 'aisle_id'), algorithm='pivotSize', vSize='var', title="Variance of 1-gram of aisles", overlap.labels = 1, 
        fontsize.title = 20, fontsize.labels=c(25,18), bg.labels=0, fontcolor.labels=c('white','black'), force.print.labels=T, ymod.labels=c(0.3,0))
dev.off()

# huge variation in the missing and other dep and aisle. 

# putting missing dep and aisle into real dep and aisle
# if we know aisle, we know its department already, so really, gotta put them into correct aisle
# we first construct a table of all 1, 2 and 3-grams of each aisle
product.names$product_name=removeWords(product.names$product_name, topJJ$word)
product.names$product_name=gsub(" *\\b[[:alpha:]]{1}\\b *", " ", product.names$product_name)
product.names$product_name=trimws(product.names$product_name)
product.names$product_name=gsub(' +', ' ',product.names$product_name)

get.aisle.phrases=function(data){
  phrase=data.frame()
  for(n in 1:3){
    temp=data$product_name
    temp=temp[which(sapply(strsplit(temp, " "), length)>=n)]
    temp=ngram(temp, n=n)
    temp=get.phrasetable(temp)
    temp$ngrams=trimws(temp$ngrams)
    temp$phrase.length=n
    phrase=rbind(phrase, temp)}
  
  return(phrase)}

predict.aisle=function(data){
  pname=data$product_name
  nword=sapply(strsplit(pname, " "), length)
  if(nword){}
  match.found=F
  while(!match.found & nword>0){
    temp=ngram(pname, n=nword)
    temp=get.ngrams(temp)
    candidates=aisle.phrases[which(aisle.phrases$phrase.length==nword),]
    candidates=filter(candidates, ngrams%in%temp)
    match.found=ifelse(nrow(candidates)==0, F, T)
    if(match.found){
      pred=candidates%>%group_by(aisle_id)%>%summarise(score=sum(freq))
      pred=pred[order(pred$score, decreasing=T)[1],'aisle_id']
    }else{
      nword=nword-1}
  }
  if(!exists('pred')){return(100)}else{return(pred)}
}

aisle.phrases=filter(product.names, aisle!='missing')%>%group_by(aisle_id)%>%group_modify(~get.aisle.phrases(.x))

missing.aisle=filter(product.names, aisle=='missing')

test=product.names[sample(nrow(product.names),size=3000),]

for(i in 1:nrow(test)){
  test[i,'pred']=predict.aisle(test[i,])
  print(i)
}


length(which(test$pred==test$aisle_id))/3000 #91% accuracy


for(i in 1:nrow(missing.aisle)){
  missing.aisle[i,'aisle_id']=predict.aisle(missing.aisle[i,])
  print(i)
  }

aisle.dep=unique(products[c('aisle_id','aisle','department_id','department')])
aisle.dep=aisle.dep[order(aisle.dep$aisle_id),]
missing.aisle[c('aisle','department_id','department')]=
  aisle.dep[missing.aisle$aisle_id, c('aisle','department_id','department')]
test=product.names
product.names[which(product.names$aisle=='missing'),]=missing.aisle

write.csv(product.names, 'products_clean.csv', row.names=F)

#------------------------------------ orders_pp ----------------------------------------
str(order_pp) #all ints, a summary makes no sense since they are all mostly categorical integers
#---------------- order_id, the sensible thing here is to see the stats of order size
order_size=order_pp%>%group_by(order_id)%>%summarise(n=max(add_to_cart_order))
summary(order_size) # median 8, mean 10.09
sd(order_size) # 7.525, which is a lot

outlier.range=function(data, mult=1.5){
  low=quantile(data, 0.25, na.rm=T)
  high=quantile(data, 0.75, na.rm=T)
  iqr=IQR(data, na.rm=T)
  temp=c(low-mult*iqr, high+mult*iqr)
  return(as.vector(temp))}

order_size=order_size[which(order_size$n%between%outlier.range(order_size$n, 2)),]
order_size=inner_join(orders[c('order_id','user_id')],order_size)
order_size=order_size%>%group_by(n)%>%summarise(num.order=n(), num.user=length(unique(user_id)))


ggplot()+
  geom_bar(data=order_size, stat='identity', aes(x=n, y=num.order, fill=num.user))+
  labs(title='Number of orders per order size', x='Order Size', y='Number of Orders')+
  scale_fill_gradient(name = "Number of Users", low='#0027fc', high='#f71b1b')
  

#---------------- product_id
length(which(!order_pp$product_id %in% products$product_id)) # no unknown product id's
# treemap of aisle and dep based on number of orders, lets inner join the products with order_pp
order_pp=inner_join(order_pp, products, by='product_id')

order_size=order_pp%>%group_by(department, aisle)%>%summarise(n=n(), reordered=sum(reordered), reorder_pct=reordered/n*100)

png(filename="product_order_reorder.png",width=1920, height=1080)
treemap(order_size, index=c('department', 'aisle'), algorithm='pivotSize', type='value', vSize='n', vColor='reorder_pct', title="Number of orders and reorders", 
        overlap.labels = 1, fontsize.title = 20, fontsize.labels=c(25,18), bg.labels=0, fontcolor.labels=c('white','black'), 
        fontsize.legend=30, force.print.labels=T, ymod.labels=c(0.3,0), palette='RdYlBu')
dev.off()
# Let's see the order the products are placed in baskets and how much of the order they constitute
basket=order_pp[c('order_id', 'department_id', 'add_to_cart_order', 'reordered')]
basket=basket%>%group_by(department_id)

i=0
basket.order=function(data){
  i<<-i+1
  print(i)
  mean=mean(data$add_to_cart_order)
  temp=data[order(data$order_id, data$add_to_cart_order),]
  temp=data%>%group_by(order_id)%>%summarise(first=first(add_to_cart_order))
  temp=mean(temp$first)
  reordered=sum(data$reordered)
  temp=data.frame(first=temp, mean=mean, total=nrow(data), reordered=reordered)
  return(temp)}

basket=group_modify(basket, ~basket.order(.x))
basket$department=departments[basket$department_id,'department']
basket=filter(basket, department!='missing')
basket$reordered=basket$reordered/basket$total

ggplot(data=basket, aes(x=first, y=total, label=str_to_title(department)))+
  geom_point(size=basket$reordered^2*35, color='red', alpha=0.7)+
  labs(title='Department popularity', x='Mean of first add to cart order', y='Total orders')+
  geom_text_repel(box.padding=0.7)

ggplot(data=basket, aes(x=mean, y=total, label=str_to_title(department)))+
  geom_point(size=basket$reordered^2*35, color='blue', alpha=0.7)+
  labs(title='Department popularity', x='Mean of add to cart order', y='Total orders')+
  geom_text_repel(box.padding=0.7)

test=table(order_pp$order_id)
test=data.frame(order_id=names(test), size=as.vector(test))
write.csv(test, 'order_size.csv', row.names=F)

# now we can actually go back to orders and find out the relationship between days_since_prior_order and ordeer size
orders=inner_join(orders, order_size, by='order_id')
summary(order_size$size)
orders.plot=filter(orders, size %between% outlier.range(size, 2), 
                   days_since_prior_order<30, !is.na(days_since_prior_order))
orders.plot=orders.plot%>%group_by(size, days_since_prior_order)%>%summarise(n=n())


png(filename="prior_order_size_tile.png",width=1920, height=1080)
ggplot(data=orders.plot)+
  geom_tile(aes(x=size, y=days_since_prior_order, fill=n))+
  scale_fill_gradient(name='number of orders', low='darkblue', high='orange')+
  scale_x_continuous(breaks=seq(0, max(orders.plot$size), 1), labels=seq(0, max(orders.plot$size), 1))+
  scale_y_continuous(breaks=seq(0,30,1), labels=seq(0,30,1))+
  theme(text = element_text(size=25), legend.key.size = unit(20, 'mm'))+
  labs(title='size of orders vs days since prior order',x='size of order',y='days since prior order')
dev.off()

################################## Feature Engineering and modelling #########################################
# first order of business is to cluster the products so that each aisle is split into many clusters of similar products
# we would do this with the cleaned product names and using ngrams

word.dist=function(word1, word2){
  if(word1=='' | word2==''){return(1)}else{
    word1=strsplit(word1, ' ')[[1]]
    word2=strsplit(word2, ' ')[[1]]
    total.length=length(word1)+length(word2)
    temp=length(c(setdiff(word1, word2),setdiff(word2, word1)))/total.length
    return(temp)
  }}

word.dist.matrix=function(words){
  dim=length(words)
  distance=matrix(nrow=dim, ncol=dim)
  for(i in 1:dim){
    for(k in 1:i){
      val=word.dist(words[i],words[k])
      distance[i,k]=val
      distance[k,i]=val}
    }
  return(distance)
  }

fast.dist=function(data){
  dim.data=nrow(data)
  # x^2 and y^2 and -2xy
  mat=as.matrix(data)
  mat=data%*%t(data)
  diag=diag(mat)
  x2mat=matrix(data=diag, ncol=dim.data, nrow=dim.data)
  y2mat=t(x2mat)
  dist=x2mat+y2mat-2*mat
  dist=sqrt(dist)
  return(round(dist,10))
  }

fast.cos.dist=function(data){
  dim.data=nrow(data)
  mat=as.matrix(data)
  xy=mat%*%t(mat)
  diag=sqrt(diag(xy))
  diag[which(diag==0)]=1
  x2mat=matrix(data=diag, ncol=dim.data, nrow=dim.data)
  y2mat=t(x2mat)
  dist=xy/(x2mat*y2mat)
  return(1-round(dist,10))
  }

count.words=function(data, top=5){
  tokens=Boost_tokenizer(data$product_name)
  temp=table(tokens)
  temp=temp[order(temp, decreasing=T)[1:top]]
  temp=data.frame(word=names(temp), freq=as.vector(temp))
  return(temp)
}


temp=products%>%group_by(aisle_id)%>%summarise(n=n())

test=subset(products, aisle_id==2)
names=VCorpus(VectorSource(test$product_name))
names=DocumentTermMatrix(names)
names=as.matrix(names)
test.dist=as.dist(fast.cos.dist(names))

test.clust=hclust(test.dist, method='ward.D2')
out=outlier.range(test.clust$height)[2]
plot(test.clust)
rect.hclust(test.clust, k=floor(sqrt(nrow(test))))
test=cbind(test, clust=cutree(test.clust, k=floor(sqrt(nrow(test)))))


cluster.tokens=test%>%group_by(clust)%>%group_modify(~count.words(.x, top=3))

i=0
cluster.prod=function(data){
  i<<-i+1
  print(i)
  names=VCorpus(VectorSource(data$product_name))
  names=DocumentTermMatrix(names)
  names=as.matrix(names)
  names.dist=as.dist(fast.cos.dist(names))
  
  prod.clust=hclust(names.dist, method='ward.D2')
  clusters=cutree(prod.clust, k=floor(sqrt(nrow(data))))
  data=cbind(data, clusters)
  return(data)
  }

name.cluster=function(data){
  cluster.size=nrow(data)
  data=Boost_tokenizer(data$product_name)
  data=prop.table(table(data))
  data=data[order(data, decreasing=T)[1:3]]
  data=paste(names(data), collapse=' ')
  return(data.frame(cluster.name=data, cluster.size=cluster.size))
}


test=filter(products.clust, clusters==202)
name.cluster2(test)

products.clust=products%>%group_by(aisle_id)%>%group_modify(~cluster.prod(.x))
products.clust$clusters=products.clust$clusters+100*products.clust$aisle_id
write.csv(products.clust, 'products_clust.csv', row.names=F)

# cluster names now, extracted from the top words of each aisle
cluster.names=products.clust%>%group_by(aisle_id, clusters)%>%group_modify(~name.cluster(.x))
order_pp=inner_join(order_pp, products.clust[c('product_id','clusters')], by='product_id')
cluster.freq=table(order_pp$clusters)
cluster.freq=data.frame(clusters=names(cluster.freq), freq=as.vector(cluster.freq))
cluster.freq$clusters=as.numeric(cluster.freq$clusters)
cluster.freq=inner_join(cluster.freq, cluster.names, by='clusters')
cluster.freq$prop=cluster.freq$freq/sum(cluster.freq$freq)
write.csv(cluster.freq, 'cluster.csv', row.names=F)
#------------------ Association rules --------------------------
# now we have the clustered items, we join it with the orders_pp

transactions=inner_join(order_pp[c('order_id','product_id')], products.clust[c('product_id','clusters')], by='product_id')
transactions=transactions[,-which(names(transactions)=='product_id')]
outlier.products=clusters[which(clusters$prop>quantile(clusters$prop, 0.95)),'clusters']
transactions=filter(transactions, !clusters%in%outlier.products)
transactions=format_csv(transactions)
conn=textConnection(object=transactions)
transactions=read.transactions(conn, 'single', cols=c("order_id","clusters"), header=T, sep = ",")


##transactions=read.transactions('order_transactions.csv', 'single', cols=c("order_id","clusters"), header=T, sep = ",")
supp.min=quantile(clusters$prop, 0.5)
test.arules=apriori(transactions, parameter = list(supp=supp.min, conf = 0.2, target = "rules"))
arules.frame=DATAFRAME(test.arules)

rule.cross.aisle=function(row){
  print(row)
  rhs=gsub('[{|}]', '', row['RHS'])
  lhs=gsub('[{|}]', '', row['LHS'])
  rhs=as.numeric(str_split(rhs, ',')[[1]])
  lhs=as.numeric(str_split(lhs, ',')[[1]])
  rhs=unique(rhs%/%100)
  lhs=unique(lhs%/%100)
  return(ifelse(length(intersect(lhs,rhs)), 0, 1))
}

rule.cluster.name=function(row){
  row=gsub('[{|}]', '', row)
  row=as.numeric(str_split(row, ',')[[1]])
  row=paste(cluster.names[which(cluster.names$clusters%in%row),'cluster.name'], collapse=', ')
  return(row)
}

## social network graph or aisles
aisle.rules=function(data, naisle){
  edges=data.frame(matrix(ncol=3, nrow=0))
  colnames(edges)=c('lhs','rhs','count')
  for(i in 1:nrow(data)){
    row=data[i,]
    lhs=gsub('[{|}]', '', row$LHS)
    lhs=as.numeric(str_split(lhs,',')[[1]])
    lhs=lhs%/%100
    rhs=gsub('[{|}]', '', row$RHS)
    rhs=as.numeric(str_split(rhs,',')[[1]])
    rhs=rhs%/%100
    n=row$count
    conn=data.frame(lhs=lhs, rhs=rhs, count=n)
    edges=rbind(edges, conn)
  }
  edges=edges%>%group_by(lhs,rhs)%>%summarise(count=sum(count))
  edges$lhs=aisles[edges$lhs,'aisle']
  edges$rhs=aisles[edges$rhs,'aisle']
  
  aisle=edges%>%group_by(lhs)%>%summarise(n=sum(count))
  aisle=aisle[order(aisle$n, decreasing=T)[1:naisle],]
  
  self.count=edges[which(edges$lhs==edges$rhs),]
  self.count=self.count%>%group_by(lhs)%>%summarise(n=sum(count))
  
  edges=edges[which(edges$lhs!=edges$rhs),]
  edges=edges[order(edges$count, decreasing=T),]
  
  nodes=data.frame(lhs=unique(c(edges$lhs, edges$rhs)))
  nodes=full_join(nodes, self.count, by='lhs')
  
  nodes=nodes[which(nodes$lhs%in%aisle$lhs),]
  edges=edges[which(edges$lhs%in%aisle$lhs & edges$rhs%in%aisle$lhs),]
  return(list(edges, nodes))
}
test=aisle.rules(rules.frame, 20)
edges=test[[1]]
nodes=test[[2]]
g=graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
E(g)$width=edges$count/3000
V(g)$size=log(nodes$n)
plot.igraph(g, vertex.label.cex = .75, layout=layout_as_star)
