#### setup ####

library(tidyverse)
library(plyr)
library(caret)

set.seed(12)

path='c:/Users/soirk/Krisztian/Egyetem/Survey Statisztika Msc/Szakdolgozat/corpus/'
# read the weighted dataset
papers=read.csv2(paste(path,'papers_w.csv',sep=''),sep=',',
                 dec='.')

papers$id=papers$id %>% as.character()
papers$sum=papers[,5:ncol(papers)] %>% apply(.,1,sum)

#### creating subsample ####

papers_sub=papers %>% filter(.,sum>0)

train_ids=sample(1:nrow(papers_sub),100)

papers_train=papers_sub[train_ids,]

papers_test=papers_sub[-train_ids,]

papers_train$title=papers_train$title %>% gsub(.,patt=", ", replace="-")

#write.table(papers_train,paste(path,'papers_train.csv',sep=''),sep = ',',row.names = F)

###### read the manually trained dataset ####

papers_train=read.csv2(paste(path,'papers_train_2.csv',sep=''),sep=',',
                 dec='.')

model_log=glm(miss_cat~.,data=papers_train[,c(5:10,12)],
              family = 'binomial')

papers_train=cbind(papers_train,log=model_log$fitted.values)

odds_to_prob=function(log_model,predictors){
  prob_pred=1/(1+exp(-(predict(log_model,predictors))))
  return(prob_pred)
}

test_pred=odds_to_prob(model_log,papers_test[,c(5:10)])

papers_test=cbind(papers_test,log=test_pred)

papers_test$mis_cat=ifelse(papers_test$log>.5,1,0)
#### create subsample of the test set to calculate accuracy and other measures 
# with a sample from the test set ####

set.seed(123)

acc_ids=sample(1:nrow(papers_test),100)

papers_acc=papers_test[acc_ids,]

papers_acc$mis_cat=ifelse(papers_acc$log>.5,1,0)

#write.table(papers_acc,paste(path,'papers_acc.csv',sep=''),sep = ',',row.names = F)

#### fitting the model to the subsample ####

pred=odds_to_prob(model_log,papers_sub[,c(5:10)])

papers_sub=cbind(papers_sub,log=pred)

papers_sub$mis_cat=ifelse(papers_sub$log>.5,1,0)

#### fitting the model to the whole sample ####
pred=odds_to_prob(model_log,papers[,c(5:10)])

papers=cbind(papers,log=pred)
papers$mis_cat=ifelse(papers$log>.5,1,0)

## handling "handl"

for(i in 1:nrow(papers)){
  if(papers[i,7]>0){
    papers[i,13]=1
  }else{
    next
  }
}

#### clearing titles, duplicates #### 

papers$title=papers$title %>% gsub(.,patt=", ", replace="-")

#nrow(unique(papers))-nrow(papers)

papers=unique(papers)

#write.table(papers[,-c(2,11,12)],paste(path,'papers_cat_v2.csv',sep=''),sep = ',',row.names = F)