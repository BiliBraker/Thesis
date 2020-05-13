library(tidyverse)
library(stargazer)

path='c:/Users/soirk/Krisztian/Egyetem/Survey Statisztika Msc/Szakdolgozat/corpus/'

papers=readxl::read_xlsx(paste(path,'papers_FINAL_really.xlsx',sep=''),
                         sheet='data_to_model_2',
                         col_names = T,
                         col_types = c('text','numeric','text','numeric','numeric','numeric','numeric'))

papers=papers %>% 
  filter(.,date<2017)

papers$category=papers$category %>% as.factor()
papers$date=papers$date %>% as.factor()

model=glm(advanced~date+category,family = 'binomial',data=papers)
model %>% 
  summary()

model1=glm(deletion~date+category,family = 'binomial',data=papers)
model1 %>% 
  summary()

model2=glm(not_advanced~date+category,family = 'binomial',data=papers)
model2 %>% 
  summary()

model3=glm(basic~date+category,family = 'binomial',data=papers)
model3 %>% 
  summary()

# save the results
stargazer(model,model1,model3,model2,
          type='html',
          out=paste(path,'papers_log_models.html',sep=''),
          title = 'Logistic models',
          dep.var.labels = c('Advanced Imputation',
                             'Deletion',
                             'Basic Imputation',
                             'Not Advanced method'),
          covariate.labels = c(levels(papers$date)[2:length(levels(papers$date))],
                               levels(papers$category)[2:length(levels(papers$category))])
)