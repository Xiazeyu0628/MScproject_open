rm(list=ls())
library(tidyverse)
library(viridis)

setwd("/Users/xiazeyu/Desktop/github_local/MSCproject/my_models")

source('gibbs.R')
source('preds.R')
source('shared.R')
load('./Rdata/hypos.Rdata')
tasks<-read.csv('./Rdata/mturk/main.csv')
load('./Rdata/mturk/mturk_main.Rdata')


alpha=1024
beta=0
softmax_base=''
drop=500
slice=1
iter=10000
set.seed(123)

n_gen_obs<-16
n_learn_obs<-6
number_condition = 8

results<-list()
preds<-data.frame(group=character(0),
                  trial=numeric(0),
                  object=numeric(0),
                  prob=numeric(0),
                  type=character(0))

first_run_post = FALSE
first_run_allpreds = FALSE
first_run_gibbs = FALSE

# 有新的数据时候，这里需要修改
#df.posts数据集，是拿不同condition下的learning example和prior of causal law去更新p(law|condition)这个后验
if(first_run_post==TRUE){
  df.posts<-get_hypo_posts('A1',tasks,df.hypos)
  for (i in 2:number_condition) df.posts<-rbind(df.posts, get_hypo_posts(paste0('A',i)))
  save(df.posts, file='./Rdata/posts.Rdata')
  
}else{load('./Rdata/posts.Rdata')}

if(first_run_allpreds==TRUE){  # get all preds
  # all_preds 里包含着对于所有causal laws来说，每一个condition下每一个gen trial可能的result，及其概率
  all_preds<-list()
  for (h in df.hypos$hypo) {
    all_preds[[h]]<-list()
    for (t in 1:number_condition) {
      all_preds[[h]][[t]]<-list()
      for (i in 1:16) {
        tk<-tasks%>%
          filter(condition==paste0('A',t)&phase=='gen'&task==i)%>%
          select(agent, recipient)%>%
          as.list()
        all_preds[[h]][[t]][[i]]<-causal_mechanism(h, tk)
      }
    }
  }  
  save(all_preds, file='./Rdata/all_preds.Rdata')
}else{load('./Rdata/all_preds.Rdata')}

if(first_run_gibbs==TRUE){  
  for (c in 1:number_condition) {  # 8个condition
    for (g in c(1,0.5,0)) {                # g就是gamma
      cond<-paste0('A', c)
      
      type_value = get_type(g)
      
      if (type_value=='A') n = c*3-2 
      else if (type_value=='AR') n = c*3-1
      else if (type_value=='R') n = c*3
        
      x<-run_gibbs_sampler(cond, g, alpha, beta, iter, F,df.posts)   #function(cond, grouping, alpha, beta, limit, logging=T, hypo_source=df.posts)
      # read category
      cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)  # thinning
      func_preds<-prep_preds(x[[2]], cond)  # 从all_preds中拿出对应的sampled laws/funcs 以及cond
      y<-get_cond_preds(cond, cats, func_preds, alpha, beta, g)  # g=1
      y<-y%>%mutate(type=type_value)
      results[[n]]<-x 
      preds<-rbind(preds, y)
    }
  }
  preds$object<-as.character(preds$object)
  save(results, preds, file='./Rdata/dp_models.Rdata')
  
}else{load("./Rdata/dp_models.Rdata")}


# Look ups
#cats<-read_cats(x[[1]])
#nrow(cats%>%filter(n>5))

#cats_cp<-read_cats(results[[3]][[1]])

# Plot with baseline model & ppt data
# Get pplt data from ../analysis/behav_plot.R
load("./Rdata/ppts.Rdata")
load("./Rdata/baseline_models.Rdata")

ppt<-ok%>%
  select(condition, task=trial, object=result, prob) %>%
  mutate(type='ppt')
plain<-ce_preds%>%
  select(condition=group, task=trial, object, prob=pred)%>%
  mutate(type='plain')
dpa<-preds%>%
  select(condition=group, task=trial, object, prob, type)%>%filter(type=="A")
dpr<-preds%>%
  select(condition=group, task=trial, object, prob, type)%>%filter(type=="R")
dpar<-preds%>%
  select(condition=group, task=trial, object, prob, type)%>%filter(type=="AR")


combined<-bind_rows(ppt, plain, dpa, dpr,dpar)
combined$type=factor(combined$type, levels=c('plain','R', 'AR', 'A', 'ppt'))
combined$object<-as.character(combined$object)

ggplot(combined, aes(x=object, y=task, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  facet_grid(type~condition)



fit_ll<-function(b, data, type='fit') {   # b是平滑系数
  softed<-filter(data, condition=='A1', task==1)%>%mutate(softmaxed=softmax(prob, b))
  for (c in 1:number_condition) {
    for (i in 1:16) {
      if (!(c==1 & i==1)) {
        softed<-rbind(softed, 
                      filter(data, condition==paste0('A',c), task==i)%>%mutate(softmaxed=softmax(prob, b)))
      }
    }
  }
  if (type=='fit') {
    return(-sum(softed$count*log(softed$softmaxed)))
  } else {
    return(softed)
  }
}
#fit_ll(1, counts)
# 101*16*log(1/20) # -4841.103

add_count<-function(posterior_distribution){
  prep<-df.tw %>%
    filter(phase=='gen'&grepl('gen', sid)) %>%
    mutate(
      condition=condition,
      task=as.numeric(substr(sid,8,9)),     #substr(s, first, last) 截取sid中间数字的部分
      object=as.character(result)) %>%
    group_by(condition, task, object) %>%
    summarise(count=n()) %>%ungroup() %>%  #解除分组
    right_join(posterior_distribution, by=c('condition', 'task', 'object')) %>%   #在ce_preds中group', 'trial', 'object'的右边加count
    mutate(count=ifelse(is.na(count), 0, count))
  return(prep)
}


plain_out<-optim(par=0, fn=fit_ll, data=add_count(plain), method='Brent', lower=0, upper=100)
plain_out$par #1.723045
plain_out$value #3486.221

dpa_out<-optim(par=0, fn=fit_ll, data=add_count(dpa), method='Brent', lower=0, upper=100)
dpa_out$par #5.96251
dpa_out$value #3328.233

dpar_out<-optim(par=0, fn=fit_ll, data=add_count(dpar), method='Brent', lower=0, upper=100)
dpar_out$par #6.949102
dpar_out$value #3284.791

dpr_out<-optim(par=0, fn=fit_ll, data=add_count(dpr), method='Brent', lower=0, upper=100)
dpr_out$par #6.089
dpr_out$value #3296

plain_softmaxed = fit_ll(plain_out$par, plain, '')
dpa_softmaxed = fit_ll(dpa_out$par, dpa, '')
dpar_softmaxed = fit_ll(dpar_out$par, dpar, '')
dpr_softmaxed = fit_ll(dpr_out$par, dpr, '')


combined<-bind_rows(ppt, plain_softmaxed, dpa_softmaxed, dpar_softmaxed,dpr_softmaxed)
combined$type=factor(combined$type, levels=c('plain','R', 'AR', 'A', 'ppt'))
combined$object<-as.character(combined$object)

ggplot(combined, aes(x=object, y=task, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  facet_grid(type~condition)










