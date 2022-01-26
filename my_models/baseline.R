rm(list=ls())
library(tidyverse)
library(viridis)

setwd("/Users/xiazeyu/Desktop/github_local/MSCproject/my_models")
source('shared.R')

# Get experiment data
tasks<-read.csv('./Rdata/mturk/main.csv')
load('./Rdata/mturk/mturk_main.Rdata')
load('./Rdata/hypos.Rdata')

mode = "learning"
num_condition = 8

fetch_task<-function(group_name, phase_name, trial_id, type='list', source=tasks) {
  task_data<-source%>%
    filter(condition==group_name&phase==phase_name&task==trial_id)%>%
    select(agent, recipient, result)  # 选择 agent, recipient，result这三列
  if (type=='s') return(paste(task_data, collapse=',')) else return(as.list(task_data)) # 强制类型到list
}

listify_task<-function(task_str) {  #列表化
  if (typeof(task_str)=='list') return(task_str) else {
    task_els<-strsplit(task_str, ',')[[1]]
    if (task_els[3]==0) task_els[3]<-NA
    return(list(agent=task_els[1], recipient=task_els[2], result=task_els[3]))
  }
}

# Learning: get posterior

if(mode == "learning"){
  get_group_post<-function(group_name, source=df.hypos) {
    hypos<-source%>%select(hypo, prior)   # 选择了hypo和prior这两列  管道操作的优先级比赋值要高
    final_col<-paste0('post_', group_name)   # post_A1
    for (i in seq(6)) {  # 通过6次学习，每次学习更新后验概率
      data<-fetch_task(group_name, 'learn', i, 's')
      prior_col<-if (i==1) 'prior' else paste0('post_', i-1)
      likeli_col<-paste0('likeli_',i)
      post_col<-paste0('post_',i)
      rep_data = rep(data, nrow(hypos))
      # Apply a Function to Multiple List or Vector  Arguments
      hypos[,likeli_col]<-mapply(get_likeli, hypos$hypo, rep_data)   # rep将data重复hypos的行数
      
      hypos[,post_col]<-hypos[,prior_col]*hypos[,likeli_col]  # posterior = prior*likelihood
      hypos[,post_col]<-normalize(hypos[,post_col])
    }
    hypos[,final_col]<-hypos$post_6    # 最后一次更新的值就是该condition的值
    return(hypos[,c('hypo', final_col)])
  }
  df.post<-get_group_post('A1')   # 后验概率，已知hypo，求处于什么condition的概率
  
  for (i in 2:num_condition) {    # 更新4个condition
    post<-get_group_post(paste0('A',i))
    df.post<-df.post%>%left_join(post,by='hypo')
  }
  df.hypos<-df.hypos%>%select(hypo, prior)
  df.hypos<-df.hypos%>%left_join(df.post, by='hypo')
  save(df.hypos, file='./Rdata/hypos.Rdata')

}



# Generalization predictions
# 输入group，gen trial_id, 以及学习到的后验概率
get_one_gen_pred<-function(group_name, trial_id, learn_post) {   
  data<-fetch_task(group_name, 'gen', trial_id)
  post_col<-paste0('post_', group_name)
  df<-learn_post[(learn_post[,post_col]>0),]  # 取出了对应condition的那列后验概率
  #（已知condition的情况下，可能的causal law，以及其所对应的概率）
  result<-unlist(init_dist())
  for (i in 1:nrow(df)) {
    hypo = df$hypo[i]  # 这里的hypo 就是一个causal law
    # causal_mechanism(hypo,data),输入某一次gen具体的参数（data），
    #其在某个causal law下（hypo），所可能的结果以及每个结果的概率
    
    # 乘并加在一起，则表示在已知condition的情况下，每一个结果object的概率
    result<-result+(unlist(causal_mechanism(hypo, data)))*df[i, post_col]  
  }
  return(data.frame(group=group_name, phase='gen', trial=trial_id, 
                    object=all_objects, pred=normalize(result)))
}

# Put them together

init_result<-data.frame(group=character(0), phase=character(0), trial=numeric(0), pred=character(0), pred=numeric(0))

ce_preds<-init_result
for (i in 1:num_condition) {   # 这个值原本是2，但我觉得逻辑上是
  group_name=paste0('A', i)
  learned<-df.hypos[,c('hypo', paste0('post_', group_name))]
  prediction<-get_one_gen_pred(group_name, 1, learned)
  for (j in 2:16) {
    prediction<-rbind(prediction, get_one_gen_pred(group_name, j, learned))  # rebind合并行
  }
  ce_preds<-rbind(ce_preds, prediction)
}
ce_preds<-ce_preds%>%mutate(source='causal_grouped')%>%select(group, trial, object, pred, source)  #加入新的一项，causal_grouped
ce_preds$object<-as.character(ce_preds$object)  # 把object这一列的值由int转化为character
save(ce_preds, file='./Rdata/baseline_models.Rdata')


# Fit a softmax and check likelihood
baseline_ll<-nrow(df.sw)*16*log(1/20)    # nrow(df.sw)是人数，20个object，所以抽中正确的概率是1/20 （exp1中是1/9）

prep<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(
    group=condition,
    trial=as.numeric(substr(sid,8,9)),     #substr(s, first, last) 截取sid中间数字的部分
    object=as.character(result)) %>%
  group_by(group, trial, object) %>%
  summarise(count=n()) %>%
  ungroup() %>%   #解除分组
  right_join(ce_preds, by=c('group', 'trial', 'object')) %>%   #在ce_preds中group', 'trial', 'object'的右边加count
  mutate(count=ifelse(is.na(count), 0, count))
  
  
#counts =  select(prep,group, trial, object, count, prob=pred)
counts =  select(prep,group, trial, object, count, pred)  # prep 去掉一个source就是counts

fit_ll<-function(b, data, type='fit') {   # b是平滑系数
  softed<-filter(data, group=='A1', trial==1)%>%mutate(soft=softmax(pred, b))
  for (c in 1:num_condition) {
    for (i in 1:16) {
      if (!(c==1 & i==1)) {
        softed<-rbind(softed, 
                      filter(data, group==paste0('A',c), trial==i)%>%mutate(soft=softmax(pred, b)))
      }
    }
  }
  if (type=='fit') {
    return(-sum(softed$count*log(softed$soft)))
  } else {
    return(softed)
  }
}
#fit_ll(1, counts)
# 101*16*log(1/20) # -4841.103
out<-optim(par=0, fn=fit_ll, data=counts, method='Brent', lower=0, upper=100)   # 找一个最好的平滑系数
out$par #1.723045
out$value #3486.221

plain_model<-fit_ll(3.19, counts, '')
#passed_short<-passed %>% select(condition, trial, result, prob, label)
plain<-plain_model %>%
  mutate(condition=group, result=object, label='plain') %>%
  select(condition, trial, result, pred, label)

#ggplot(bind_rows(passed_short, plain), aes(x=result, y=trial, fill=prob)) + geom_tile() + 
ggplot(plain, aes(x=result, y=trial, fill=pred)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(label~condition)








