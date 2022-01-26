# 用来算consistency的
rm(list = ls())
setwd("/Users/xiazeyu/Desktop/github_local/MSCproject/data_process")
library(dplyr)
library(ggplot2)


load('../my_models/Rdata/mturk/mturk_filtered.Rdata')
load('../my_models/Rdata/gen_labeled.Rdata')


number_condtions = 8

# Get aggregated data
all_objects<-c()
for (s in 3:7) {
  for (c in 1:4) {
    all_objects<-c(all_objects, s*10+c)
  }
}
default<-expand.grid(
  group=paste0('A',1:number_condtions), trial=1:16, object=as.character(all_objects),
  stringsAsFactors =F)

# Sneaky: see compatible ones
# good_ixes<-labels %>% filter(rule_type!='incompatible') %>% pull(ix)
# Nope

counts<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(
    group=condition,
    trial=as.numeric(substr(sid,8,9)), 
    object=as.character(result)) %>%
  group_by(group, trial, object) %>%
  summarise(count=n()) %>%
  mutate(freq=count/sum(count)) %>%
  ungroup() %>%
  full_join(default, by=c('group', 'trial', 'object')) %>%
  mutate(
    count=ifelse(is.na(count), 0, count), 
    freq=ifelse(is.na(freq), 0, freq),
  ) %>%
  arrange(group, trial, object)

# Plot  行为数据
# pic1
ggplot(counts, aes(x=object, y=trial, fill=freq)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~group)

# Analyze consistency with cronbach-alpha
# https://en.wikipedia.org/wiki/Cronbach%27s_alpha
get_cronbach_alpha<-function(vec) {
  sx<-var(c(1, rep(0,19)))   # rep.int(x, times)  result obj的方差
  k=sum(vec)  #某一个task的人数总和
  sy<-var(vec)  # 总的方差
  return(k/(k-1)*(1-(k*sx)/sy))
} 

KR20<-function(x, k=20) {
  n=length(x)
  if (n-length(which(x==0))==1) {
    rho=1
  } else {
    total=sum(x)
    items=sum(sapply(x, function(i) (i/total)*(1-i/total)))
    rho=(k/(k-1))*(1-items/var(x))
  }
  return(rho)
}

KR21<-function(x) {
  k=sum(x)
  n=length(x)
  p=1/n   #p is the chance probability of picking an object if responding randomly
  rho<-(k/(k-1))*(1-(k*p*(1-p)/var(x)))
  return(rho)
}

# KR21的取值范围是-inf～1(n number of K)
test_vec = c(0,0,0,0,10,0,0,0,0,0)
KR21(test_vec)

test_vec = c(1,1,1,1,1,1,1,1,1)
KR21(test_vec)

get_cronbach_alpha(filter(counts, group=='A1', trial==1) %>% pull(count))  # Extract a single column

consistency<-expand.grid(
  condition=paste0('A',1:number_condtions), trial=1:16, cronbach_alpha=NA, kr20 = NA,kr21=NA,
  issue = NA, fixed = NA,stringsAsFactors = F
) %>% 
  arrange(condition, trial)

for (i in 1:nrow(consistency)) {
  cond=consistency[i,'condition']
  tid=consistency[i, 'trial']
  count_vec<-filter(counts, group==cond, trial==tid) %>% pull(count)
  consistency[i, 'cronbach_alpha'] = get_cronbach_alpha(count_vec)
  consistency[i, 'kr20'] = KR20(count_vec)
  consistency[i, 'kr21'] = KR21(count_vec)
  
  if (cond=="A4"||cond=="A8") issue_value = "original"
  else if (cond=="A1"||cond=="A5") issue_value = "statechange"
  else if (cond=="A2"||cond=="A6") issue_value = "movement"
  else if (cond=="A3"||cond=="A7") issue_value = "indicator"
  
  if (cond %in% c("A1","A2","A3","A4")) fixed_value = "fixed_A"
  else if (cond %in% c("A4","A5","A6","A7")) fixed_value = "fixed_R"
  
  consistency[i, 'issue'] = issue_value
  consistency[i, 'fixed'] = fixed_value
  
}
save(consistency, file='../my_models/Rdata/consistencty.Rdata')


##### gen_labeled

# gen_labeled 就是consistency+每一个trial的对于diff 的统计

gen_labeled<-read.csv('../my_models/Rdata/mturk/main.csv', stringsAsFactors=F) %>%
  select(condition, phase, trial=task, agent, recipient)

get_stone_overlaps<-function(vec_a, vec_b) {
  appeared_shades<-unique(vec_a%%10)
  appeared_edges<-unique(floor(vec_a/10))
  tasked_shades<-unique(vec_b%%10)
  tasked_edges<-unique(floor(vec_b/10))
  return(2-max(tasked_edges %in% appeared_edges)-max(tasked_shades %in% appeared_shades))
}

get_stone_overlaps_upd<-function(vec_a, vec_b) {
  #appeared_shades<-unique(vec_a%%10)
  #appeared_edges<-unique(floor(vec_a/10))
  #tasked_shades<-unique(vec_b%%10)
  #tasked_edges<-unique(floor(vec_b/10))
  value = max(vec_b %in% vec_a)
  return(1-value)
}



for (i in 1:nrow(gen_labeled)) {
  cond=gen_labeled[i, 'condition']
  tid=gen_labeled[i, 'trial']
  agent=gen_labeled[i, 'agent']
  recipient=gen_labeled[i, 'recipient']
  all_tasked<-c(agent, recipient) %>% unique()
  # Get learning data
  learn_agents<-gen_labeled %>% filter(phase=='learn', condition==cond) %>% 
    pull(agent) %>% unique()     #condition 为 A* 时的agent的类
  learn_recipient<-gen_labeled %>% filter(phase=='learn', condition==cond) %>% 
    pull(recipient) %>% unique()  #condition 为 A* 时的recipient的类
  all_appeared<-c(learn_agents, learn_recipient) %>% unique() # learning阶段所有出现的obj
  # calculate different measurements
  gen_labeled[i, 'agent_diff']<-get_stone_overlaps(learn_agents, agent)
  gen_labeled[i, 'recipient_diff']<-get_stone_overlaps(learn_recipient, recipient)
  gen_labeled[i, 'fixed_diff']<-ifelse(cond %in% c('A1','A2','A3','A4'), gen_labeled[i, 'agent_diff'], gen_labeled[i, 'recipient_diff'])
  gen_labeled[i, 'varied_diff']<-ifelse(cond %in% c('A1','A2','A3','A4'), gen_labeled[i, 'recipient_diff'], gen_labeled[i, 'agent_diff'])
}

gen_labeled<-gen_labeled %>%
  filter(phase=='gen') %>%   #因为learn阶段不可能有区别
  select(condition, trial, ends_with('diff'))

gen_labeled<-gen_labeled %>% 
  left_join(consistency, by=c('condition', 'trial'))

gen_labeled$total_diff<-gen_labeled$fixed_diff+gen_labeled$varied_diff
save(gen_labeled, file='../my_models/Rdata/gen_labeled.Rdata')



