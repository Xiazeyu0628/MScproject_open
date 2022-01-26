rm(list = ls())
load('../my_models/Rdata/consistencty.Rdata')
load('../my_models/Rdata/gen_labeled.Rdata')

# 数据分析部分


lm(cronbach_alpha~fixed+issue, data=consistency) %>% summary()
#lm(cronbach_alpha~fix+rule_change+fix*rule_change, data=consistency) %>% summary()

selected_consistency = consistency%>% filter(fixed=="fix_A")
lm(cronbach_alpha~issue, data=consistency) %>% summary()   #0.2606

selected_consistency = consistency%>% filter(fixed=="fix_R")
lm(cronbach_alpha~issue, data=consistency) %>% summary()   #0.06548


lm(cronbach_alpha~fixed, data=consistency) %>% summary()  


selected_consistency = consistency%>% filter(issue=="indicator")
lm(cronbach_alpha~fixed, data=selected_consistency) %>% summary()  #0.1672

summarise(consistency, mu=mean(cronbach_alpha), sd=sd(cronbach_alpha),max = max(cronbach_alpha),min = min(cronbach_alpha))
#      mu        sd      max       min
#1 0.7773429 0.0926196 0.898374 0.3859649

gen_labeled %>% 
  filter(fixed=='fixed_R') %>%  # mu = 0.7713563 0.09260682
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(fixed=='fixed_A') %>%  #0.7833295 0.09297368
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))

t.test(gen_labeled %>% 
         filter(fixed=='fixed_R')%>%pull("cronbach_alpha"), gen_labeled %>% 
         filter(fixed=='fixed_A')%>%pull("cronbach_alpha"))

# original

gen_labeled %>% 
  filter(issue=='original') %>%  # 0.7834333 0.09913871
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='original'&fixed=='fixed_A') %>%  #0.8233647 0.05274233
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='original'&fixed=='fixed_R') %>%  #0.7435018 0.1188647
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))

selected_consistency = consistency%>% filter(issue=="original")
lm(cronbach_alpha~fixed, data=selected_consistency) %>% summary()  # 0.02003


gen_labeled %>% 
  filter(issue=='indicator') %>%  #  0.7611047 0.1151517
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='indicator'&fixed=='fixed_A') %>%  # 0.7327468 0.1265065
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='indicator'&fixed=='fixed_R') %>%  # 0.8021459 0.08515452
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))

selected_consistency = consistency%>% filter(issue=="indicator")
lm(cronbach_alpha~fixed, data=selected_consistency) %>% summary()  # 0.0.1672


# 感觉实际上更像是把R作为实际的agent
gen_labeled %>% 
  filter(issue=='movement') %>%  #  0.801475 0.05062199
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='movement'&fixed=='fixed_A') %>%  #  0.8156619 0.04125595
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='movement'&fixed=='fixed_R') %>%  # 0.8104356 0.04662204
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))

selected_consistency = consistency%>% filter(issue=="movement")
lm(cronbach_alpha~fixed, data=selected_consistency) %>% summary()    # 0.0.5603



# statechange 的因果是对称的

gen_labeled %>% 
  filter(issue=='statechange') %>%  #0.7633587 0.09153829
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='statechange'&fixed=='fixed_A') %>%  # 0.7615446 0.09877949
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))
gen_labeled %>% 
  filter(issue=='statechange'&fixed=='fixed_R') %>%  # 0.7651727 0.08690643
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))

selected_consistency = consistency%>% filter(issue=="statechange")
lm(cronbach_alpha~fixed, data=selected_consistency) %>% summary()    # p = 0.9129





t.test(gen_labeled %>% 
         filter(issue=='statechange'&fixed=='fixed_R')%>%pull("cronbach_alpha"), gen_labeled %>% 
         filter(issue=='statechange'&fixed=='fixed_A')%>%pull("cronbach_alpha"))


# Label gen task types





# Random baseline (simulated)
sim.random<-function(n) {
  a<-data.frame(object=all_objects, id=seq(20))
  x<-replicate(24, sample(seq(20), 1)) %>% table() %>% data.frame()   # 对20个obj随机采样，采24个
  colnames(x)<-c('id', 'n')
  x<- x %>% mutate(id=as.numeric(as.character(id)))
  a<-a %>%
    left_join(x, by='id') %>%
    mutate(n=ifelse(is.na(n), 0, n))
  return(a$n)
}
mean(replicate(1000,KR21(sim.random(25)))) # ~  # 24人随机选择，做1000组，1000组的kr21均值为0



lm(cronbach_alpha~total_diff, data=gen_labeled) %>% summary()

lm(cronbach_alpha~fixed_diff+varied_diff+fixed_diff*varied_diff, 
   data=gen_labeled) %>% summary()

lm(cronbach_alpha~agent_diff+recipient_diff+agent_diff*recipient_diff, 
   data=gen_labeled) %>% summary()


lm(cronbach_alpha~agent_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~recipient_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~fixed_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~varied_diff, gen_labeled) %>% summary()



t<-lm(cronbach_alpha~agent_diff, gen_labeled) %>% summary()
t$coefficients['agent_diff',4]   # 0.478











