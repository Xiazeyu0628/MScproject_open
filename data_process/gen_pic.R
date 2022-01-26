rm(list = ls())

load('../my_models/Rdata/consistencty.Rdata')
load('../my_models/Rdata/gen_labeled.Rdata')
load('../my_models/Rdata/mturk/mturk_filtered.Rdata')

#gen_labeled %>% mutate()

for (i in 1:nrow(gen_labeled)) {
  cond=gen_labeled[i, 'condition']
  if(cond!="A2"||cond!="A6") movement_difference =gen_labeled[i, 'agent_diff'] else gen_labeled[i, 'recipient_diff']
  if(cond!="A2"||cond!="A6") stationary_difference =gen_labeled[i, 'recipient_diff'] else gen_labeled[i, 'agent_diff']
  gen_labeled[i, 'movement_diff']<-movement_difference
  gen_labeled[i, 'stationary_diff']<-stationary_difference
}

x<-gen_labeled %>%
  select(condition, trial, ends_with('diff'), cronbach_alpha) %>%
  pivot_longer(cols=ends_with('diff'), names_to='diff', values_to='value')

# pic12  movement_diff 和 stationary_diff 的变化趋势
pic12 = x %>%
  filter(diff %in% c('movement_diff', 'stationary_diff')) %>%
   ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point(size=2) + labs(x='') +
  scale_x_continuous(breaks = c(0,1,2)) +
  geom_smooth(method = "lm", fill=NA) +
  labs(x='Difference', y='', title='') +
  theme(legend.title = element_blank()) +
  scale_color_brewer(palette="Paired")
pic12


# pic1
ggplot(consistency, aes(x=condition, y=kr21, fill=condition)) +
  geom_violin() 

# pic2
ggplot(consistency, aes(x=issue, y=cronbach_alpha, fill=condition)) +
  geom_violin() +
  facet_grid(~fixed, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  labs(x='', y='', title='Cronbach alpha per trial') +
  scale_fill_brewer(palette="Paired")


#  pic3
ggplot(consistency, aes(x=fixed, y=cronbach_alpha, fill=condition)) +
  geom_violin() +
  facet_grid(~issue, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  labs(x='', y='', title='Cronbach alpha per trial') +
  scale_fill_brewer(palette="Paired")


# pic=4
ggplot(gen_labeled, aes(x=total_diff, y=cronbach_alpha)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA) +
  facet_wrap(~condition)


# pic=5
gen_labeled %>%
  ggplot(aes(x=total_diff, y=cronbach_alpha, shape=condition, color=condition)) +
  scale_x_continuous(breaks = c(0,1,2,3)) +
  geom_point(size=2) +
  geom_smooth(method = "lm", fill=NA) +
  scale_color_brewer(palette="Paired")


library(tidyr)
x<-gen_labeled %>%
  select(condition, trial, ends_with('diff'), cronbach_alpha, fixed, issue) %>%
  pivot_longer(cols=ends_with('diff'), names_to='diff', values_to='value')

a<-x %>%
  filter(diff %in% c('agent_diff', 'recipient_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA)

#pic6
x %>%
  filter(diff %in% c('agent_diff', 'recipient_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() + labs(x='') +
  scale_x_continuous(breaks = c(0,1,2)) +
  geom_smooth(method = "lm", fill=NA) 

#pic7
x %>%
  filter(diff %in% c('agent_diff', 'recipient_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA) +
  facet_grid(fixed~issue)

b<-x %>%
  filter(diff %in% c('fixed_diff', 'varied_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() + labs(x='') +
  scale_x_continuous(breaks = c(0,1,2)) +
  geom_smooth(method = "lm", fill=NA)


# pic8
x %>%
  filter(diff %in% c('fixed_diff', 'varied_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA) +
  facet_grid(fixed~issue)


# pic9
x %>%
  filter(diff %in% c('total_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA) +
  facet_grid(fixed~issue)

# pic10
ggplot(gen_labeled, aes(x=varied_diff, y=cronbach_alpha)) +
  geom_point() +
  facet_wrap(~condition)

# pic11
x %>%
  filter(diff %in% c('fixed_diff', 'varied_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA)



#### Self-reports ####


# 具体的值还需要在改
a<-ggplot(df.sw, aes(x=issue, fill=content)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(~fixed, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Effect specification') +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette="Paired")
a


a