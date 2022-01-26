rm(list = ls())
load('../my_models/Rdata/mturk/mturk_filtered.Rdata')
df.tw_A2 = df.tw %>%filter(condition == "A2")


all_objects<-c()
for (s in 3:7) {
  for (c in 1:4) {
    all_objects<-c(all_objects, s*10+c)
  }
}

default<-expand.grid(
  group=paste0('A',1:8), trial=1:16, object=as.character(all_objects),
  stringsAsFactors =F)

counts<-df.tw_A2 %>%
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

counts_A2 = counts %>%filter(group == "A2")

ggplot(counts_A2, aes(x=object, y=trial, fill=freq)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~group)

save(df.tw_A2,counts_A2,file = "A2.Rdata" )
