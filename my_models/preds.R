summarise
# Helpers ####
read_cats<-function(states_source, burn_in=0, thinning=1, base='') {
 
  df<-data.frame(matrix(unlist(states_source), nrow=length(states_source), byrow=T))
  # Burn in: filter out first n samples
  #seq(from,to,length),该函数的意思是生成一组数字，从from开始，到to结束，每两个数间的间隔是length
  df$i<-seq(nrow(df))
  df<-df%>%filter(i>burn_in) # filter的意思是不满足条件的都被filter掉
  # Thinning: keep very nth sample
  df$i<-seq(nrow(df))
  df<-df%>%filter(i%%thinning==0)
  # Get unique categories
  df<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n(),.groups = 'drop')%>%ungroup()
  cats<-cbind(df%>%select(starts_with('X'))%>%mutate_all(as.character), 
              df%>%select(n))%>%
    mutate(prob=normalize(n))
  # Apply softmax
  if (base!='') cats$prob<-softmax(cats$prob, base)
  return(cats)
}
prep_preds<-function(funcs, cond) {
  # gibss采样的结果包含着每一个category对应的causal law
  # all_preds中包含着 每一个causal law对应的每一个gen trial的result obj的likelihood
  # prep_preds function的作用就是将category和每一个gen trial的result obj的likelihood联系起来
  
  preds<-list()
  cond_idx<-as.numeric(substr(cond, 2, 2)) #substr(s, first, last)  # A1 -> 1
  for (f in names(funcs)) {    # f是category
    preds[[f]]<-list()
    h<-funcs[[f]]     # h 是 causal law
    for (d in 1:n_gen_obs) {    # 1:16  d代表的是第几个gen obj
      preds[[f]][[d]]<-all_preds[[h]][[cond_idx]][[d]]
    }
  }
  return(preds)
}

# Get predictions dataframe for a condition
get_cond_preds<-function(cond, learned_cats, func_preds, alpha, beta, grouping) {
  # Shared values
  learn_tasks<-tasks%>%filter(condition==cond&phase=='learn')%>%select(agent, recipient)
  gen_tasks<-tasks%>%filter(condition==cond&phase=='gen')%>%select(agent, recipient)
  
  # Functions to get predictions
  
  # One c_i makes prediction for a gen task
  #实际上是对一个category makes prediction for a gen task
  pred_by_group<-function(tid, group_func, group_idx) {  #pred_by_group(1, “c101”, 1)
    # Causal function
    preds<-func_preds[[group_func]][[tid]]  # 找到当前category下的gen trial，返回其result obj likelihood
    group_size<-length(group_idx)  # 查看当前group里有几个indx
    task<-as.list(gen_tasks[tid,])
    if (group_size==n_learn_obs) {  # 如果6个learning examples全是一个category
      return(preds)
    } else {
      # Chinese restaurant process
      crp<-group_size/(n_learn_obs-1+alpha)  #
      
      group_feats<-init_feat_dist(beta)  # group features
      for (i in group_idx) {
        obs_feats<-read_data_feature(as.list(learn_tasks[i,]), grouping)
        group_feats<-as.list(unlist(group_feats)+unlist(obs_feats))
      } 
      # Dirichlet on feature similarity
      dir_ll<-Reduce('+', Map('*', read_data_feature(task, grouping), group_feats))/Reduce('+',group_feats)
      
      preds<-lapply(preds, function(x) x*crp*dir_ll)
      return(preds)
    }
  }
  
  # One cat prediction
  #实际上是计算个一个group里的多个category的prediction，并将最后的值加在一起
  pred_by_cat<-function(tid, cond_groups) {   # cond_groups = c101,c173,c20,c24,c101....
    groups<-list()
    for (g in unique(cond_groups)) groups[[g]]<-which(cond_groups==g) # indices
    preds<-init_dist()
    for (i in 1:length(groups)) {
      group_func<-names(groups)[i]; 
      group_idx<-groups[[i]]
      intermedia_2 = pred_by_group(tid, group_func, group_idx)# pred_by_group(1, “c101”, 1)
      preds<-Map('+', preds,intermedia_2 )
    }
    return(normalize(preds))
  }
  
  # All cats predictions
  #把所有的group求prediction，并最后加在一起
  pred_by_task<-function(tid){
    preds<-init_dist()
    for (i in 1:nrow(learned_cats)) {  # 6个learning example有多少种可能的category的组合
      cond_groups<-unlist(learned_cats[i, seq(n_learn_obs)]) # n_learn_obs = 6  cond_groups = c101,c173,c20,c24,c40
      cond_prob<-learned_cats[i, 'prob']
      intermedia_1  = pred_by_cat(tid, cond_groups)
      cat_preds<-Map('*', intermedia_1, cond_prob)
      preds<-Map('+', preds, cat_preds)
    }
    # return(normalize(preds)) # should be good
    # return(preds)
    # Add a noise term to avoid empty entries
    noisy_preds<-sapply(preds, function(x) x+abs(rnorm(1,0,.001)))
    return(normalize(noisy_preds))
  }
  
  # Format prediction for one task into a dataframe
  # 对一次gen trial 求prediction
  format_task_preds<-function(tid) {
    x<-pred_by_task(tid)
    df<-data.frame(object=names(x), prob=unlist(x)); rownames(df)<-c()
    df<-df%>%mutate(object=as.character(object))%>%
      mutate(group=cond, trial=tid)%>%
      select(group, trial, object, prob)
    return(df)
  }
  
  # Get predictions
  df<-format_task_preds(1)  # 1 就是第1个gen obs
  for (i in 2:n_gen_obs) df<-rbind(df, format_task_preds(i))
  
  return(df)
}

data_likeli<-function(b, data) {
  ds<-filter(data, group=='A1', trial==1) %>% mutate(softmaxed=softmax(prob, b))
  for (c in 1:4) {
    for (i in 1:16) {
      if (!(c==1 & i==1)) {
        ds<-rbind(ds,
                  filter(data, group==paste0('A', c), trial==i) %>% 
                    mutate(softmaxed=softmax(prob, b)))
      }
    }
  }
  ll<-sum(ds$count*log(ds$softmaxed))
  return(-ll)
}

# cats<-read_cats(x[[1]], 500, 1)
# func_preds<-prep_preds(x[[2]], 'A1')
# y<-get_cond_preds("A1", cats, func_preds, 1, 1/9, 'A')

# all_preds<-list()
# for (h in df.hypos$hypo) {
#   all_preds[[h]]<-list()
#   for (t in 1:4) {
#     all_preds[[h]][[t]]<-list()
#     for (i in 1:16) {
#       tk<-tasks%>%
 #        filter(condition==paste0('A',t)&phase=='gen'&task==i)%>%
 #        select(agent, recipient)%>%
#         as.list()
# #      all_preds[[h]][[t]][[i]]<-causal_mechanism(h, tk)
#     }
#   }
# }  
# save(all_preds, file='all_preds.Rdata')

# # Test how long to make a full prediction
# preds<-data.frame(group=character(0),
#                   trial=numeric(0),
#                   object=numeric(0),
#                   prob=numeric(0),
#                   type=character(0))
# x<-results[[1]]
# cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
# func_preds<-prep_preds(x[[2]], 'A1')
# start_pred<-Sys.time()
# y<-get_cond_preds('A1', cats, func_preds, .1, 1/9, 'A')
# done_pred<-Sys.time()
# print(done_pred-start_pred)












