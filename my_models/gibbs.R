
# Prep hypo table
get_hypo_posts<-function(cond, task_source=tasks, hypo_source=df.hypos) {
  task_obs<-tasks%>%filter(condition==cond&phase=='learn')%>%select(agent, recipient, result)
  df<-hypo_source%>%select(hypo, prior)
  
  for (i in seq(nrow(task_obs))) {    # i in seq(6) 6个learning example
    d<-paste(task_obs[i,], collapse=',')
    post_col<-paste0('post_',i)
    df[,post_col]<-mapply(get_likeli, df$hypo, rep(d, nrow(df))) # likelihoods
    df[,post_col]<-df[,post_col]*df$prior
    df[,post_col]<-normalize(df[,post_col])   #   
    #（对于某一个观察到的learning example，在已知condition的情况下，可能的causal law，以及其所对应的概率）
  }
  
  df$condition=cond
  return(df)
}
#df.posts<-get_hypo_posts('A1')
#for (i in 2:4) df.posts<-rbind(df.posts, get_hypo_posts(paste0('A',i)))
# save(df.hypos, df.posts, file='hypos.Rdata')

run_gibbs_sampler<-function(cond, grouping, alpha, beta, limit, logging=T, hypo_source=df.posts) {
  # Task setup
  task_obs<-tasks%>%filter(condition==cond&phase=='learn')%>%select(agent, recipient, result)
  hypos<-hypo_source%>%filter(condition==cond)
  #non_empty<-hypos%>%filter(post_1>0&post_2>0&post_3>0&post_4>0&post_5>0&post_6>0)

  # Pre-calculated values
  nobs<-nrow(task_obs)   #new objects有多少个learning example
  join_new<-alpha/(nobs-1+alpha)   #  CRP中新category的概率
  feats<-list()   # features 字典列，key为第几个learning example，value为该learning example所包含的feature值
  for (i in seq(nobs)) feats[[i]]<-read_data_feature(as.list(task_obs[i,]), grouping)  # 传入的data是一次learning exnample的a，r，m
  # Helper function that calculates mean-feature similarity using feats
  
  
  # dirichelet likelihood
  dir_likeli<-function(ob_idx, cat_obs_idx) {    
    # ob_idx是当前的learning example的indx
    # cat_obs_idx 是不同于当前的learning example其他learning example的indx
    # dir_likeli方程计算的是learning example中出现的feature数量占其他learning example中出现的feature数量的比例
    # 思路是通过feature的数量来判断当前learning example与其他learning example是否相似
    cat_feat<-init_feat_dist(beta)
    for (i in cat_obs_idx){
      cat_feat<-Map('+', cat_feat, feats[[i]])   # 将几个feature list加在一起构成cat_feat
    } 
    found<-Map('*', cat_feat, feats[[ob_idx]])   # feats[ob_idx]
    return(Reduce('+', found)/Reduce('+', cat_feat))  #Reduce函数：它是将一个向量按相邻两个元素依次作二元函数运算，最后输出结果
  }
  
  # initialize
  states<-list()
  func_refs<-list()  # 将装着所有采样得到的causal laws
  print(paste0('Start sampling ', limit, ' iterations of ', cond))
  
  
  # Initalization
  state<-rep('c1', nobs)  # 重复输出nobs次，c1 代表category 1，6个learning example，最多也只有6个category
  func_refs[['c1']]<-sample(hypos$hypo, 1, prob=hypos$prior)   #sample(x=data,size=5,replace=T)，这里根据先验概率来采样
  # state<-paste0('c', seq(nobs))
  # funcs<-sample(non_empty$hypo, nobs, prob=non_empty$prior, replace=F)
  # for (i in seq(nobs)) func_refs[[state[i]]]<-funcs[i]

  n<- 1
  while (n<limit) {
    to_update<- n %% 6  # 求余
    
    to_update<- if (to_update==0) 6 else to_update  # 1 2 3 4 5 6 
    obs_to_update<-as.list(task_obs[to_update,])  # 第1～6个learning example
    self_resemblance<-dir_likeli(to_update, to_update)
    
    other_idx<-setdiff(seq(nobs), to_update)   #求向量x与向量y中不同的元素(只取x中不同的元素) ，即除了to_update外其他元素
    
    
    
    
    
    #～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～#
    # 接下来的部分是对于采样的causal law分配到具体某一个category
    #～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～#
    # sample new function(s) for this obs from conditional probablity
    # 对于当前的learning example,我们根据后验概率随机抽取一个causal law
    post_col<-paste0('post_',to_update)  #  post_1
    funcs_pool<-hypos$hypo[which(hypos[,post_col]>0)]  #所有后验概率大于0的causal law
    funcs_post<-hypos[which(hypos[,post_col]>0), post_col]   #这些causal law的后验概率
    
    new_funcs<-sample(funcs_pool, 1, prob=funcs_post)   # 随机sample一个
    
    
    # whether the sampled func belongs to an existing category
    # 判断随机抽取的causal law是否存在在已有的类别当中，若果没有则创建一个新的类
    # 看上去抽了10000次，其实也就400+个causal law常见
    new_cats<-c()
    for (f in unique(new_funcs)) {  # unique的操作等于set()
      checks<-sapply(func_refs, function(x) x==f)
      if (T %in% checks) { # belongs to an existing category
        cat<-names(func_refs)[which(checks)]  #返回checks = 1的那个下标，如果在现有的func_refs里找到了这个这个类
      } else {
        cat<-paste0('c', length(func_refs)+1)   #length(func_refs) 是现有的category的数量
        func_refs[[cat]]<-f  # 将这个判定为新category的f加入进去
      }
      if (!(cat %in% state)) new_cats<-c(new_cats, cat)
    }
    # treat single-unique existing to-update category as joining a new category
    if (!(state[to_update] %in% state[other_idx])) {
      new_cats<-c(new_cats, state[to_update])
    }
    new_cats<-unique(new_cats)   # new category
    
    
    
    
    
    #～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～#
    # 接下来的部分是对于当前的learning example（obj_to_update）判断分配到具体某一个category
    #～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～～#
    # Check for assigning learning example to existing categories
    # 
    propto<-list()
    for (s in unique(state[other_idx])) {#unique(state[other_idx])是指除了当前learning sample之外，其他example被分配到的category
      # Chinese restaurant process
      join_this<-length(which(state[other_idx]==s))/(nobs-1+alpha)   # ，其他example被分配到s这个category的概率
      # Dirichlet on feature similarity
      # setdiff(x,y),找到y里面没有包含的x的元素。属于 category为s的learning example，找到这些learning exampls不同于to_update的那些
      #
      resemblance<-dir_likeli(to_update, setdiff(which(state==s), to_update))  
      # Causal function
      likeli<-get_likeli(func_refs[[s]], obs_to_update)  #func_refs[[s]]是被分配到s这个category的causal laws
      #obs_to_update是当前的learning example
      # Put together
      propto[[s]]<-join_this*resemblance*likeli  # 分配到已有category的概率
    }
    # Or learning example assign to a new one(s)
    if (length(new_cats)>0) {
      for (cat in new_cats) {
        post_likeli<-get_likeli(func_refs[[cat]], obs_to_update)
        propto[[cat]]<-join_new*self_resemblance*post_likeli   # # 分配到新的category的概率
      } 
    }
    
    
    
    
    
    
    if (Reduce('+', propto)==0) {
      print('Warining: zero sum!')
      next
    } else {
      # Filter out zero probs to avoid sample() error
      t<-propto[unlist(lapply(propto, function(x) x>0))]
      t<-normalize(t)
      # Sample new category
      # 如果只有一个category，那就直接赋值，如果有多个，则随机采样
      sampled<-if (length(t)==1) names(t)[1] else sample(names(t), 1, prob=unlist(t))
      state[to_update]<-sampled
      if(logging == T )print(paste0(n, ': ', 'sampling ', to_update, ' | ', 
                                paste(state, collapse=',')))
      # Save everything for developing
      # Play with burn-in and thinning in the pred.R script
      # For the final version do built-in burn-in and thinning here
      states[[n]]<-state 
    }
    
    
    
    
    # Go to the next iteration
    n<-n+1
  }


  return(list(state=states, funcs=func_refs))
}

# x<-run_gibbs_sampler('A1', 'A', 1, 1/9, 10000, T)
# df<-data.frame(matrix(unlist(x[[1]]), nrow=length(x[[1]]), byrow=T))
# df<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())%>%ungroup()









