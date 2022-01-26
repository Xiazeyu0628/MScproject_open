setwd("/Users/xiazeyu/Desktop/github_local/MSCproject/my_models")
source('shared.R')


################################################################
# Generate universal effects
generate_hypo<-function() {
  pcfg<-function(s, role) {
    s<-gsub('S', sample(c('', 'and(S, S)', 'F(X)LYT'), 1), s)
    
    x_vals<-if (role=='cause') c('A', 'R') else 'M'
    s<-sub('X', sample(x_vals,1), s)
    
    f_drawn<-sample(names(feature_setting), 1)
    s<-sub('F', f_drawn, s)
    
    s<-sub('L', sample(c('==', '!=', '>', '<'), 1, prob=c(1, .2, .2, .2)), s)
    
    s<-sub('Y', sample(c('V', 'O'), 1), s)
    
    s<-sub('V', sample(feature_setting[[f_drawn]], 1), s)
    
    o_drawn<-sample(c('A', 'R'), 1)
    o_drawn_formatted<-paste0(f_drawn, '(', o_drawn, ')')
    s<-sub('O', o_drawn_formatted, s)
    
    s<-sub('T', sample(c('+1', '-1', ''), 1), s)
    
    if (grepl('S|X|F|L|Y|V|O|T',s)) return(pcfg(s, role)) else return(s)
  }
  
  # cause<-pcfg('S', 'cause')
  # effect<-pcfg('S', 'effect')
  # 
  # return(paste0("list(cause='",cause,"',effect='",effect,"')"))
  return(paste0("list(cause='',effect='", pcfg('S', 'effect'), "')"))
}

#effects<-list()
#for (i in seq(100000)) effects[[i]]<-generate_hypo()
#effects<-unique(effects)
# save(effects, file='effects.Rdata')
load("./Rdata/effects.Rdata")

# Prior
library(stringr)

pcfg_prior<-function(hypo) {
  get_prior<-function(x) {
    # count ands
    n_and<-str_count(x, 'and')
    # count ==s
    n_eq<-str_count(x, '==')
    # count sub sentence
    x<-gsub('and\\(', '', x)
    x<-gsub(' ', '', x)
    ss<-strsplit(x, ',')[[1]]
    n_drawn<-length(ss)
    n_neq<-n_drawn-n_eq
    # count relative picks
    rels<-0
    for (s in ss) {
      obj<-strsplit(s, '==|!=|>|<')[[1]][2]
      if (!is.na(obj)&nchar(obj)>5) rels<-rels+1
    }
    vals<-n_drawn-rels
    
    
    # 0.125 = 0.2/1.6
    # 1/1.6 = 0.625
    
    return((1/3)^n_and*0.625^n_eq*0.125^n_neq*0.5^rels*0.2^vals)
  }
  h<-eval(parse(text=hypo))
  return(get_prior(h$cause)*get_prior(h$effect))
}

# systematically generate basic ones
g_edges<-c()
for (r in c('==', '!=', '>', '<')) {
  for (m in c('3', '4', '5', '6', '7', 
              'edges(A)', 'edges(R)', 
              'edges(A)+1', 'edges(R)+1', 'edges(A)-1', 'edges(R)-1')) {
    if (!((r=='>' & m=='7') | (r=='<' & m=='3')))
      g_edges<-c(g_edges, paste0('edges(M)', r, m))
  }
}
g_shades<-c()
for (r in c('==', '!=', '>', '<')) {
  for (m in c('1', '2', '3', '4', 
              'shades(A)', 'shades(R)', 
              'shades(A)+1', 'shades(R)+1', 'shades(A)-1', 'shades(R)-1')) {
    if (!((r=='>' & m=='4') | (r=='<' & m=='1')))
      g_shades<-c(g_shades, paste0('shades(M)', r, m))
  }
}
combos<-c()
for (e in g_edges) {
  for (s in g_shades) {
    combos<-c(combos, (paste0('and(', e, ', ', s, ')')))
  }
}


# unify them, get prior, clean up
hypos<-c(g_edges, g_shades, combos)
df.hypos<-data.frame(hypo=hypos) %>%
  mutate(hypo=paste0("list(cause='',effect='", hypo, "')"))
df.hypos$prior<-mapply(pcfg_prior, df.hypos$hypo)
df.hypos$prior<-normalize(df.hypos$prior)
save(df.hypos, file='hypos.Rdata')



