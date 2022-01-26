rm(list=ls())
setwd("/Users/xiazeyu/Desktop/github_local/MSCproject/my_models")
library("rjson")
json_file = "../js/configs/configurations2.json"   
json_data = fromJSON(file=json_file)


csv_data<-data.frame(
condition=numeric(0), phase=numeric(0), task=numeric(0), agent=numeric(0), 
  recipient=numeric(0), result=numeric(0), sid=numeric(0), indicator = numeric(0)
)


for (n in 1:length(json_data)) {
  list = json_data[[n]]
  csv_data[n, 'condition']<-json_data[[n]]['group']
  csv_data[n, 'phase']<-json_data[[n]]['phase']
  csv_data[n, 'task']<-json_data[[n]]['trial']
  csv_data[n, 'agent']<-json_data[[n]]['agent']
  csv_data[n, 'recipient']<-json_data[[n]]['recipient']
  csv_data[n, 'result']<-json_data[[n]]['result']
  csv_data[n, 'sid']<-json_data[[n]]['sid']
  csv_data[n, 'indicator']<-json_data[[n]]['indicator']
}

#write.csv(csv_data, './Rdata/mturk/main.csv')

experiment_json_file = "../Amazon_mturk/exp_group1.json"
experiment_json_data = fromJSON(file=experiment_json_file)


df.sw_all =  data.frame(
  ix=numeric(0), id=numeric(0), initial_input=numeric(0), correct=numeric(0),condition =numeric(0),issue = numeric(0),fixed = numeric(0)  )
temp = data.frame(
  ix=numeric(0), id=numeric(0), phase=numeric(0), tid =numeric(0), sid=numeric(0),agent=numeric(0),
  recipient=numeric(0), result=numeric(0), condition=numeric(0))
df.tw_all = data.frame(
  ix=numeric(0), id=numeric(0), phase=numeric(0), tid =numeric(0), sid=numeric(0),agent=numeric(0),
  recipient=numeric(0), result=numeric(0), condition=numeric(0))

for (n in 1:length(experiment_json_data)) {
  experiment_result = experiment_json_data[[n]][["experimentResult"]]
  subject_data = experiment_result[["subject"]]
  trial_data = experiment_result[["trialData"]]
  df.sw_all[n,"ix"] = n
  df.sw_all[n,"id"] = experiment_json_data[[n]]["sessionId"]
  df.sw_all[n,"initial_input"] = subject_data["initial_input"]
  df.sw_all[n,"correct"] = subject_data["correct"]
  df.sw_all[n,"condition"]  = subject_data["condition"]
  
  cond = subject_data["condition"]
  if (cond=="A4"||cond=="A8") issue_value = "original"
  else if (cond=="A1"||cond=="A5") issue_value = "statechange"
  else if (cond=="A2"||cond=="A6") issue_value = "movement"
  else if (cond=="A3"||cond=="A7") issue_value = "indicator"
  
  if (cond %in% c("A1","A2","A3","A4")) fixed_value = "fixed_A"
  else if (cond %in% c("A4","A5","A6","A7")) fixed_value = "fixed_R"
  
  df.sw_all[n, 'issue'] = issue_value
  df.sw_all[n, 'fixed'] = fixed_value
  
  # 处理tw

  sidlist = trial_data[["sid"]]
  agentlist = trial_data[["agent"]]
  recipientlist = trial_data[["recipient"]]
  resultlist = trial_data[["result"]]
  predictlist = trial_data[["predicted"]]
  resultlist = rbind(resultlist,predictlist)
  
  num_of_learn = 0
  for( m in 1:length(sidlist)){
    phase  = if(substr(sidlist[m],4,4)=="g") "gen" else "learn" 
    if (phase=="learn"){
      num_of_learn = num_of_learn+1
    }
    temp[m,"ix"] = n
    temp[m,"id"] = experiment_json_data[[n]]["sessionId"]
    temp[m,"phase"] = if(num_of_learn>6) "gen" else phase
    temp[m,"tid"] = substring(sidlist[m],4,1000000)
    temp[m,"sid"] = sidlist[m]
    temp[m,"agent"] = agentlist[m]
    temp[m,"recipient"] = recipientlist[m]
    temp[m,"result"] =resultlist[m]
    temp[m,"condition"] = subject_data["condition"]
    
  }
  temp = temp%>%filter(duplicated(temp$sid)==FALSE)
  df.tw_all<-rbind(df.tw_all,temp)
}

df.tw = df.tw_all
df.sw = df.sw_all



save(df.tw, df.sw, file='./Rdata/mturk/mturk_main.Rdata')
#save(df.tw, df.sw, file='./Rdata/mturk/mturk_raw.Rdata')


# 有新数据的时候必须要重写
#write.csv(df.sw,file='./Rdata/mturk/response.csv')
df.sw_marked = read.csv("./Rdata/mturk/response1.csv")[,c(2,3,4,5,6,7,8,9)]

quality_threshold = 1

#df.sw_filtered = df.sw_all%>%filter(df.sw_all$correct>quality_threshold)

df.sw_filtered = df.sw_marked%>%filter(df.sw_marked$content != "meaningless")

df.tw_filtered = left_join(df.sw_filtered,df.tw_all)
df.tw_filtered = df.tw_filtered[,-3]

df.tw = df.tw_filtered
df.sw = df.sw_filtered
save(df.sw, df.tw, file='./Rdata/mturk/mturk_filtered.Rdata')


df.sw_filtered%>%
  group_by(condition)%>%
  summarize(count=n())

