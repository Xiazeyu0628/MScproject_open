import json



# with open("exp_group1.csv") as f:
#     fcsv=csv.reader(f)
#     headers = next(fcsv)
#     for row in fcsv:
#         print(row)
experiment_dict = {}
count_dict = {}
rest_dict = {}

for i in range(1,9):
    key = "A{}".format(i)
    value = 0
    count_dict[key] = value

with open('exp_group1.json','r',encoding='utf8')as fp:
    experiment_data = json.load(fp)

# 将实验数据设计为以token为key的dict

for item in experiment_data:
    token = item["sessionId"]
    experiment_dict[token] = item["experimentResult"]

#print(experiment_dict)

for item in experiment_dict.values():
    sid = item["trialData"]["sid"]
    condition = sid[0].split("-")[0]
    count_dict[condition] = count_dict[condition] + 1

for key,item in count_dict.items():
    rest_dict[key] = 23-item


print("existing condition value:{}".format(count_dict))
print("rest condition value:{}".format(rest_dict))
