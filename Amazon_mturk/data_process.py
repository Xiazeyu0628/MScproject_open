import csv
import pandas as pd
import json

# with open("exp_group1.csv") as f:
#     fcsv=csv.reader(f)
#     headers = next(fcsv)
#     for row in fcsv:
#         print(row)
experiment_dict = {}

csv_file = "exp_group18.csv"

with open('exp_group1.json','r',encoding='utf8')as fp:
    experiment_data = json.load(fp)

# 将实验数据设计为以token为key的dict

for item in experiment_data:
    token = item["sessionId"]
    experiment_dict[token] = item["experimentResult"]


fullData=pd.read_csv(csv_file)
userData = fullData[["AssignmentId","WorkerId","Answer.surveycode"]]
userData = userData.copy()
userData.loc[:,"bonus"] = 0
userData.loc[:,"reason"] = 0



for index, row in userData.iterrows():
    token = row["Answer.surveycode"]
    causality_guess = experiment_dict[token]["subject"]["initial_input"]
    correct_number = experiment_dict[token]["subject"]["correct"]
    print(causality_guess)
    print("\n")
    print(correct_number)
    Causality_bonus = input("what is the quality for this causality guess ?")
    bonus = round(correct_number*0.1+float(Causality_bonus)*0.5,2)
    userData.loc[index,"bonus"]= bonus

    if float(Causality_bonus)==1:
        guess_response = "you get a correct guess for potential causality. "
    elif float(Causality_bonus)==0.5:
        guess_response = "Your guess about potential causality is incomplete or partially correct. "
    elif float(Causality_bonus) == 0:
        guess_response = "Your guess about potential causality is incorrect. "

    total_response = "you get {} correct prediction and ".format(correct_number)+guess_response
    userData.loc[index,"reason"] = total_response



def generate_script(userData):
    for index, row in userData.iterrows():
        script = "aws mturk send-bonus --worker-id {0} --bonus-amount {1} \
        --assignment-id {2} --reason '{3}'"\
            .format(row["WorkerId"],row["bonus"],row["AssignmentId"],row["reason"])
        print(script)


generate_script(userData)