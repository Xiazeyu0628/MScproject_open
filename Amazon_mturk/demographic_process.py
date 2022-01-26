import csv
import pandas as pd
import json

df = pd.DataFrame(columns = ['token','age','sex','condition','task_duration','correct'])

experiment_dict = {}

with open('exp_group1.json','r',encoding='utf8')as fp:
    experiment_data = json.load(fp)


for item in experiment_data:
    temp = {}
    experiment_data = item["experimentResult"]
    subject_data = experiment_data["subject"]
    temp["token"] = item["sessionId"]
    temp["age"] = age = subject_data["age"]
    temp["sex"] = subject_data["sex"]
    temp["condition"] = subject_data["condition"]
    temp["task_duration"] = subject_data['task_duration']
    temp["correct"] = subject_data["correct"]
    df = df.append(temp, ignore_index=True)

df.to_csv('out.csv')











