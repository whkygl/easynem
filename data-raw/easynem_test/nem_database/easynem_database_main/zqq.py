#!/usr/bin/env python 
# -*- coding:utf-8 -*-
import requests
from bs4 import BeautifulSoup
import urllib.request
import re
import csv
import pandas as pd
#需求：爬取三国演义小说所有的章节标题和章节内容http://www.shicimingju.com/book/sanguoyanyi.html
if __name__ == "__main__":
    #对首页的页面数据进行爬取
    headers = {
        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.121 Safari/537.36'
    }
    url = 'http://nemaplex.ucdavis.edu/Uppermnus/topmnu.htm'
    page_text = requests.get(url=url,headers=headers).text

    #在首页中解析出章节的标题和详情页的url
    #1.实例化BeautifulSoup对象，需要将页面源码数据加载到该对象中
    soup = BeautifulSoup(page_text,'lxml')
    #解析章节标题和详情页的url
    li_list = soup.select('body > nav > ul > li:nth-child(2) > ul > li:nth-child(2) > ul > li')
    #创建表头
    rows = []
    rows.append(['Nematodes_Genera','Family','Putative_Feeding','c-p_Group','Feeding_Group','Functional_Guild','Internal_Code'])
    # print(rows)
    # fp = open('./easynem_database_main.txt','w',encoding='utf-8')
    for li in li_list:
        title = li.a.string
        detail_url = 'http://nemaplex.ucdavis.edu'+li.a['href']
        detail_url = re.sub(r'\.\.',"",detail_url)
        print(title)
        print(detail_url)
        #对详情页发起请求，解析出章节内容
        detail_page_text = requests.get(url=detail_url,headers=headers).text
        # print(detail_page_text)
        # #解析出详情页中相关的章节内容
        detail_soup = BeautifulSoup(detail_page_text,'lxml')
        # tb = pd.read_html(detail_soup)
        # print(tb)
        # print(detail_soup)
        table_html = detail_soup.find('table')
        result_html = table_html.find_all('tr')
        # result_html = result_html[1:]
        # result_html = result_html.remove(0) #去掉第一行
        # print(result_html)
        # print(result_html)
        for i in result_html:
            data = i.find_all('td')
            #如果该单元格无数据则跳过
            if len(data) != 7:
                continue
            # print(len(data))
            Nematodes_Genera = data[0].getText()
            Family = data[1].getText()
            Putative_Feeding = data[2].getText()
            c_p_Group = data[3].getText()
            Feeding_Group = data[4].getText()
            Functional_Guild = data[5].getText()
            Internal_Code = data[6].getText()
            rows.append([Nematodes_Genera,Family,Putative_Feeding,c_p_Group,Feeding_Group,Functional_Guild,Internal_Code])
    
    # rows = list(filter(lambda x: re.sub(r'\\r | \\n | \\t',"",x),rows))
    # print(rows)
    rows = pd.DataFrame(data=rows)
    # print(rows)
    for j in range(0,3):
        rows[j] = rows[j].apply(lambda x:x.replace('\n', '').replace('\r', ''))
        rows[j] = rows[j].apply(lambda x:x.replace('\t', '').replace('\\', ''))
    # rows["0"].str.replace('\\r' | '\\n' | '\\t',"")
    print(rows)
    rows.to_csv('./easynem_database_main.csv',encoding='utf-8-sig',index=False,header=False)
    # # 筛除easynem_database_main.csv文件中的第二行和第三行
    # rows.to_csv('./easynem_database_main.csv', encoding='utf-8-sig', index=False, header=False)
    # # Exclude the second and third rows from the CSV file
    # df = pd.read_csv('./easynem_database_main.csv')
    # df = df.drop([1, 2])  # Exclude the second and third rows
    # df.to_csv('./easynem_database_main.csv', encoding='utf-8-sig', index=False, header=False)
        # print(detail_soup)
        # div_tag = detail_soup.find('div',class_='chapter_content')
        # #解析到了章节的内容
        # content = div_tag.text
        # fp.write(title+':'+content+'\n')
        # print(title,'爬取成功！！！')
# 用正则表达式删除easynem_database_main.csv文件中包含PutativeFeeding的行及其后7行
file_path = 'easynem_database_main.csv'
with open(file_path, 'r') as file:
    lines = file.readlines()
# 使用正则表达式删除包含 PutativeFeeding 的行及其后7行
new_lines = []
skip_lines = 0
for line in lines:
    if skip_lines > 0:
        skip_lines -= 1
        continue

    if re.search(r'PutativeFeeding', line):
        skip_lines = 7
    else:
        #用正则表达式将line中的空格替换为空
        line = re.sub(r', +', ",", line)
        line = re.sub(r' +,', ",", line)
        #零宽断言
        line = re.sub(r'(?<=[a-zA-Z0-9]) +(?=[a-zA-Z0-9])', "_", line)
        new_lines.append(line)
        
# 将处理后的内容写回文件
with open(file_path, 'w') as file:
    file.writelines(new_lines)





    


