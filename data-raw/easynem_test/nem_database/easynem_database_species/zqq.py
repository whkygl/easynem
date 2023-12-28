import selenium
from selenium import webdriver
# 导入 Select 类
from selenium.webdriver.support.select import Select
from selenium.webdriver.support import ui
from selenium.webdriver.common.by import By
import pandas as pd
import requests
from lxml import etree
import re
import time
# import time
if  __name__ == "__main__":
    cols2 = []
    cols2.append(['Nominal_Family','Genspec','Gender','cp_value','feeding','Functional_guild','Basal_Wtg','Enrich_Wtg','Structure_Wtg','Length_micm','Width_micm','Mass_micg','CPr','CRs','MFP','EFP','SFP','HFP','FFP','BFP','PFP','Measurement_Sources'])
    cols2 = pd.DataFrame(cols2)
    cols2.to_csv('./easynem_database_species.csv',encoding='utf-8-sig',index=False,header=False)
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36 Edg/105.0.1343.27'
        }
    url = 'http://nemaplex.ucdavis.edu/Ecology/EcophysiologyParms/GenusspeciesParmsDDQuery.aspx'
    page_text = requests.get(url = url, headers=headers).text
    tree = etree.HTML(page_text)
    select_list = tree.xpath('//*[@id="DropDownList1"]/option/text()')
    url2 = 'http://10.7.250.8/'
    browser = selenium.webdriver.Chrome()
    browser.maximize_window()
    browser.get(url2)
    # print(select_list)
    for select in select_list:
        # print(select)
        zqq = select.replace(' ', '%20')
        print(zqq)
        detail_url = 'http://nemaplex.ucdavis.edu/Ecology/EcophysiologyParms/GenusspecParmsDDResult.aspx?Genspec='
        full_url = f"{detail_url}{zqq}"
        print(full_url)
        # 这段代码是强制让爬取请求重连，如果连接失败就等待五秒继续连接
        while True:
            try:
                detail_page_text = requests.get(url = full_url, headers=headers,timeout=(30,50),verify=False).text
                break
            except:
                print('Connection refused by the server..')
                print('Let me sleep for 5 seconds')
                print('ZZzzzz...')
                time.sleep(5)
                print('Was a nice sleep, now let me continue...')
                continue
        # print(detail_page_text)
        detail_tree = etree.HTML(detail_page_text)
        # time.sleep(5)
        # print(detail_tree)
        browser.refresh()
        Nominal_Family = detail_tree.xpath('//*[@id="GridView2"]//td[1]/text()')
        # print(Nominal_Family[0])
        Genspec = detail_tree.xpath('//*[@id="GridView2"]//td[2]/text()')
        Gender = detail_tree.xpath('//*[@id="GridView2"]//td[3]/text()')
        cp_value = detail_tree.xpath('//*[@id="GridView2"]//td[4]/text()')
        # print(cp_value[0])
        feeding = detail_tree.xpath('//*[@id="GridView2"]//td[5]/text()')
        Functional_guild = detail_tree.xpath('//*[@id="GridView2"]//td[6]/text()')
        Basal_Wtg = detail_tree.xpath('//*[@id="GridView2"]//td[7]/text()')
        Enrich_Wtg = detail_tree.xpath('//*[@id="GridView2"]//td[8]/text()')
        Structure_Wtg = detail_tree.xpath('//*[@id="GridView2"]//td[9]/text()')
        Length_micm = detail_tree.xpath('//*[@id="GridView2"]//td[10]/text()')
        Width_micm = detail_tree.xpath('//*[@id="GridView2"]//td[11]/text()')
        Mass_micg = detail_tree.xpath('//*[@id="GridView2"]//td[12]/text()')
        CPr = detail_tree.xpath('//*[@id="GridView2"]//td[13]/text()')
        CRs = detail_tree.xpath('//*[@id="GridView2"]//td[14]/text()')
        MFP = detail_tree.xpath('//*[@id="GridView2"]//td[15]/text()')
        EFP = detail_tree.xpath('//*[@id="GridView2"]//td[16]/text()')
        SFP = detail_tree.xpath('//*[@id="GridView2"]//td[17]/text()')
        HFP = detail_tree.xpath('//*[@id="GridView2"]//td[18]/text()')
        FFP = detail_tree.xpath('//*[@id="GridView2"]//td[19]/text()')
        BFP = detail_tree.xpath('//*[@id="GridView2"]//td[20]/text()')
        PFP = detail_tree.xpath('//*[@id="GridView2"]//td[21]/text()')
        Measurement_Sources = detail_tree.xpath('//*[@id="GridView2"]//td[22]/text()')
        magnet_link = detail_tree.xpath('//*[@id="GridView2"]//td[22]/text()')
        Measurement_Sources =[x.replace("\n","") for x in magnet_link]
        print(Measurement_Sources)
        # cols.append([Nominal_Family,Genspec,Gender,cp_value,feeding,Functional_guild,Basal_Wtg,Enrich_Wtg,Structure_Wtg,Length_micm,Width_micm,Mass_micg,CPr,CRs,MFP,EFP,SFP,HFP,FFP,BFP,PFP,Measurement_Sources])
        # print(detail_tr)
        cols = pd.DataFrame({"Nominal_Family":Nominal_Family,"Genspec":Genspec,"Gender":Gender,"cp_value":cp_value,"feeding":feeding,"Functional_guild":Functional_guild,"Basal_Wtg":Basal_Wtg,"Enrich_Wtg":Enrich_Wtg,"Structure_Wtg":Structure_Wtg,"Length_micm":Length_micm,"Width_micm":Width_micm,"Mass_micg":Mass_micg,"CPr":CPr,"CRs":CRs,"MFP":MFP,"EFP":EFP,"SFP":SFP,"HFP":HFP,"FFP":FFP,"BFP":BFP,"PFP":PFP,"Measurement_Sources":Measurement_Sources})
        # print(cols)
        cols.to_csv('./easynem_database_species.csv',mode='a',encoding='utf-8-sig',index=False,header=False)
        # print(detail_tr)
        # detail_td1 = detail_tree.xpath('//*[@id="GridView2"]//tr[2]/td//text()')
        # detail_td2 = detail_tree.xpath('//*[@id="GridView2"]//tr[3]/td//text()')
        # print(detail_td1)
        # print(detail_td[1])
    browser.close()



