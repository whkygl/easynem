import selenium
from selenium import webdriver
# 导入 Select 类
from selenium.webdriver.support.select import Select
from selenium.webdriver.support import ui
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup
import pandas as pd
import time
if __name__ == "__main__":
    wait_time = 180
    rows = []
    rows.append(['Genus','cp_value','feeding','Basal_Wtg','Enrich_Wtg','Structure_Wtg','Ngenus','GenavgMass','GenavgCPr','GenavgCRs','GenavgMFP','GenavgEFP','GenavgSFP','GenavgHFP','GenavgFFP','GenavgBFP','GenavgPFP',
        'StderrMass','StderrCPr','StderrCRs','StderrMFP','StderrEFP','StderrSFP','StderrHFP','StderrFFP','StderrBFP','StderrPFP'])
    url = 'http://nemaplex.ucdavis.edu/Ecology/EcophysiologyParms/GenusParmsQuery.aspx'
    browser = selenium.webdriver.Chrome()
    browser.maximize_window()
    browser.get(url)
    # browser.implicity_wait(20)
    select_list = browser.find_elements(By.XPATH,'//*[@id="DropDownList3"]/option')
    button = browser.find_element(By.XPATH,'//*[@id="form1"]/table/tbody/tr[5]/td[2]')
    button.click()
    col1 = browser.find_elements(By.XPATH,'//*[@id="DetailsView2"]/tbody/tr/td[2]')
    Genus = col1[0].text
    cp_value = col1[1].text
    feeding = col1[2].text
    Basal_Wtg = col1[3].text
    Enrich_Wtg = col1[4].text
    Structure_Wtg = col1[5].text
    Ngenus = col1[6].text
    col2 = browser.find_elements(By.XPATH,'//*[@id="DetailsView1"]/tbody/tr/td[2]')
    GenavgMass = col2[0].text
    GenavgCPr = col2[1].text
    GenavgCRs = col2[2].text
    GenavgMFP = col2[3].text
    GenavgEFP = col2[4].text
    GenavgSFP = col2[5].text
    GenavgHFP = col2[6].text
    GenavgFFP = col2[7].text
    GenavgBFP = col2[8].text
    GenavgPFP = col2[9].text
    col3 = browser.find_elements(By.XPATH,'//*[@id="DetailsView3"]/tbody/tr/td[2]')
    StderrMass = col3[0].text
    StderrCPr = col3[1].text
    StderrCRs = col3[2].text
    StderrMFP = col3[3].text
    StderrEFP = col3[4].text
    StderrSFP = col3[5].text
    StderrHFP = col3[6].text
    StderrFFP = col3[7].text
    StderrBFP = col3[8].text
    StderrPFP = col3[9].text
    rows.append([Genus,cp_value,feeding,Basal_Wtg,Enrich_Wtg,Structure_Wtg,Ngenus,GenavgMass,GenavgCPr,GenavgCRs,GenavgMFP,GenavgEFP,GenavgSFP,GenavgHFP,GenavgFFP,GenavgBFP,GenavgPFP,
        StderrMass,StderrCPr,StderrCRs,StderrMFP,StderrEFP,StderrSFP,StderrHFP,StderrFFP,StderrBFP,StderrPFP])
    browser = selenium.webdriver.Chrome()
    browser.maximize_window()
    browser.get(url)
    # select_list = browser.find_elements(By.XPATH,'//*[@id="DropDownList3"]/option')
    # button = browser.find_element(By.XPATH,'//*[@id="form1"]/table/tbody/tr[5]/td[2]')
    # 找到下拉菜单元素
    # select = Select(browser.find_element(By.NAME,'DropDownList3'))
    # len(select_list)
    for i in range(1,len(select_list)+1):
        select = Select(browser.find_element(By.NAME,'DropDownList3'))
        select.select_by_index(i)
        wait = ui.WebDriverWait(browser, wait_time)
        try:
            wait.until(lambda driver: driver.find_element(By.XPATH,'//*[@id="form1"]/table/tbody/tr[5]/td[2]'))
            button = browser.find_element(By.XPATH,'//*[@id="form1"]/table/tbody/tr[5]/td[2]')
        except Exception as error1:
            print(error1)
            button = browser.find_element(By.XPATH,'//*[@id="form1"]/table/tbody/tr[5]/td[2]')
            time.sleep(10)
        button.click()
        # browser.execute_script("arguments[0].click();", button)
        # browser.back()
        col1 = browser.find_elements(By.XPATH,'//*[@id="DetailsView2"]/tbody/tr/td[2]')
        Genus = col1[0].text
        cp_value = col1[1].text
        feeding = col1[2].text
        Basal_Wtg = col1[3].text
        Enrich_Wtg = col1[4].text
        Structure_Wtg = col1[5].text
        Ngenus = col1[6].text
        col2 = browser.find_elements(By.XPATH,'//*[@id="DetailsView1"]/tbody/tr/td[2]')
        GenavgMass = col2[0].text
        GenavgCPr = col2[1].text
        GenavgCRs = col2[2].text
        GenavgMFP = col2[3].text
        GenavgEFP = col2[4].text
        GenavgSFP = col2[5].text
        GenavgHFP = col2[6].text
        GenavgFFP = col2[7].text
        GenavgBFP = col2[8].text
        GenavgPFP = col2[9].text
        col3 = browser.find_elements(By.XPATH,'//*[@id="DetailsView3"]/tbody/tr/td[2]')
        StderrMass = col3[0].text
        StderrCPr = col3[1].text
        StderrCRs = col3[2].text
        StderrMFP = col3[3].text
        StderrEFP = col3[4].text
        StderrSFP = col3[5].text
        StderrHFP = col3[6].text
        StderrFFP = col3[7].text
        StderrBFP = col3[8].text
        StderrPFP = col3[9].text
        rows.append([Genus,cp_value,feeding,Basal_Wtg,Enrich_Wtg,Structure_Wtg,Ngenus,GenavgMass,GenavgCPr,GenavgCRs,GenavgMFP,GenavgEFP,GenavgSFP,GenavgHFP,GenavgFFP,GenavgBFP,GenavgPFP,
            StderrMass,StderrCPr,StderrCRs,StderrMFP,StderrEFP,StderrSFP,StderrHFP,StderrFFP,StderrBFP,StderrPFP])
        if Genus == 'Zygotylenchus':
            break
        browser.back()
        # browser = selenium.webdriver.Chrome()
        # browser.get(url)
        # # browser.implicity_wait(20)
        # select_list = browser.find_elements(By.XPATH,'//*[@id="DropDownList3"]/option')
        # # 找到下拉菜单元素
        # select = Select(browser.find_element(By.NAME,'DropDownList3'))
    zqq = pd.DataFrame(data=rows)
    zqq.to_csv('easynem_database_genus.csv',encoding='utf-8-sig',header=False,index=False)
    browser.close()
    # text = browser.page_source
    # # print(text)
    # soup = BeautifulSoup(text,'lxml')
    # print(soup)
    # table_html1 = soup.select('#DetailsView2 > tbody')
    # print(table_html1)
    # result_html1 = table_html1.find_all('tr')
    # data1 = result_html1.find_all('td')[1]
    # Genus = data1[0].getText()
    # print(Genus)
    # cp_value = data1[1].getText()
    # feeding = data1[2].getText()
    # Basal_Wtg = data1[3].getText()
    # Enrich_Wtg = data1[4].getText()
    # Structure_Wtg = data1[5].getText()
    # Ngenus = data1[6].getText()
    # table_html2 = soup.select('#DetailsView1 > tbody')
    # result_html2 = table_html2.find_all('tr')
    # data2 = result_html2.find_all('td')[1]
    # GenavgMass = data2[0].getText()
    # GenavgCPr = data2[1].getText()
    # GenavgCRs = data2[2].getText()
    # GenavgMFP = data2[3].getText()
    # GenavgEFP = data2[4].getText()
    # GenavgSFP = data2[5].getText()
    # GenavgHFP = data2[6].getText()
    # GenavgFFP = data2[7].getText()
    # GenavgBFP = data2[8].getText()
    # GenavgPFP = data2[9].getText()
    # table_html3 = soup.select('#DetailsView3 > tbody')
    # result_html3 = table_html3.find_all('tr')
    # data3 = result_html3.find_all('td')[1]
    # StderrMass = data3[0].getText()
    # StderrCPr = data3[1].getText()
    # StderrCRs = data3[2].getText()
    # StderrMFP = data3[3].getText()
    # StderrEFP = data3[4].getText()
    # StderrSFP = data3[5].getText()
    # StderrHFP = data3[6].getText()
    # StderrFFP = data3[7].getText()
    # StderrBFP = data3[8].getText()
    # StderrPFP = data3[9].getText()
    # rows.append([Genus,cp_value,feeding,Basal_Wtg,Enrich_Wtg,Structure_Wtg,Ngenus,GenavgMass,GenavgCPr,GenavgCRs,GenavgMFP,GenavgEFP,GenavgSFP,GenavgHFP,GenavgFFP,GenavgBFP,GenavgPFP,
    #     StderrMass,StderrCPr,StderrCRs,StderrMFP,StderrEFP,StderrSFP,StderrHFP,StderrFFP,StderrBFP,StderrPFP])
    # print(rows)
    # zqq = pd.DataFrame(rows,data=list)
    # zqq.to_csv('zqq.csv',encoding='utf-8-sig')
    # rows.append(['Genus','cp_value','feeding','Basal_Wtg','Enrich_Wtg','Structure_Wtg','Ngenus','GenavgMass','GenavgCPr','GenavgCRs','GenavgMFP','GenavgEFP','GenavgSFP','GenavgHFP','GenavgFFP','GenavgBFP','GenavgPFP',
    #     'StderrMass','StderrCPr','StderrCRs','StderrMFP','StderrEFP','StderrSFP','StderrHFP','StderrFFP','StderrBFP','StderrPFP'])
    # for i in result_html1:
    #     data = i.find_all('td')[2]
    #     Nematodes_Genera = data[0].getText()
    #     Family = data[1].getText()
    #     Putative_Feeding = data[2].getText()
    #     c_p_Group = data[3].getText()
    #     Feeding_Group = data[4].getText()
    #     Functional_Guild = data[5].getText()
    #     Internal_Code = data[6].getText()
    #     rows.append([Nematodes_Genera,Family,Putative_Feeding,c_p_Group,Feeding_Group,Functional_Guild,Internal_Code])
    # 寻找按钮节点
    # names = browser.find_elements_by_xpath('//input[@type="button"]')
