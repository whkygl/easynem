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
    rows.append(['cp_value','Basal_Wtg','Enrich_Wtg','Structure_Wtg','NumObs','GldavgMass','GldavgCPr','GldavgCRs','GldavgMFP','GldavgEFP','GldavgSFP','GldavgHFP','GldavgFFP','GldavgBFP','GldavgPFP',
        'StderrMass','StderrCPr','StderrCRs','StderrMFP','StderrEFP','StderrSFP','StderrHFP','StderrFFP','StderrBFP','StderrPFP'])
    url = 'http://nemaplex.ucdavis.edu/Ecology/EcophysiologyParms/StructGuildParmsQuery.aspx'
    browser = selenium.webdriver.Chrome()
    browser.maximize_window()
    browser.get(url)
    # browser.implicity_wait(20)
    select_list = browser.find_elements(By.XPATH,'//*[@id="DropDownList1"]/option')
    button = browser.find_element(By.XPATH,'//*[@id="Button1"]')
    button.click()
    col1 = browser.find_elements(By.XPATH,'//*[@id="DetailsView2"]/tbody/tr/td[2]')
    cp_value = col1[0].text
    Basal_Wtg = col1[1].text
    Enrich_Wtg = col1[2].text
    Structure_Wtg = col1[3].text
    NumObs = col1[4].text
    col2 = browser.find_elements(By.XPATH,'//*[@id="DetailsView1"]/tbody/tr/td[2]')
    GldavgMass = col2[0].text
    GldavgCPr = col2[1].text
    GldavgCRs = col2[2].text
    GldavgMFP = col2[3].text
    GldavgEFP = col2[4].text
    GldavgSFP = col2[5].text
    GldavgHFP = col2[6].text
    GldavgFFP = col2[7].text
    GldavgBFP = col2[8].text
    GldavgPFP = col2[9].text
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
    rows.append([cp_value,Basal_Wtg,Enrich_Wtg,Structure_Wtg,NumObs,GldavgMass,GldavgCPr,GldavgCRs,GldavgMFP,GldavgEFP,GldavgSFP,GldavgHFP,GldavgFFP,GldavgBFP,GldavgPFP,
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
        select = Select(browser.find_element(By.NAME,'DropDownList1'))
        select.select_by_index(i)
        wait = ui.WebDriverWait(browser, wait_time)
        try:
            wait.until(lambda driver: driver.find_element(By.XPATH,'//*[@id="Button1"]'))
            button = browser.find_element(By.XPATH,'//*[@id="Button1"]')
        except Exception as error1:
            print(error1)
            button = browser.find_element(By.XPATH,'//*[@id="Button1"]')
            time.sleep(10)
        button.click()
        # browser.execute_script("arguments[0].click();", button)
        # browser.back()
        col1 = browser.find_elements(By.XPATH,'//*[@id="DetailsView2"]/tbody/tr/td[2]')
        cp_value = col1[0].text
        Basal_Wtg = col1[1].text
        Enrich_Wtg = col1[2].text
        Structure_Wtg = col1[3].text
        NumObs = col1[4].text
        col2 = browser.find_elements(By.XPATH,'//*[@id="DetailsView1"]/tbody/tr/td[2]')
        GldavgMass = col2[0].text
        GldavgCPr = col2[1].text
        GldavgCRs = col2[2].text
        GldavgMFP = col2[3].text
        GldavgEFP = col2[4].text
        GldavgSFP = col2[5].text
        GldavgHFP = col2[6].text
        GldavgFFP = col2[7].text
        GldavgBFP = col2[8].text
        GldavgPFP = col2[9].text
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
        rows.append([cp_value,Basal_Wtg,Enrich_Wtg,Structure_Wtg,NumObs,GldavgMass,GldavgCPr,GldavgCRs,GldavgMFP,GldavgEFP,GldavgSFP,GldavgHFP,GldavgFFP,GldavgBFP,GldavgPFP,
            StderrMass,StderrCPr,StderrCRs,StderrMFP,StderrEFP,StderrSFP,StderrHFP,StderrFFP,StderrBFP,StderrPFP])
        if cp_value == '5':
            break
        browser.back()
        # browser = selenium.webdriver.Chrome()
        # browser.get(url)
        # # browser.implicity_wait(20)
        # select_list = browser.find_elements(By.XPATH,'//*[@id="DropDownList3"]/option')
        # # 找到下拉菜单元素
        # select = Select(browser.find_element(By.NAME,'DropDownList3'))
    zqq = pd.DataFrame(data=rows)
    zqq.to_csv('easynem_database_structure.csv',encoding='utf-8-sig',header=False,index=False)
    browser.close()