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
    rows.append(['Family','cp_value','feeding','feeding_code','Functional_guild','Basal_Wtg','Enrich_Wtg','Structure_Wtg','nFamily','FamavgMass','FamavgCPr','FamavgCRs','FamavgMFP','FamavgEFP','FamavgSFP','FamavgHFP','FamavgFFP','FamavgBFP','FamavgPFP',
        'StderrMass','StderrCPr','StderrCRs','StderrMFP','StderrEFP','StderrSFP','StderrHFP','StderrFFP','StderrBFP','StderrPFP'])
    url = 'http://nemaplex.ucdavis.edu/Ecology/EcophysiologyParms/FamilyParmsQuery.aspx'
    browser = selenium.webdriver.Chrome()
    browser.maximize_window()
    browser.get(url)
    # browser.implicity_wait(20)
    select_list = browser.find_elements(By.XPATH,' //*[@id="DropDownList2"]/option')
    button = browser.find_element(By.XPATH,'//*[@id="Button1"]')
    button.click()
    col1 = browser.find_elements(By.XPATH,'//*[@id="DetailsView5"]/tbody/tr/td[2]')
    Family = col1[0].text
    cp_value = col1[1].text
    feeding = col1[2].text
    feeding_code = col1[3].text
    Functional_guild = col1[4].text
    Basal_Wtg = col1[5].text
    Enrich_Wtg = col1[6].text
    Structure_Wtg = col1[7].text
    nFamily = col1[8].text
    col2 = browser.find_elements(By.XPATH,'//*[@id="DetailsView6"]/tbody/tr/td[2]')
    FamavgMass = col2[0].text
    FamavgCPr = col2[1].text
    FamavgCRs = col2[2].text
    FamavgMFP = col2[3].text
    FamavgEFP = col2[4].text
    FamavgSFP = col2[5].text
    FamavgHFP = col2[6].text
    FamavgFFP = col2[7].text
    FamavgBFP = col2[8].text
    FamavgPFP = col2[9].text
    col3 = browser.find_elements(By.XPATH,'//*[@id="DetailsView7"]/tbody/tr/td[2]')
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
    rows.append([Family,cp_value,feeding,feeding_code,Functional_guild,Basal_Wtg,Enrich_Wtg,Structure_Wtg,nFamily,FamavgMass,FamavgCPr,FamavgCRs,FamavgMFP,FamavgEFP,FamavgSFP,FamavgHFP,FamavgFFP,FamavgBFP,FamavgPFP,
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
        select = Select(browser.find_element(By.NAME,'DropDownList2'))
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
        col1 = browser.find_elements(By.XPATH,'//*[@id="DetailsView5"]/tbody/tr/td[2]')
        Family = col1[0].text
        cp_value = col1[1].text
        feeding = col1[2].text
        feeding_code = col1[3].text
        Functional_guild = col1[4].text
        Basal_Wtg = col1[5].text
        Enrich_Wtg = col1[6].text
        Structure_Wtg = col1[7].text
        nFamily = col1[8].text
        col2 = browser.find_elements(By.XPATH,'//*[@id="DetailsView6"]/tbody/tr/td[2]')
        FamavgMass = col2[0].text
        FamavgCPr = col2[1].text
        FamavgCRs = col2[2].text
        FamavgMFP = col2[3].text
        FamavgEFP = col2[4].text
        FamavgSFP = col2[5].text
        FamavgHFP = col2[6].text
        FamavgFFP = col2[7].text
        FamavgBFP = col2[8].text
        FamavgPFP = col2[9].text
        col3 = browser.find_elements(By.XPATH,'//*[@id="DetailsView7"]/tbody/tr/td[2]')
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
        rows.append([Family,cp_value,feeding,feeding_code,Functional_guild,Basal_Wtg,Enrich_Wtg,Structure_Wtg,nFamily,FamavgMass,FamavgCPr,FamavgCRs,FamavgMFP,FamavgEFP,FamavgSFP,FamavgHFP,FamavgFFP,FamavgBFP,FamavgPFP,
            StderrMass,StderrCPr,StderrCRs,StderrMFP,StderrEFP,StderrSFP,StderrHFP,StderrFFP,StderrBFP,StderrPFP])
        if Family == 'Xyalidae':
            break
        browser.back()
        # browser = selenium.webdriver.Chrome()
        # browser.get(url)
        # # browser.implicity_wait(20)
        # select_list = browser.find_elements(By.XPATH,'//*[@id="DropDownList3"]/option')
        # # 找到下拉菜单元素
        # select = Select(browser.find_element(By.NAME,'DropDownList3'))
    zqq = pd.DataFrame(data=rows)
    zqq.to_csv('easynem_database_family.csv',encoding='utf-8-sig',header=False,index=False)
    browser.close()
 