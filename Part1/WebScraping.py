
# coding: utf-8

# In[1]:

import requests
from lxml import html
import os
import sys
import logging # for logging
import shutil #to delete the directory contents
import zipfile
from zipfile import ZipFile
from io import BytesIO
import pandas as pd
import zipfile
import numpy as np
import json
import logging
import logging.handlers
import time
import glob
import csv


# In[2]:

import logging
import logging.handlers

logger=logging.getLogger(__name__)
logger.setLevel(logging.INFO)

logfile1 = time.strftime("%Y-%m-%d_%H_%M_%S"+".log")
print (logfile1)
handler= logging.FileHandler(logfile1)
handler.setLevel(logging.INFO)

formatter= logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
handler.setFormatter(formatter)
logger.addHandler(handler)


# In[3]:

logger.info('Reading Json')

with open('config.json') as data:
    payload=json.load(data)


# In[16]:

Summary='Summaries'
path_summary = '/Summaries/'
path_to_summary= os.getcwd()+path_summary
if not os.path.exists(path_to_summary):
    os.makedirs(path_to_summary)


# In[ ]:




# In[10]:

logger.info('Logging in and Downloading Zip ')
filename= 'downloaded_zips'
path = '/downloaded_zips'
path1= os.getcwd()+path
if not os.path.exists(path1):
    os.makedirs(path1)
with requests.session() as s:
    login_url = "https://freddiemac.embs.com/FLoan/secure/auth.php"
    result = s.post(
        login_url, 
        data = payload, 
        headers = dict(referer=login_url)
    )
    result.content
    url = 'https://freddiemac.embs.com/FLoan/Data/download.php'
    agreement_payload={
        "accept":"Yes",
        "action":"acceptTandC",
        "acceptSubmit":"Continue"
        }
    result1 = s.post(
        url, 
        agreement_payload,
        headers = dict(referer = url)
    )
    tree = html.fromstring(result1.content)
    all_links= tree.findall('.//a')
    for link in all_links[76:88]:
        href=link.get("href")
        if "sample" in href:
            url= 'https://freddiemac.embs.com/FLoan/Data/'+href
            r = s.get(url,stream=True)
#             print(r)
            with open(os.path.join(path1,link.text), 'wb') as f:
#                 print(link.text)
                for chunk in r.iter_content(chunk_size=1024):
                    if chunk: # filter out keep-alive new chunks
                        f.write(chunk)

                    
logger.info('Successfully downloaded zip files ')


# In[5]:

logger.info('Checking if a folder exists and creating a folder for saving unzipped files')
unzip_folder='unzipped_files'
path_unzip = '/unzipped_files'
path_to_unzip= os.getcwd()+path_unzip
if not os.path.exists(path_to_unzip):
    os.makedirs(path_to_unzip)


# In[7]:

logger.info('Unzipping files')
filename= 'downloaded_zips'
import zipfile
for files in glob.glob(os.path.join(filename, '*.zip')):       
        with zipfile.ZipFile(files) as zip_ref:
            zip_ref.extractall(path_to_unzip)


# In[8]:

logger.info('Reading and Concatenating all the sample Origination files in a dataframe ')
final_df=[]
for file in glob.glob(os.path.join(path_to_unzip, '*.txt'))[:12]:                                     
        a = pd.read_table(file, delimiter='|',header=None,low_memory=False)
#         print(a.shape)
        final_df.append(a)
origination_df=pd.concat(final_df)


# In[9]:

logger.info('Cleaning the columns')
origination_df[0]= pd.DataFrame(origination_df[0].replace(r'\s+', np.nan, regex=True))
origination_df[0]=pd.DataFrame(origination_df[0].fillna(900))

origination_df[2]= pd.DataFrame(origination_df[2].replace(r'\s+', np.nan, regex=True))
origination_df[2]=pd.DataFrame(origination_df[2].fillna('U'))

origination_df[4] =pd.DataFrame(origination_df[4].replace(r'\s+', np.nan, regex=True))
origination_df[4]=pd.DataFrame(origination_df[4].fillna('00000.0'))

origination_df[5]= pd.DataFrame(origination_df[5].replace(r'\s+', np.nan, regex=True))
origination_df[5]=pd.DataFrame(origination_df[5].fillna('56'))

origination_df[8] =pd.DataFrame(origination_df[8].replace(r'\s+', np.nan, regex=True))
origination_df[8]=pd.DataFrame(origination_df[8].fillna('201'))

origination_df[9]=pd.DataFrame(origination_df[9].fillna('00'))
origination_df[9]= pd.DataFrame(origination_df[9].replace(r'\s+','70', regex=True))

origination_df[11]= pd.DataFrame(origination_df[11].replace(r'\s+', np.nan, regex=True))
origination_df[11]=pd.DataFrame(origination_df[11].fillna('106'))

origination_df[14]= pd.DataFrame(origination_df[14].replace(r'\s+', np.nan, regex=True))
origination_df[14]=pd.DataFrame(origination_df[14].fillna('U'))

origination_df[18]= pd.DataFrame(origination_df[18].replace(r'\s+', np.nan, regex=True))
origination_df[18]=pd.DataFrame(origination_df[18].fillna('0000'))

origination_df[22]= pd.DataFrame(origination_df[22].replace(r'\s+', np.nan, regex=True))
origination_df[22]=pd.DataFrame(origination_df[22].fillna('03'))

origination_df[23]= pd.DataFrame(origination_df[23].replace(r'\s+', np.nan, regex=True))
origination_df[23]=pd.DataFrame(origination_df[23].fillna('U'))

origination_df[24]= pd.DataFrame(origination_df[24].replace(r'\s+', np.nan, regex=True))
origination_df[24]=pd.DataFrame(origination_df[24].fillna('U'))

origination_df[25]= pd.DataFrame(origination_df[25].replace(r'\s+', np.nan, regex=True))
origination_df[25]=pd.DataFrame(origination_df[25].fillna('U'))


logger.info('Columns Cleaned')


# In[10]:

logger.info('Assigning Column Names')
origination_df.columns=['CREDIT SCORE','FIRST PAYMENT DATE','FIRST TIME HOMEBUYER FLAG','MATURITY DATE',
                                                'METROPOLITAN DIVISION','MORTGAGE INSURANCE PERCENTAGE (MI %)',
                                                'NUMBER OF UNITS','OCCUPANCY STATUS', '(CLTV)','DTI) RATIO','ORIGINAL UPB',
                                                '(LTV)','ORIGINAL INTEREST RATE','CHANNEL','(PPM)','PRODUCT TYPE','PROPERTY STATE',
                                                'PROPERTY TYPE','POSTAL CODE','LOAN SEQUENCE NUMBER','LOAN PURPOSE','ORIGINAL LOAN TERM',
                                                'NUMBER OF BORROWERS','SELLER NAME','SERVICER NAME','Super Conforming Flag']


# In[11]:

logger.info('Creating new column for Year, Month and Quarter')

origination_df['year']=pd.DataFrame(origination_df['FIRST PAYMENT DATE'].astype(str))
origination_df['month']=pd.DataFrame(origination_df['FIRST PAYMENT DATE'].astype(str))

origination_df['year']=pd.DataFrame(origination_df['year'].str[:4])
origination_df['month']= pd.DataFrame(origination_df['month'].str[4:6])


origination_df.loc[origination_df['month'] <='03', "Quarter"] = "Q1"
origination_df.loc[((origination_df['month'] >='04') & (origination_df['month'] <='06')) , "Quarter"] = "Q2"
origination_df.loc[((origination_df['month'] >='06') & (origination_df['month'] <='09')) , "Quarter"] = "Q3"
origination_df.loc[((origination_df['month'] >='09') & (origination_df['month'] <='12')) , "Quarter"] = "Q4"


# In[12]:

logger.info('Changing the data type of few columns')
origination_df['CREDIT SCORE']=pd.DataFrame(origination_df['CREDIT SCORE'].astype(int))
origination_df['(CLTV)']=pd.DataFrame(origination_df['(CLTV)'].astype(int))
origination_df['(LTV)']=pd.DataFrame(origination_df['(LTV)'].astype(int))
origination_df['DTI) RATIO']=pd.DataFrame(origination_df['DTI) RATIO'].astype(int))
origination_df['MORTGAGE INSURANCE PERCENTAGE (MI %)']=pd.DataFrame(origination_df['MORTGAGE INSURANCE PERCENTAGE (MI %)'].astype(int))


# In[18]:

logger.info('Writing the file to a csv file')
origination_df.to_csv(path_to_summary+'Origination.csv')


# # Origination Summary statistics

# ### 1.  Origination loan details over the year and state

# In[17]:

logger.info('Creating Summary file for loan details over the year and state')

orig_summary_1= (origination_df.groupby(['PROPERTY STATE','year'])['NUMBER OF UNITS'].count()).reset_index()

orig_summary_2= (origination_df.groupby(['PROPERTY STATE','year'])['ORIGINAL UPB','CREDIT SCORE','(CLTV)','(LTV)','DTI) RATIO','MORTGAGE INSURANCE PERCENTAGE (MI %)',
                                           'ORIGINAL INTEREST RATE'].mean()).reset_index()

orig_final_summary_1=orig_summary_1.merge(orig_summary_2,on=['PROPERTY STATE','year'],how='left')

orig_final_summary_1.to_csv(path_to_summary+'Orig_Loan_Details_Over_Year_State.csv')


# In[ ]:




# In[ ]:




# ### 2. Origination loan type over the year and state

# In[29]:

logger.info('Creating Summary file for loan type over year annd state')
orig_summary3= pd.DataFrame(origination_df.groupby(['year','NUMBER OF UNITS','OCCUPANCY STATUS','CHANNEL','PROPERTY TYPE','PROPERTY STATE',
                                              'FIRST TIME HOMEBUYER FLAG','POSTAL CODE','LOAN PURPOSE','NUMBER OF BORROWERS'])['NUMBER OF UNITS'].count())



orig_summary3.to_csv(path_to_summary+'Loan_Type_Year_State.csv')


# In[ ]:




# ### 3. Origination Summary over loan number

# In[30]:

logger.info('Summary file over loan number')
orig_summary_4= (origination_df.groupby(['LOAN SEQUENCE NUMBER','year'])['ORIGINAL UPB','CREDIT SCORE','(CLTV)','(LTV)','DTI) RATIO','MORTGAGE INSURANCE PERCENTAGE (MI %)',
                                           'ORIGINAL INTEREST RATE'].mean()).reset_index()

orig_summary_4.to_csv(path_to_summary+'Orig_Loan_Number_Summary.csv')


# In[ ]:




# ## Pre-Precoessing of  Performance files

# In[5]:

import glob,os
b=[]
for file in glob.glob(os.path.join(path_to_unzip, '*.txt'))[12:24]:                                     
#     print(file)
    b.append(file)


# In[6]:

logger.info('Creating a Dataframe for performance file, CLeaning it and taking the neccessary columns')
summary1=[]
for x in b:
    
    a = pd.read_table(x, delimiter='|',header=None)
    a[6]=pd.DataFrame(a[6].fillna('U'))
    a[7]=pd.DataFrame(a[7].fillna('N'))
    a[8]=pd.DataFrame(a[8].fillna('00'))
    a[9]=pd.DataFrame(a[9].fillna('000000'))
    a[12]=pd.DataFrame(a[12].fillna('000000'))
    a[13]=pd.DataFrame(a[13].fillna('000000'))
    a[14]=pd.DataFrame(a[14].fillna('000000'))
    a[15]=pd.DataFrame(a[15].fillna('000000'))
    a[16]=pd.DataFrame(a[16].fillna('000000'))
    a[17]=pd.DataFrame(a[17].fillna('000000'))
    a[18]=pd.DataFrame(a[18].fillna('000000'))
    a[19]=pd.DataFrame(a[19].fillna('000000'))
    a[20]=pd.DataFrame(a[20].fillna('000000'))
    a[21]=pd.DataFrame(a[21].fillna('000000'))
    a[22]=pd.DataFrame(a[22].fillna('000000'))
    
    a.columns=['LOAN SEQUENCE NUMBER','MONTHLY REPORTING PERIOD','CURRENT ACTUAL UPB','CURRENT LOAN DELINQUENCY STATUS',
                          'LOAN AGE','REMAINING MONTHS TO LEGAL MATURITY','REPURCHASE FLAG','MODIFICATION FLAG','ZERO BALANCE CODE',
                          'ZERO BALANCE EFFECTIVE DATE','CURRENT INTEREST RATE','CURRENT DEFERRED UPB','DUE DATE OF LAST PAID INSTALLMENT (DDLPI)',
                          'MI RECOVERIES','NET SALES PROCEEDS','NON MI RECOVERIES','EXPENSES','Legal Costs','Maintenance and Preservation Costs','Taxes and Insurance',
                          'Miscellaneous Expenses','Actual Loss Calculation','Modification Cost']
#     a.to_csv('123.csv',chunksize=1024)
    a['year']=pd.DataFrame(a['MONTHLY REPORTING PERIOD'].astype(str))
    a['year']=pd.DataFrame(a['year'].str[:4])
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].replace(r'R', 0, regex=True))
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].replace(r'XX', 0, regex=True))
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].replace(r'\s+', 0, regex=True))
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].astype(int))
    a['MI RECOVERIES']=pd.DataFrame(a['MI RECOVERIES'].astype(int))
    a['EXPENSES']=pd.DataFrame(a['EXPENSES'].astype(int))
    a['Maintenance and Preservation Costs']=pd.DataFrame(a['Maintenance and Preservation Costs'].astype(int))
    a['Actual Loss Calculation']=pd.DataFrame(a['Actual Loss Calculation'].astype(int))
    df1=pd.DataFrame(a.groupby(['year','LOAN SEQUENCE NUMBER'])['CURRENT LOAN DELINQUENCY STATUS','CURRENT ACTUAL UPB','LOAN AGE','REMAINING MONTHS TO LEGAL MATURITY',
                                    'CURRENT INTEREST RATE','CURRENT DEFERRED UPB','MI RECOVERIES','EXPENSES','Maintenance and Preservation Costs',
                                     'Actual Loss Calculation'].mean())
    
    summary1.append(df1)
    
final_summary_1= pd.concat(summary1)

    


# In[31]:

del summary1
final_summary_1.to_csv(path_to_summary+'Summary1_Performance.csv')
del final_summary_1


# In[32]:

logger.info('Creating second performance Summary')
import csv
summary2=[]
for files in b[2:5]:
    
    a = pd.read_table(files, delimiter='|',header=None)
    a[6]=pd.DataFrame(a[6].fillna('U'))
    a[7]=pd.DataFrame(a[7].fillna('N'))
    a[8]=pd.DataFrame(a[8].fillna('00'))
    a[9]=pd.DataFrame(a[9].fillna('000000'))
    a[12]=pd.DataFrame(a[12].fillna('000000'))
    a[13]=pd.DataFrame(a[13].fillna('000000'))
    a[14]=pd.DataFrame(a[14].fillna('000000'))
    a[15]=pd.DataFrame(a[15].fillna('000000'))
    a[16]=pd.DataFrame(a[16].fillna('000000'))
    a[17]=pd.DataFrame(a[17].fillna('000000'))
    a[18]=pd.DataFrame(a[18].fillna('000000'))
    a[19]=pd.DataFrame(a[19].fillna('000000'))
    a[20]=pd.DataFrame(a[20].fillna('000000'))
    a[21]=pd.DataFrame(a[21].fillna('000000'))
    a[22]=pd.DataFrame(a[22].fillna('000000'))
    
    a.columns=['LOAN SEQUENCE NUMBER','MONTHLY REPORTING PERIOD','CURRENT ACTUAL UPB','CURRENT LOAN DELINQUENCY STATUS',
                          'LOAN AGE','REMAINING MONTHS TO LEGAL MATURITY','REPURCHASE FLAG','MODIFICATION FLAG','ZERO BALANCE CODE',
                          'ZERO BALANCE EFFECTIVE DATE','CURRENT INTEREST RATE','CURRENT DEFERRED UPB','DUE DATE OF LAST PAID INSTALLMENT (DDLPI)',
                          'MI RECOVERIES','NET SALES PROCEEDS','NON MI RECOVERIES','EXPENSES','Legal Costs','Maintenance and Preservation Costs','Taxes and Insurance',
                          'Miscellaneous Expenses','Actual Loss Calculation','Modification Cost']
#     a.to_csv('123.csv',chunksize=1024)
    a['year']=pd.DataFrame(a['MONTHLY REPORTING PERIOD'].astype(str))
    
    a['year']=pd.DataFrame(a['year'].str[:4])
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].replace(r'R', 0, regex=True))
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].replace(r'XX', 0, regex=True))
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].replace(r'\s+', 0, regex=True))
    a['CURRENT LOAN DELINQUENCY STATUS']=pd.DataFrame(a['CURRENT LOAN DELINQUENCY STATUS'].astype(int))
    a['MI RECOVERIES']=pd.DataFrame(a['MI RECOVERIES'].astype(int))
    a['EXPENSES']=pd.DataFrame(a['EXPENSES'].astype(int))
    a['Maintenance and Preservation Costs']=pd.DataFrame(a['Maintenance and Preservation Costs'].astype(int))
    a['Actual Loss Calculation']=pd.DataFrame(a['Actual Loss Calculation'].astype(int))
    df1=pd.DataFrame(a.groupby(['year','ZERO BALANCE CODE','REPURCHASE FLAG','MODIFICATION FLAG','LOAN SEQUENCE NUMBER'])['Actual Loss Calculation',
                                        'Modification Cost','Miscellaneous Expenses','Taxes and Insurance','Maintenance and Preservation Costs',
                                        'Legal Costs','EXPENSES'].sum())
    
    summary2.append(df1)
    
final_summary_2= pd.concat(summary2)
del summary2


# In[33]:

final_summary_2.to_csv(path_to_summary+'Summary2_Performance.csv')
del final_summary_2


# In[ ]:




# In[34]:

final_summary_2=pd.read_csv('Summary2_Performance.csv')

df_origination= pd.read_csv('Origination.csv')


# In[35]:

df_performance_origination2= final_summary_2.merge(df_origination,on='LOAN SEQUENCE NUMBER',how='left')
df_performance_origination2.to_csv(path_to_summary+'Performance_Origination_Summary2.csv')


# In[36]:

df_performance_1= pd.read_csv('Summary1_Performance.csv')

df_performance_orig_1= (df_performance_1.groupby('LOAN SEQUENCE NUMBER')['CURRENT LOAN DELINQUENCY STATUS','CURRENT ACTUAL UPB',
                                                                       'CURRENT INTEREST RATE','Actual Loss Calculation'].mean()).reset_index()

df_performance_orig_2= pd.read_csv('Orig_Loan_Number_Summary.csv')

df_performance_orig= df_performance_orig_2.merge(df_performance_orig_1,on='LOAN SEQUENCE NUMBER',how='left')


df_performance_orig.to_csv(path_to_summary+'Performance_Origination_summary.csv')


# In[111]:




# In[112]:




# In[ ]:




# In[113]:




# In[ ]:




# In[115]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:



