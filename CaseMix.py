#Author: Asgeir Orn Sigurpalsson
#Date: 27/10/2017 23:24
#---------------------------------------------------------------------#
#Case mix classification - based on the article:
    # Case mix classification and benchmark set for surgery scheduling
    #by Greanne Leeftink and Erwin W. Hans (2017)

#Next steps: Clean the data - gives wrong results for some cases
#        +Some data is missing
#--------------------------------------------------------------------#
import pandas as pd
import numpy as np
import math
import matplotlib.pyplot as plt
from scipy import stats
import xlsxwriter
fname='-----.csv'
df = pd.read_csv(fname, delimiter=";", encoding="utf_8", header=0,
        skiprows=range(0,1),skip_blank_lines=True,parse_dates=True)

#dataframe
dfOperation = pd.DataFrame(df, columns=['Code of surg. case','Time of operation'])

#Let make a table with the mean and std for each surgery
group = dfOperation.groupby('Code of surg. case')
group=group['Time of operation'].agg([np.mean,  np.std]).reset_index()
group = group.fillna(0)
#print(group)
#-----------------------------------------------------------------#

#3-parameter lognormal distribution have the best fit with surgery
#Duration distributions.
    #We are missing the location parameter for the mean...
group['mt']=  np.exp((group['mean']/60)+((((group['std']/60)**2)/2)))

#Stdev
group['st']= (((np.exp((group['std']/60)**2))-1)*np.exp((2*(group['mean']/60))+((group['std']/60)**2)))**(1/2)

#lets define the load parameter - must check this better
c=8*1.2*4
#Add extra columns to the matrix to analyse
group['xas']=group['mt']/c
group['yas']=group['st']/group['mt']
plt.scatter(group['mt']/c, group['st']/group['mt'])

plt.xlim((0,1))
plt.ylim((0,1))
plt.show()
# Create a Pandas Excel writer using XlsxWriter as the engine.
writer = pd.ExcelWriter('analytics.xlsx', engine='xlsxwriter')
# Convert the dataframe to an XlsxWriter Excel object.
group.to_excel(writer, sheet_name='Sheet1')
# Close the Pandas Excel writer and output the Excel file.
writer.save()
