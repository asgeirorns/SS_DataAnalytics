#Author: Asgeir Orn Sigurpalsson
#Case mix classification - based on the article:
    # Case mix classification and benchmark set for surgery scheduling
    #by Greanne Leeftink and Erwin W. Hans (2017)
#----------------------------------------------------------------------------#
import pandas as pd
import numpy as np
import math
import matplotlib.pyplot as plt
from scipy import stats
fname='___.csv'
df = pd.read_csv(fname, delimiter=";", encoding="utf_8", header=0,
        skiprows=range(0,1),skip_blank_lines=True,parse_dates=True)

#print(df)

#dataframe
dfOperation = pd.DataFrame(df, columns=['Code of surg. case','Time of operation'])

#Let make a table with the mean and std for each surgery
group = dfOperation.groupby('Code of surg. case')
group=group['Time of operation'].agg([np.mean,  np.std]).reset_index()
group = group.fillna(0)
#print(group)
#-----------------------------------------------------------------#
#According to May et al.,2000 and Stephaniak et al. 2009 then
#3-parameter lognormal distribution have the best fit with surgery
#Duration distributions.
group['mt']=  np.exp((group['mean']/60)+((((group['std']/60)**2)/2)))
#We are missing the location parameter for the mean...
calc_std= ((np.exp((group['std']/60)**2))-1)*np.exp((2*group['mean']/60)+(group['std']/60))
group['st']=calc_std**(1/2)

#lets define the load parameter
c=8*1.2*4

plt.scatter(group['mt']/c, group['st']/group['mt'])
print(group)
plt.xlim((0,1))
plt.ylim((0,2))


plt.show()
