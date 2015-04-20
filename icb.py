# Import necessary libraries
import os
os.chdir('C:\\Users\\E-money\\Desktop\\lol')
from pandas import read_csv
crisis_data = read_csv('slcrisisdata.csv')
from sklearn import RFE
