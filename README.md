# **Term Deposit Prediction**

## **Bank Marketing Background** 
The data is related with direct marketing campaigns of a Portuguese banking institution. The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would be ('yes') or not ('no') subscribed. 
The dataset “bank-additional.zip” can be downloaded from [here](https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#). By unzipping this file, you shall find the following two files:
1) “bank-additional-full.csv” with all examples (41188) and 20 inputs, ordered by date (from May 2008 to November 2010). 
2) “bank-additional.csv” with 10% of the examples (4119), randomly selected from 1), and 20 inputs. This smaller dataset can be used to test more computationally demanding machine learning algorithms (e.g., SVM). 
There are 20 attributes for each client, including their information on age, job, marital status, education, etc.. The goal is to predict if the client will subscribe (yes/no) a term deposit (variable y), which is target variable shown at the last column of the datasets. Information on this data set can be found [here](https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#).

## **Requirements:** 
1)	Download the dataset from the above website. Present a general description of the dataset and present the general properties of the dataset. 
2)	Describe, with summaries, visualisations, and by other means, the relationship among these attributes and the relationship between each attribute to the target variable y. Discuss what factors might be causing this relationship. 
3)	You are required to implement two classification methods to predict if a client will subscribe (yes/no) a term deposit (variable y). You shall correctly partition the data into training and test set. Also, you need to tune the hyperparameters of your classification model in a principled way. 
4)	Discuss any data preprocessing and selection of attributes which have been applied.
5)	You need to provide the performance measures of your classification results. 
6)	Compare the two classification models you have implemented and discuss their advantages and disadvantages. 
