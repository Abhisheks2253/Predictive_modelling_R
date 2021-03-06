# Predictive_modelling_R
## **Final course project for IE 500: Data Analytics and Predictive Modelling**

## **Predicting the Number of OFF Periods Per Week for Parkinson’s Disease Patients Leveraging Statistical Learning** 

Parkinson's disease (PD) is a long-term degenerative disorder of the central nervous system that mainly affects the motor system. More than a million people in United states (10 million worldwide) are living with the symptoms of PD which get worse over time. With the actual cause still unknown, PD cases arise spontaneously or are hereditary. So far, it has been established that the brain cells responsible for ‘dopamine’ (a hormone that regulates our body movements) generation die off in PD. However, there are many aspects to PD which still remain untouched.

### **OFF-Periods**

Motor fluctuations are OFF times, when medication namely levodopa (the "gold standard" treatment for Parkinson's) is not working optimally and Parkinson's symptoms (motor and/or non-motor) return

- Currently, the diagnosis made by doctors to assess the severity level of PD is conducted using various methods based on several research domains, including cognitive deficits, speech disorders, human stability, gait cycle and others
- There is no definitive cure for PD but a variety of medications provide dramatic relief from the symptoms
- The objective of this study is to use supervised learning methods to predict the number of OFF periods for Parkinson’s Disease patients based on symptoms and level of impact of PD on their lifestyle. This will help determine the level of severity of PD in a particular patients

### **Data Source**

The data set used in this study is obtained from Michael J. Fox foundation website. Intel has teamed with the Michael J Fox Foundation to gather data from Parkinson’s patients, with the aim of using that data to better understand the disease

### **Conclusion**

- **In this study, the aim was predicting the number of off periods in a week for patients. We considered the symptoms and it’s severity each patient experiences and the magnitude of impact it has on their daliy activities**. 
- We used parametric, semi-parametric and Non-parametric predictive models to find the best fit for our data set. The predictive model with least MSE value, our defining parameter, is the best fit. 
- The difference in Test MSE and Train MSE for **Bagging** obtained is acceptable and shows us that model was not overfit. In the end, we determined the significant variables for predicting OFF periods. 
- Many studies suggest that age is an important parameter for Parkinson’s Disease. However, in this study, we found that age is not that significant. One of the reasons for this we believe is that PD commonly arises in old age. Since the disease is progressive, the symptoms go on becoming severe with age. Most of the patients that showed severe symptoms were above the age of 60 years. 
- The number of years before the patient starts experiencing OFF periods is the most significant factor. Apart from this, the patients should note how difficult it is for them to Drive, do regular scheduled activities, be independent and communicate. 
- The patients should consult their medical advisor if they frequently feel depressed/sad, have difficulty in thinking, have difficulty in hand coordination and difficulty in Bladder control.
