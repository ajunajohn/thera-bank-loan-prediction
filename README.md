🏦 Thera Bank Loan Prediction
A predictive modeling project to help Thera Bank identify liability customers likely to convert into loan customers, using supervised machine learning techniques.

👩‍💻 Author
Ajuna P John
Date: June 2021

📌 Objective
Thera Bank aims to expand its loan customer base by identifying existing liability customers with high potential for conversion. This project builds classification models to:

Predict the likelihood of a customer accepting a personal loan.

Improve campaign efficiency and reduce marketing costs.

Increase the success rate of loan conversions.

📂 Dataset Overview
Rows: 5,000 customers

Target: Personal Loan (0 = No, 1 = Yes)

Features:

Demographics: Age, Experience, Income, Education, Family

Financial: Mortgage, CCAvg, Securities/CD Accounts

Digital: Internet Banking, Credit Card Usage

⚙️ Methodology
🧹 Data Preprocessing
Replaced missing values in Family with the median

Fixed negative values in Experience

Dropped irrelevant features (ID, ZIP Code)

Converted appropriate columns to factor/ordinal types

🔍 EDA Insights
Higher income and advanced education level increase loan acceptance

Customers with more family members show higher loan uptake

Strong positive correlation between income and credit card usage

🤖 Modeling
Models Built
Model	Accuracy	AUC
CART	93.4%	0.935
Random Forest	97.6%	0.997

Best Model: Random Forest
Accuracy: 97.57% on test data

AUC: 0.9974

Key Predictors:

Income

Education

Family size

Average monthly credit card spend

✅ Recommendations
Target customers with high income and advanced education for loan promotions.

Focus campaigns on families with 3-4 members, where acceptance is higher.

Use credit card and online banking data to tailor offers.

🧰 Tech Stack
Language: R

Libraries: caret, randomForest, rpart, ROCR, ggplot2, dplyr
