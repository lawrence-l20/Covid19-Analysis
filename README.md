# <ins> COVID 19-Analysis </ins>

The objectives of these three projects is to garner new insights using various techniques of the relationship between COVID fatalities and infections and other attributes pertaining to a particular county or geography in the United States. 

The datasets used in these projects are derived from the dataset provided by USAFacts, which has uploaded the dataset onto Google Cloud on the link below. However, these files and all files derived from them are provided in each of the project folders within the R_Code folder in this repository. 


COVID-19_cases_plus_census.csv file:

https://console.cloud.google.com/marketplace/details/usafacts-public-data/covid19-us-cases?filter=solution-type:dataset&filter=category:covid19&id=3eaff9c5-fbaf-47bb-a441-89db1e1395ab

The COVID-19_cases_TX.csv file:
https://console.cloud.google.com/marketplace/details/usafacts-public-data/covid19-us-cases?filter=solution-type:dataset&filter=category:covid19&id=3eaff9c5-fbaf-47bb-a441-89db1e1395ab

NOTE: Use SQL on Google Cloud's console to extract just Texas data for the state file with the below:

SELECT *
FROM `bigquery-public-data.covid19_usafacts.summary` covid19 WHERE state = "TX"



## Project 1
In this part of the project, I am analyzing census data in California counties to understand the impacts of COVID-19. I explore how factors like working parents, median age, and income levels correlate with COVID-19 cases and deaths. My approach includes normalizing data for per capita analysis and examining various demographics such as ethnic groups and household types. The findings reveal some expected correlations, such as higher infection rates among working parents, but also uncover less obvious trends in demographic and economic groups.


## Project 2:
I focus on clustering analysis to develop a hypothetical recommendation on how to distribute the COVID-19 vaccine. I used K-Means, Hierarchical, and Fuzzy Clustering methods to analyze COVID-19 data, targeting demographic groups for vaccine prioritization. The analysis incorporated factors like deaths, socioeconomic indicators, and demographics. I evaluated clustering methods using silhouette scores and Dunn indices and compared them through validation techniques. My conclusions recommend vaccine distribution strategies based on population size and income inequality derived from the clustering results.

## Project 3 Summary:
In this part of the project, I'm analyzing COVID-19 data to classify U.S. counties based on predicted mortality rates. My approach involves feature engineering to handle missing data and outliers, and using methods like LASSO for feature selection. I've developed and compared three classification models: Random Forest, Support Vector Machine, and Multinomial Regression, to predict counties' COVID-19 impact levels. This analysis aims to inform health policy by identifying counties most likely to be affected by COVID-19, guiding resource allocation. My work also considers the implications of new COVID-19 variants on the model's effectiveness.




#R_Code Folder and 
















