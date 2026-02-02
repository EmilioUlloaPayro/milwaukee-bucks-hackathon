# milwaukee-bucks-hackathon
Analytics project for Milwaukee Bucks Hackathon

# Milwaukee Bucks Hackathon – Data Analytics Project

## Overview
This project was developed as part of the 2025 Milwaukee Bucks Hackathon (Engineered by Modine). Our team selected one of the official hackathon prompts and built a data-driven solution aimed at generating actionable basketball insights using team-constructed datasets and statistical modeling in R.

The focus of this project was to identify important performance factors and evaluate their relationship with the target outcome using interpretable and predictive models.

## Objective
To analyze basketball performance data and determine which variables most strongly influence the target outcome (e.g., game success, player impact, or performance metric), using statistical and machine learning models to support data-driven decision making.

## Data
- Source: Milwaukee Bucks Hackathon datasets combined and engineered by team members  
- Format: CSV / Excel  
- Description: Aggregated dataset containing performance metrics, contextual game variables, and engineered features selected by the team  
- Preprocessing:
  - Cleaning missing values  
  - Filtering relevant observations  
  - Feature selection and transformation  

*(Raw data not included due to competition restrictions.)*

## Methods
- Exploratory Data Analysis (EDA)  
- Feature engineering and variable selection  
- Generalized Linear Model (GLM)  
- Random Forest  

Models were used to:
- Identify statistically significant predictors  
- Measure variable importance  
- Compare predictive performance  

## Tools
- R  
- tidyverse  
- randomForest  
- ggplot2  
- Excel (initial data merging)

## Key Insights
- Certain performance metrics showed strong relationships with the target outcome  
- Random Forest highlighted top contributing variables through feature importance  
- GLM provided interpretability and direction of effects  

## Results
The models demonstrated that a small subset of variables explains a meaningful portion of variation in the outcome. Random Forest achieved stronger predictive performance, while GLM offered interpretability for explaining relationships between predictors and the target.

## Files
- `data/` – Cleaned dataset
- `notebooks/` – R analysis notebooks  
- `scripts/` – Modeling and preprocessing scripts  
- `presentation/` – Hackathon slides  

## Author
Emilio Ulloa Payro
