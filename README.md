# Madison-bike-volume-prediction
This repository stores R codes that used two different data set and several statistics models (OLS, ridge, lasso, etc.) to predict daily bike volume in Madison City.
The first data set is weather observation data in two different bike volume observation stations, JN path and SW path. The second data set used historical weather forecast data from Dark Sky API. (Powered by Dark Sky)
Since there are two bike volume observation stations, we build models for the two stations separately.
Generally, we follow the steps below: data cleaning -> add terms -> summary statistics -> exploration (basically correlation) and visualization -> build models (simple OLS -> threshold model -> add interaction term and squared term -> ridge regression -> lasso) -> change data set (use historical weather forecast data) -> repeat preceding steps.
