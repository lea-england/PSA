# PSA
Developing linear model to predict PSA level (serum prostate-specific antigen level (mg/ml)) of men with prostate cancer (a high PSA level indicates a greater chance of prostate cancer). Specifically, a model that uses volume, weight, age, hyperplasia, invasion, capsular, and gleason(or an appropriate subset thereof) to describe and predict the value of the response variable PSA.

Ultimately, I chose the model:

![con_resfit](http://latex.codecogs.com/svg.latex?%24%24%5Cwidehat%7Blog%28PSA%29%7D%3D1.597%2B0.137Volume-0.002Volume%5E2%2B0.651Invasion.%24%24)

Below, find plots of residuals vs fits of the final models chosen, referenced in the r code. 

![con_resfit](https://user-images.githubusercontent.com/31354616/30307949-c4ccfd46-9736-11e7-87fe-cfb9ef5d3f6c.png)
