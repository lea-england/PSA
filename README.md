# PSA
Developing linear model to predict PSA level (serum prostate-specific antigen level (mg/ml)) of men with prostate cancer (a high PSA level indicates a greater chance of prostate cancer). Specifically, a model that uses volume, weight, age, hyperplasia, invasion, capsular, and gleason(or an appropriate subset thereof) to describe and predict the value of the response variable PSA.

Below, find plots of residuals vs fits of the final models chosen, referenced in the r code. Ultimately, I believe the model :̂log(PSA) = 1.597 + 0.137V olume−0.002Volume^22+ 0.651Invasion best fits the data. 

![con_resfit](https://user-images.githubusercontent.com/31354616/30307949-c4ccfd46-9736-11e7-87fe-cfb9ef5d3f6c.png)
