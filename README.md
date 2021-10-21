
### William Laplante

This project consists of a basic exploratory analysis of a chosen dataset.  

In the first part of this data analysis project, we started by selecting 4 out of 7 datasets of interest to us. Then, these datasets were explored to narrow down our choices to 2 datasets. Various functions were used to explore the data, including glimpse(), summary(), etc... Then, graphs were made to visualize the data. From this exploration, a final dataset was selected, namely the cancer_sample dataset, and the following research questions were proposed for the followup of this project : 

1 - How does the area of a cancer affects the diagnosis?
2 - How strongly do smoothness and compactness correlate, and is it dependent on diagnosis?
3 - What is the variable in this dataset that is the best indicator of a malignant cancer?
4 - Is it harder to perform radius measurements on malignant cancers compared to benign cancers? That is, is the standard error in radius measurements bigger for malignant cancers?


The second part of this project consisted of exploring the data in greater depth, and reducing from 4 to 2 the number of research questions we have.

After processing and summarizing the data, the remaining research questions were the following:

1 - How does the area of a cancer affects the diagnosis?
2 - What is the variable in this dataset that is the best indicator of a malignant cancer?

In the last part of this project, we decided to analyze question 2 of the previous part, and we also went more in depth by asking the following question :

If we create two models, one trained with the variables that have “Very High” correlation with the diagnosis, and another with the variables falling under the “other” category, how do these two models compare?

We thus separated the variables in two categories, and used multivariate linear models to fit the data. It turns out that both models predicted poorly, since they both had bad R-squared coefficients. This result showed us that most likely, the main problem was our model, and not so much the variables used in our model. Using a non-linear model would have probably yielded better results.


The directory structure is pretty straightforward ; md1, md2 and md3 contain the contents of each milestones, and the output folder contains the models generated in milestone 3 along with a summary table obtained from milestone 2.
