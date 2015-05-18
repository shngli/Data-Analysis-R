Data is available [here](https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv). There is also a code book for the variables in the data set available [here](https://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf).

The data above consist of a sample of 2,500 peer-to-peer loans issued through the Lending Club. The interest rate of these loans is determined by the Lending Club on the basis of characteristics of the person asking for the loan such as their employment history, credit history, and creditworthiness scores. 

Identify and quantify associations between the interest rate of the loan and the other variables in the data set. Consider whether any of these variables have an important association with interest rate after taking into account the applicant's FICO score. For example, if two people have the same FICO score, can the other variables explain a difference in interest rate between them?

Your data analysis submission will consist of the following components:

1. Your main text should be written in the form of an essay with an introduction, methods, results, and conclusions section. 
2. One figure for your data analysis uploaded as a .png, .jpg, or .pdf file, along with a figure caption of up to 500 words. 

**Output**: 
- Interest rate (%) by loan length (36 months vs 60 months) ![InterestRateLoanLength.png](https://github.com/shngli/R-data-analysis/blob/master/Lending%20club%20interest%20rate%20analysis/InterestRateLoanLength.png) 
- Interest rate (%) vs FICO Score (colored by loan lengths, 36 months and 60 months) ![InterestRateFICOScore.png](https://github.com/shngli/R-data-analysis/blob/master/Lending%20club%20interest%20rate%20analysis/InterestRateFICOScore.png) 
- Interest rate (%) vs Amount funded by investors (colored by FICO score) ![InterestRateAmntFunded.png](https://github.com/shngli/R-data-analysis/blob/master/Lending%20club%20interest%20rate%20analysis/InterestRateAmntFunded.png)
- Residual plot of the final multivariate model ![residualsmodel4.png](https://github.com/shngli/R-data-analysis/blob/master/Lending%20club%20interest%20rate%20analysis/residualsmodel4.png)
