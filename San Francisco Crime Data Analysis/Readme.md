The updated data set is available [here](https://data.sfgov.org/Public-Safety/SFPD-Incidents-Previous-Three-Months/tmnf-yvry?). 

**crime.r** analyzes and visualizes the trends from 3 months of criminal activities collected by the San Francisco Police Department between June 1st 2014 and August 31st 2014. 

Exploratory data visualization of crime trends:

1) Histogram of frequency for each SFPD crime category: ![crimecatHist.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatHist.png)

2) Frequency of each crime category distributed by date:

- Distribution of all 36 crime categories by date ![crimecatDate.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatDate.png)

- Distribution of low frequency crime categories (< 50 incidents) by date ![crimecatLowDate.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatLowDate.png)

- Distribution of medium frequency crime categories (between 50 and 800 incidents) by date ![crimecatMedDate.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatMedDate.png)

- Distribution of high frequency crime categories (> 800 incidents) by date ![crimecatHighDate.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatHighDate.png)

3) Cumulative count of every crime category:
- Cumulative counts of all 36 crime categories between June 1st 2014 and August 31st 2014 ![crimecatCumu.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatCumu.png)
- Cumulative counts of low frequency crime categories (< 50 incidents) between June 1st 2014 and August 31st 2014 ![crimecatLowCumu.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatLowCumu.png) 
- Cumulative counts of medium frequency crime categories (between 50 and 800 incidents) between June 1st 2014 and August 31st 2014 ![crimecatMedCumu.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatMedCumu.png)
- Cumulative counts of high frequency crime categories (> 800 incidents) between June 1st 2014 and August 31st 2014 ![crimecatHighCumu.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatHighCumu.png)

4) Distribution of all crime incidents by date: ![totcrimeDate.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/totcrimeDate.png)

**crime.r** primarily answers 4 following questions:

1) Where in San Francisco should you not park your car?

2) Where are the safest locations in San Francisco?

3) Which days and what times are especially dangerous in San Francisco? **Output**: 

- Distribution of all 36 crime categories by day-of-week (Monday - Sunday) ![crimecatDay.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatDay.png)

- Distribution of low frequency crime categories (< 50 incidents) by day-of-week (Monday - Sunday) ![crimecatLowDay.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatLowDay.png)

- Distribution of medium frequency crime categories (between 50 and 800 incidents) by day-of-week (Monday - Sunday) ![crimecatMedDay.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatMedDay.png)

- Distribution of high frequency crime categories (> 800 incidents) by day-of-week (Monday - Sunday) ![crimecatHighDay.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatHighDay.png)

- Distribution of all crime incidents by day-of-week (Monday - Sunday) ![totcrimeDay.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/totcrimeDay.png)

- Distribution of all 36 crime categories by 24-hour time interval (00:00 - 23:59) ![crimecatTime.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatTime.png)

- Distribution of low frequency crime categories (< 50 incidents) by 24-hour time interval (00:00 - 23:59) ![crimecatLowTime.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatLowTime.png)
 
- Distribution of medium frequency crime categories (between 50 and 800 incidents) by 24-hour time interval (00:00 - 23:59) ![crimecatMedTime.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatMedTime.png)

- Distribution of high frequency crime categories (> 800 incidents) by 24-hour time interval (00:00 - 23:59) ![crimecatHighTime.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/crimecatHighTime.png)
 
- Distribution of all crime incidents by 24-hour time interval (00:00 - 23:59) ![totcrimeTime.png](https://github.com/shngli/R-data-analysis/blob/master/San%20Francisco%20Crime%20Data%20Analysis/totcrimeTime.png)

4) Are certain types of theft more common in certain areas?
