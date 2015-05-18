**sotu.R** performs quick text analysis on the State of the Union (SOTU) speeches from 1790 to 2012 with regular expressions and clustering techniques. It examines the frequency of words and sentences used in each speech, and compare the presidents across different eras. The raw text file of the compiled SOTU speeches is availabe [here](http://www.stat.berkeley.edu/users/nolan/stat133/data/stateoftheunion1790-2012.txt.zip).

1) Number of words used in every SOTU speech
![wordsTrend.png](https://github.com/shngli/R-data-analysis/blob/master/State%20of%20the%20Union%20text%20analysis/wordsTrend.png)

2) Number of sentences used in every SOTU speech
![senTrend.png](https://github.com/shngli/R-data-analysis/blob/master/State%20of%20the%20Union%20text%20analysis/senTrend.png)

3) Proportion of words that are 4 letters or shorter used in every SOTU speech
![shortTrend.png](https://github.com/shngli/R-data-analysis/blob/master/State%20of%20the%20Union%20text%20analysis/shortTrend.png)

4) Similarity of presidents, grouped by pre-World War II and post-World War II era
![presDist.png](https://github.com/shngli/R-data-analysis/blob/master/State%20of%20the%20Union%20text%20analysis/presDist.png)

5) Hierarchical clustering of presidents by similarity of their SOTU speeches
![presCluster.png](https://github.com/shngli/R-data-analysis/blob/master/State%20of%20the%20Union%20text%20analysis/presCluster.png)

6) Alternative dendrogram of presidents colored by their similarity
![presCluster2.png](https://github.com/shngli/R-data-analysis/blob/master/State%20of%20the%20Union%20text%20analysis/presCluster2.png)
