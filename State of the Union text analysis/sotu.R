# Author: Sheng Li
## Load data
sotu <- readLines(file("stateoftheunion1790-2012.txt"))

# Part 1) Data Munging and variables transformation
# Identify *** as the breaks between every speech
breaks <- grep("\\*\\*\\*", sotu)

# Extract the date of every SOTU speech from 4 lines below every *** break into tempDates
tempDates <- as.character(sotu[breaks + 4])
tempDates <- tempDates[c(1:222)]

# Extract the year of every SOTU speech from tempDate into Year
sotuYr <- as.numeric((gregexpr("[[:digit:]][[:digit:]][[:digit:]][[:digit:]]", tempDates)))
sotuYear <- as.numeric(substr(tempDates, sotuYr, sotuYr + 4))

# Extract the month of every SOTU speech from tempDates into Month
sotuMonth <- gsub("[[:blank:]][[:digit:]]+[,][[:blank:]][[:digit:]]+", "", tempDates)
sotuMonth <- as.numeric(sapply(sotuMonth, switch, "January" = 1, "February" = 2, "March" = 3, "April" = 4,
                           "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9,
                           "October" = 10, "November" = 11, "December" = 12))

# Extract the name of the president of every SOTU speech from 3 lines below every *** break
sotuPresidents <- sotu[breaks + 3]
sotuPresidents <- sotuPresidents[c(1:222)]

# Combine every SOTU speech into a single list, then split the speeches into separate lines by ".", "?" and "!"
combine = list()
for (i in 1:221){
  combine[[i]] = paste(sotu[(breaks[i]+6):breaks[i+1]-1], sep=" ", collapse = " ")
}
combine[[222]] <- paste(sotu[(breaks[222]+6):breaks[222+1]-1], sep=" ", collapse = " ")
sotuList <- sapply(combine, function(x) strsplit(x, "[\\?|\\.|\\!]"))

#############################################

# Part 2) Data wrangling to create a word vector to count the number of occurrences of every word used in each SOTU speech
#install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")
library(Rstem)

speechToWords = function(sentences){
  # sentences is character vector of sentences for each speech
  s1=gsub("'","",sentences)
  s2=gsub("[[:digit:]]","",s1)
  s3=gsub("(Applause.)","",s2)
  # Use gsub to eliminate apostrophes and numbers 
  # Drop the phrase (Applause.)
  # Turn characters to lower case.
  s4=tolower(s3)
  
  # Use strsplit to split the text up by blanks and punctuation
  s5=strsplit(s4,"([[:blank:]]|[[:punct:]])")
  # Unlist the return value 
  s6=unlist(s5)
  
  # Drop any empty words 
  s7=s6[s6!=""]
  
  # Use wordStem() to stem the words
  s8=wordStem(s7)  
  # return a character vector of all words in the speech
  return(s8)
}

# Extract every word used in each SOTU speech into a list Words
sotuWords <- lapply(sotuList, speechToWords)

# Sort the list of words into unique terms
#12604 words
uniqueWords <- sort(unique(unlist(sotuWords)))

# Create a word vector for every SOTU speech
vector <- rep(0, length(uniqueWords))
names(vector) <- uniqueWords

wordVector = lapply(sotuWords, function(x){
  vector[names(table(x))]=table(x)
  return(vector)
})

# From the word vector, create a matrix with columns corresponding to speeches and rows to words 
wordMatrix <- matrix(unlist(wordVector), ncol  = length(wordVector), byrow = FALSE)
	
#############################################

# Part 3) Exploratory Analysis (Incomplete)
# Create the final data structure in IntermediateResults.Rda that has 2 variables:
# 1) Pres (president's name) and 2) party (president's affliated political party).
library(ggplot2)
load("IntermediateResults.Rda")

# Add the variables to complete the data frame:
# 1. yr: SOTU year
speechesDF$yr <- sotuYear
#  [1] 1790 1790 1791 1792 1793 1794 1795 1796 1797 1798 1799 1800 1801 1802 1803 1804 1805 1806 1807 1808 1809 1810 1811 1812
# [25] 1813 1814 1815 1816 1817 1818 1819 1820 1821 1822 1823 1824 1825 1826 1827 1828 1829 1830 1831 1832 1833 1834 1835 1836
# [49] 1837 1838 1839 1840 1841 1842 1843 1844 1845 1846 1847 1848 1849 1850 1851 1852 1853 1854 1855 1856 1857 1858 1859 1860
# [73] 1861 1862 1863 1864 1865 1866 1867 1868 1869 1870 1871 1872 1873 1874 1875 1876 1877 1878 1879 1880 1881 1882 1883 1884
# [97] 1885 1886 1887 1888 1889 1890 1891 1892 1897 1898 1899 1900 1901 1902 1903 1904 1905 1906 1907 1908 1909 1910 1911 1912
#[121] 1913 1914 1915 1916 1917 1918 1919 1920 1921 1922 1923 1924 1925 1926 1927 1928 1929 1930 1931 1932 1934 1935 1936 1937
#[145] 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 1953 1953 1954 1955 1956 1957 1958 1959 1960
#[169] 1961 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983
#[193] 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2001 2002 2003 2004 2005 2006
#[217] 2007 2008 2009 2010 2011 2012

# 2. month: SOTU month
speechesDF$month <- sotuMonth
#  [1]  1 12 10 11 12 11 12 12 11 12 12 11 12 12 10 11 12 12 10 11 11 12 11 11 12  9 12 12 12 11 12 11 12 12 12 12 12 12 12 12 12
# [42] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
# [83] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
#[124] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1
#[165]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  2  1  1  2  1  1  1  2  1  1  1
#[206]  2  1  1  1  2  9  1  1  1  2  1  1  1  2  1  1  1

# 3. words: number of words in each SOTU speech
speechesDF$words <- sapply(sotuWords, length)
#  [1]  1089  1403  2304  2092  1965  2916  1988  2873  2058  2218  1505  1373  3220  2199  2268  2098  2933  2862  2389  2681
# [21]  1832  2442  2273  3249  3259  2115  3143  3363  4410  4374  4703  3436  5805  4713  6345  8399  8961  7654  6923  7262
# [41] 10514 15086  7181  7862  7881 13406 10794 12356 11430 11472 13415  8976  8222  8393  8009  9313 16025 18134 16335 21238
# [61]  7613  8310 13166  9903  9543 10099 11589 10470 13571 16261 12247 13983  6951  8292  6078  5832  9221  7068 11921  9690
# [81]  7661  8705  6462  3969  9949  9170 12148  6730  7956  7787 11536  6685  3743  2965  3599  8808 19651 15024  5250  8885
#[101] 12922 11425 16127 13296 12059 19954 14958 18910 19696  9805 14833 17488 25089 23614 27410 19443 13849  6397 23519 25076
#[121]  3565  4549  7708  2125  3933  5470  4764  2698  5605  5755  6694  6946 10795 10249  8723  7989 10889  4478  5657   724
#[141]  2231  3522  3819  2742  4703  3778  3215  3325  3486  4586  3820  8197 27234  6048  5084  3402  5133  3999  5358  9691
#[161]  6946  6008  7284  8283  4171  4955  4914  5677  6123  5260  6586  5375  3186  4424  5283  7117  4864  4080  4442  4514
#[181]  3974  1668  5116  4058  4943  4633  4560  3247  3405 33552  5169  5600  4963  4247  3517  3811  4866  4794  3773  3814
#[201]  4743  6958  7399  9181  6296  6750  7294  7477  7415  4345  2971  3827  5340  5157  5036  5303  5410  5752  6061  7217
#[221]  6866  7014

# 4. chars: number of letters in each SOTU speech
speechesDF$chars <- sapply(sotuWords, function(x) sum(nchar(x)))
#  [1]   4693   5956   9819   8874   8235  12405   8595  12271   8796   9488   6491   5916  13626   9278   9966   8921  12232
# [18]  12215  10080  11384   7710  10577   9802  13871  13769   9052  13670  14280  18674  18764  19784  14459  24305  19895
# [35]  26965  35361  37725  32331  29693  30926  44244  63939  30327  33184  33160  56541  45397  51924  48514  49119  56486
# [52]  38673  34398  35219  33533  39207  67849  76170  68735  90024  32560  35102  55673  42055  40812  43522  49838  44817
# [69]  57927  69826  52482  59580  29266  34870  25922  24989  38698  30413  50208  41809  32204  36559  27205  16760  42076
# [86]  38508  51023  28466  33802  33577  49557  28803  16627  12806  15827  38219  84499  64556  22161  38275  54519  48681
#[103]  67855  56205  51046  85772  64706  81897  82485  40899  63405  73770 104668  98037 114604  82022  59324  28280 100256
#[120] 107804  14642  18158  32027   9008  15965  22482  20123  11404  23797  24686  28615  29823  46174  43688  37752  34760
#[137]  47667  19845  25332   3301   9544  14889  15777  11744  19709  15905  13303  13854  14139  18675  15693  34477 119916
#[154]  26125  21581  14618  21645  16319  21658  40267  30519  26554  32543  36527  18324  21456  21338  24742  27818  22354
#[171]  27855  22463  13216  18197  21782  29653  20379  16706  18086  18310  16372   6907  20752  17225  20907  19657  18863
#[188]  13819  14368 151171  22001  23498  20953  17944  14664  15880  20354  19745  15306  15891  18977  27912  30405  36517
#[205]  25982  27975  30242  31168  31416  18110  12567  16249  22646  21824  21305  22444  22285  24204  24646  29805  28125
#[222]  28992

# 5. sent: number of sentences in each SOTU speech
speechesDF$sent <- sapply(sotuList, length)
#  [1]   25   40   60   63   56   79   53   82   60   59   37   41   91   64   51   53   81   79   65   76   41   65   48   90
# [25]   72   51   63   73  131  119  135   89  169  130  205  258  216  180  170  204  309  396  169  201  187  321  241  299
# [49]  286  267  346  185  213  213  197  273  466  498  457  626  222  243  381  290  234  269  276  256  423  553  440  507
# [73]  226  370  227  238  277  205  386  303  253  298  217  123  303  280  363  205  238  272  349  207  158  138  197  315
# [97]  588  447  130  261  459  387  494  468  388  656  474  657  685  339  586  542  806  676  897  640  392  312  755  763
#[121]  110  177  228   54  139  164  163   74  211  227  367  279  487  448  453  382  410  164  253   62   80  143  174  110
#[145]  176  174  124  151  179  204  172  339 1745  297  287  188  240  246  301  438  354  292  342  380  191  263  269  261
#[169]  279  216  278  247  143  258  252  373  266  216  204  199  178   61  212  230  279  219  252  162  168 1574  270  275
#[193]  311  238  190  220  244  299  248  249  305  297  378  461  367  358  371  407  421  290  195  217  310  282  245  281
#[217]  369  313  292  437  400  424

# 6. short: proportion of words that are >= 4 letters
speechesDF$short <- sapply(sotuWords, function(x) sum(nchar(x) <= 4)/length(x))
#  [1] 0.5785124 0.6079829 0.6028646 0.6008604 0.6045802 0.6076818 0.5835010 0.5892795 0.5996113 0.5924256 0.5774086 0.5855790
# [13] 0.6000000 0.6098226 0.5846561 0.6120114 0.6167746 0.5943396 0.5981582 0.5923163 0.6069869 0.5819001 0.5833700 0.5967990
# [25] 0.5958883 0.6033097 0.5777919 0.5908415 0.6052154 0.5813900 0.6068467 0.6024447 0.6041344 0.5998303 0.5968479 0.6028099
# [37] 0.6127664 0.6148419 0.5935288 0.5998348 0.5994864 0.5969110 0.6039549 0.6055711 0.6128664 0.6030136 0.6038540 0.6144383
# [49] 0.6062992 0.5944038 0.6090943 0.5823307 0.6106787 0.6047897 0.6129354 0.6005584 0.6043682 0.6112827 0.6135904 0.6062247
# [61] 0.5945094 0.6117930 0.6105879 0.6017368 0.5904852 0.5852065 0.5830529 0.5937918 0.5953135 0.5890167 0.5889606 0.5916470
# [73] 0.6098403 0.6107091 0.5931227 0.6087106 0.6060080 0.5878608 0.6090093 0.5876161 0.6134969 0.6083860 0.6053853 0.6097254
# [85] 0.6081013 0.6062159 0.6098946 0.6059435 0.5940171 0.5918839 0.5912795 0.5817502 0.5655891 0.5871838 0.5579328 0.5856040
# [97] 0.5855173 0.5891906 0.6087619 0.5894204 0.6122891 0.6107659 0.6151795 0.6212395 0.6006302 0.5891049 0.5816286 0.5818086
#[109] 0.6155057 0.6215196 0.5938111 0.6059584 0.6132169 0.6223003 0.6155418 0.6059250 0.5892122 0.5607316 0.5961138 0.5904052
#[121] 0.6339411 0.6643218 0.6237675 0.5957647 0.6478515 0.6398537 0.5988665 0.6119348 0.5889384 0.5862728 0.5748431 0.5871005
#[133] 0.5881427 0.5910821 0.5843173 0.5786707 0.5698411 0.5591782 0.5508220 0.5331492 0.5880771 0.5914253 0.6203195 0.5922684
#[145] 0.6055709 0.6101112 0.6314152 0.6147368 0.6540448 0.6530746 0.6376963 0.6185190 0.5695454 0.5767196 0.5952006 0.5825985
#[157] 0.6123125 0.6404101 0.6543486 0.6283149 0.5577311 0.5514314 0.5448929 0.5590969 0.5581395 0.5830474 0.5828246 0.5689625
#[169] 0.5389515 0.5990494 0.6041603 0.6191628 0.6343377 0.6306510 0.6360023 0.6297597 0.6247944 0.6477941 0.6337235 0.6411165
#[181] 0.6389029 0.6187050 0.6594996 0.5936422 0.6097512 0.5974531 0.6302632 0.6076378 0.6093979 0.5433357 0.6142387 0.6255357
#[193] 0.6167641 0.6173770 0.6238271 0.6266072 0.6280312 0.6408010 0.6578320 0.6318825 0.6736243 0.6707387 0.6507636 0.6821697
#[205] 0.6513659 0.6374815 0.6417604 0.6332754 0.6204990 0.6299194 0.6223494 0.6109224 0.6031835 0.6183828 0.6127879 0.6126721
#[217] 0.6465804 0.6253477 0.6700214 0.6506859 0.6596271 0.6536926

# Explore the speeches using this data frame.  
# Plot the number of words used in every SOTU speech
png("/wordsTrend.png", height=1000, width=1500)
ggplot(speechesDF, aes(x=yr, y=words)) + geom_line() +
    ggtitle("Number of Words in SOTU Speeches") + ylab("Number of words") + xlab("Year")
#plot(speechesDF$yr, speechesDF$words, type = "l", main = "Number of Words in SOTU Speeches", xlab = "Year", ylab = "Number of words")
dev.off()

# Plot the number of sentences used in every SOTU speech
png("/senTrend.png", height=1000, width=1500)
ggplot(speechesDF, aes(x=yr, y=sent)) + geom_line() +
    ggtitle("Number of Sentences in SOTU Speeches") + ylab("Number of sentences") + xlab("Year")
#plot(speechesDF$yr, speechesDF$sent, type = "l", main = "Number of Sentences in SOTU Speeches", xlab = "Year", ylab = "Number of sentences")
dev.off()

# Plot the proportion of short words used in every SOTU speech
png("/shortTrend.png", height=1000, width=1500)
ggplot(speechesDF, aes(x=yr, y=short)) + geom_line() +
    ggtitle("Proportion of short words in SOTU Speeches") + ylab("Proportion of short words") + xlab("Year")
#plot(speechesDF$yr, speechesDF$short, type = "l", main = "Proportion of short words in SOTU Speeches", xlab = "Year", ylab = "Proportion of short words")
dev.off()

# Plotting the above 3 trends together
library(grid)
library(gridExtra)
p1 <- ggplot(speechesDF, aes(x=yr, y=words)) + geom_line() +
    ggtitle("Number of Words in SOTU Speeches") + ylab("Number of words") + xlab("Year")
p2 <- ggplot(speechesDF, aes(x=yr, y=sent)) + geom_line() +
    ggtitle("Number of Sentences in SOTU Speeches") + ylab("Number of sentences") + xlab("Year")
p3 <- ggplot(speechesDF, aes(x=yr, y=short)) + geom_line() +
    ggtitle("Proportion of short words in SOTU Speeches") + ylab("Proportion of short words") + xlab("Year")

png("/threeTrends.png", height=1000, width=1500)
grid.arrange(p1, p2, p3)
dev.off()

# To be fix: Make a plot comparing speeches/presidents.  

#############################################

# Part 4) Computing the distances between every SOTU speech

# Load computeSJDistance.R to compute the linguinstic distance between every president using the Shannon-Jensen metric
source("computeSJDistance.R")

# Create a matrix presMatrix by adding the word vectors for the speecbes made by the same president
pres <- unique(speechesDF$Pres)

presMatrix <- matrix(0, nrow=length(uniqueWords),ncol=length(pres))
for(i in 1:length(pres)){
  if(sum(speechesDF$Pres==pres[i])!=1){
    presMatrix[,i]=rowSums(wordMatrix[,speechesDF$Pres==pres[i]])
  }else{
    presMatrix[,i]=wordMatrix[,speechesDF$Pres==pres[i]]
  }
}

# Compute the document frequency df by calculating the number of presidents that used every word
df <- list()
for(i in 1:length(uniqueWords)){
  df[i] = sum(presMatrix[i,]!=0)
}
df <- unlist(df)

# Calculate the distance between every president based on the similarity of words that they used in their SOTU speech
# The lower distance values denote higher similarity
presDist <- computeSJDistance(tf = presMatrix, df=df, terms=uniqueWords)
rownames(presDist) <- unique(pres)
colnames(presDist) <- unique(pres)

# Plot a multidimensional scaling of the presidents' similarity according depending on pre or post WWII presidency
png("/presDist.png", height=900, width=900)
coordinates <- cmdscale(presDist)
preWW2 <- coordinates[1:28,]
postWW2 <- coordinates[29:41,]
plot(preWW2,xlab="Dimension 1",ylab="Dimension 2",
     main="Multidimensional Comparison of Presidents' SOTU Speeches",
     pch=23,col="Blue",xlim=c(-0.04,0.07),ylim=c(-0.04,0.03))
points(postWW2, pch=21, col="Red")
for(i in 1:41){
  text(coordinates[i,1], coordinates[i,2]-0.001, pres[i], cex=0.85)
}
points(-0.035,0.028,pch=23,col="Blue")
text(-0.02,0.028,"Pre-World War II Presidents",cex=1.25)
points(-0.035,0.026,pch=21,col="Red")
text(-0.0195,0.026,"Post-World War II Presidents",cex=1.25)
dev.off()

# Plot a hierarchical clustering of the presidents' similarity
library(ggdendro)

hc <- hclust(as.dist(presDist))
png("/presCluster.png", height=800, width=1000)
ggdendrogram(hc, rotate = FALSE, size = 2) + labs(title="SOTU Presidents Cluster Dendrogram")
dev.off()
# Alternative clustering plot
png("/presCluster2.png", height=800, width=1000)
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(hc, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("#FF6B6B", 
    "#4ECDC4", "#556270"))
dev.off()

#plot(hclust(as.dist(presDist)),sub="",xlab="",ylab="", main="SOTU Presidents Cluster Dendrogram")


