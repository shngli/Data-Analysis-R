# Example: Commputing canonical directions and variates

#install.packages("classifly")
library(classifly)
data(olives)

# We are interested in the correlations between the region of origin and the fatty acid measurements

# Look at the dimensions and summary of the dataset
dim(olives)
##[1] 572  10]
colnames(olives)
## [1] "Region"      "Area"        "palmitic"    "palmitoleic" "stearic"     "oleic"       "linoleic"    "linolenic"  
## [9] "arachidic"   "eicosenoic"

# Select the first 10 rows of the data set
olives[1:10,]
##   Region         Area palmitic palmitoleic stearic oleic linoleic linolenic arachidic eicosenoic
##1       1 North-Apulia     1075          75     226  7823      672        36        60         29
##2       1 North-Apulia     1088          73     224  7709      781        31        61         29
##3       1 North-Apulia      911          54     246  8113      549        31        63         29
##4       1 North-Apulia      966          57     240  7952      619        50        78         35
##5       1 North-Apulia     1051          67     259  7771      672        50        80         46
##6       1 North-Apulia      911          49     268  7924      678        51        70         44
##7       1 North-Apulia      922          66     264  7990      618        49        56         29
##8       1 North-Apulia     1100          61     235  7728      734        39        64         35
##9       1 North-Apulia     1082          60     239  7745      709        46        83         33
##10      1 North-Apulia     1037          55     213  7944      633        26        52         30

# In column 1 (Region) of olives dataset, 323 rows labeled as '1', 98 rows labeled as '2' 
# and 151 rows labeled as '3'
table(olives[,1])
#  1   2   3 
#323  98 151


as.indmat = function(z) {
  z = as.factor(z)
  l = levels(z)
  b = as.numeric(z==rep(l,each=length(z)))
  return(matrix(b,length(z)))
}

# Take X ∈ R^(572x8) to contain the fatty acidy measurements, Y ∈ R^(572x3) to be an indicator matrix
# ie. each row of Y indicates the region with a 1 and otherwise has 0s
# select column 1 of olives dataset for y
y = as.indmat(olives[,1])
# select column 3-10 of olives dataset for x
x = as.matrix(olives[,3:10])


# Apply the cancor function to implement the canonical correlation 
# In canonical correlation analysis, we are looking for pairs of directions, one in each of the feature
# spaces of 2 datasets X ∈ R^(nxp) and Y ∈ R^(nxp) to maximize the covariance or correlation
cc = cancor(x,y,ycenter=F)
alpha = cc$xcoef
alpha
##                     [,1]          [,2]         [,3]          [,4]          [,5]          [,6]          [,7]
##palmitic    -3.721700e-05 -2.063809e-04 -0.001872670  4.909167e-04  2.623369e-04  1.137085e-04  1.524635e-04
##palmitoleic -1.750691e-04 -4.171885e-04 -0.001637201 -1.616203e-03 -1.263372e-04 -4.557448e-04  2.396575e-04
##stearic      3.841792e-05 -9.788537e-05 -0.001883404 -2.547249e-05  7.233830e-04 -7.662342e-04  6.360410e-04
##oleic       -7.478840e-06 -1.442237e-04 -0.001797507  8.600299e-05  5.879313e-05 -3.231186e-05  2.641819e-05
##linoleic    -1.447828e-05  2.812837e-05 -0.001758353  1.582663e-04  1.323487e-04 -5.786306e-05 -1.091649e-04
##linolenic   -5.592188e-04 -1.276851e-04 -0.001990617  3.545960e-04 -7.758693e-04 -4.281893e-03 -1.800164e-03
##arachidic    2.393391e-04  8.029737e-04 -0.001802390 -5.119165e-04 -7.455062e-04  9.406025e-04  2.020838e-03
##eicosenoic  -2.220599e-03 -2.381330e-04 -0.002807810  6.558428e-04 -7.890238e-04  1.769016e-03 -8.876308e-04
##                     [,8]
##palmitic     1.679320e-04
##palmitoleic  3.735009e-04
##stearic     -3.274389e-04
##oleic        4.410961e-05
##linoleic     5.836442e-06
##linolenic    2.144203e-03
##arachidic   -5.713078e-06
##eicosenoic  -2.882337e-03

beta = cc$ycoef
beta
##            [,1]          [,2]      [,3]
##[1,] -0.03671124 -9.295689e-05 0.0418121
##[2,]  0.04742222  7.878428e-02 0.0418121
##[3,]  0.04775067 -5.093268e-02 0.0418121

alpha[,1:2]*1e4
##                   [,1]       [,2]
##palmitic     -0.3721700 -2.0638090
##palmitoleic  -1.7506909 -4.1718850
##stearic       0.3841792 -0.9788537
##oleic        -0.0747884 -1.4422371
##linoleic     -0.1447828  0.2812837
##linolenic    -5.5921881 -1.2768509
##arachidic     2.3933905  8.0297369
##eicosenoic  -22.2059947 -2.3813299
beta[,1:2]
##            [,1]          [,2]
##[1,] -0.03671124 -9.295689e-05
##[2,]  0.04742222  7.878428e-02
##[3,]  0.04775067 -5.093268e-02

xvars = x %*% alpha
yvars = y %*% beta


# Plot the first 2 canonical X variates, with the points colored by region
cols = c("red","darkgreen","blue")
par(mar=c(4.5,4.5,0.5,0.5))

plot(xvars[,1:2],col=cols[olives[,1]],
xlab="First canonical x variate",
ylab="Second canonical x variate")

legend("bottomleft",pch=21,col=cols,
legend=c("Region 1","Region 2","Region 3"))

