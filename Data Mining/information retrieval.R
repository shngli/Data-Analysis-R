library(tm)
load(file="docs.Rdata")

# docs.Rdata contains 8 Wikipedia articles, 4 about the TMNT Leonardo, Raphael, Michelangelo and Donatello
# and 4 about the painters of the same name
corp = VCorpus(VectorSource(docs))
dtm = DocumentTermMatrix(corp, control=list(tolower=TRUE,removePunctuation=TRUE,removeNumbers=TRUE))

mydtm = as.matrix(dtm)

# Display the terms from column 100 to column 110
# Row 1 to 8 are tmnt characters and the artists, row 9 represent the terms to be queried
mydtm[,100:110]
##    Terms
##Docs adopted adopteesfictional adoption adopts adoration adorn adrian adulthood advance advanced advantage
##   1       0                 1        0      0         0     0      0         0       0        0         1
##   2       0                 1        0      0         0     0      0         0       0        0         2
##   3       1                 1        1      1         0     0      0         0       2        0         0
##   4       0                 1        0      0         0     0      0         0       0        0         0
##   5       2                 0        0      0         7     0      0         0       0        0         0
##   6       0                 0        0      0         0     0      0         0       0        1         0
##   7       0                 0        0      0         0     1      0         2       0        0         1
##   8       0                 0        0      0         0     0      1         0       0        0         0
##   9       0                 0        0      0         0     0      0         0       0        0         0

# Compute unnormalized distance
# Query for "raphael is cool but rude, michelangelo is a party dude" in alphabetical order
q = c("but","cool","dude","party","michelangelo","raphael","rude")

# Calculate l2 Euclidean distance 
dist = sqrt(rowSums((scale(mydtm,center=mydtm[9,],scale=F)^2)))

dist
##       1        2        3        4        5        6        7        8        9 
##309.4527 185.1837 330.9698 220.1999 928.4670 646.4743 527.2561 196.1403   0.0000 

mat = cbind(mydtm[,q],dist)
mat
##  but cool dude party michelangelo raphael rude     dist
##1  19    0    0     0            4      24    0 309.4527
##2   8    1    0     0            7      45    1 185.1837
##3   7    0    4     3           77      23    0 330.9698
##4   2    0    0     0            4      11    0 220.1999
##5  17    0    0     0            9       6    0 928.4670
##6  36    0    0     0           17     101    0 646.4743
##7  10    0    0     0          159       2    0 527.2561
##8   2    0    0     0            0       0    0 196.1403
##9   1    1    1     1            1       1    1   0.0000

colnames(mat) = c(q,"dist")

# Different documents have different lengths. Total word counts:
rowSums(mydtm)
##   1    2    3    4    5    6    7    8    9 
##3114 1976 3330 2143 8962 6524 4618 1766    7

# The Wikipedia entry on Leonardo the painter is almost 3x as long as that of TMNT Leonardo 
# (8962 vs 3114 words), and the query is only 7 words long! We should normalize the document length.

# Document length normalization: divide document X by its sum of words (dl = document length)
mydtm.dl = mydtm/rowSums(mydtm)
dist.dl = sqrt(rowSums((scale(mydtm.dl,center=mydtm.dl[9,],scale=F)^2)))
mat.dl = cbind(mydtm.dl[,q],dist.dl)

colnames(mat.dl) = c(q,"dist.dl")
rownames(mat.dl) = paste(rownames(mat.dl),
c("(tmnt leo)","(tmnt rap)","(tmnt mic)","(tmnt don)",
"(real leo)","(real rap)","(real mic)","(real don)", "query"))

mat.dl
##                      but         cool        dude        party michelangelo      raphael         rude   dist.dl
##1 (tmnt leo) 0.0061014772 0.0000000000 0.000000000 0.0000000000  0.001284522 0.0077071291 0.0000000000 0.3852650
##2 (tmnt rap) 0.0040485830 0.0005060729 0.000000000 0.0000000000  0.003542510 0.0227732794 0.0005060729 0.3777634
##3 (tmnt mic) 0.0021021021 0.0000000000 0.001201201 0.0009009009  0.023123123 0.0069069069 0.0000000000 0.3781194
##4 (tmnt don) 0.0009332711 0.0000000000 0.000000000 0.0000000000  0.001866542 0.0051329911 0.0000000000 0.3887862
##5 (real leo) 0.0018968980 0.0000000000 0.000000000 0.0000000000  0.001004240 0.0006694934 0.0000000000 0.3906030
##6 (real rap) 0.0055180871 0.0000000000 0.000000000 0.0000000000  0.002605763 0.0154812998 0.0000000000 0.3820197
##7 (real mic) 0.0021654396 0.0000000000 0.000000000 0.0000000000  0.034430489 0.0004330879 0.0000000000 0.3812202
##8 (real don) 0.0011325028 0.0000000000 0.000000000 0.0000000000  0.000000000 0.0000000000 0.0000000000 0.3935327
##9 query      0.1428571429 0.1428571429 0.142857143 0.1428571429  0.142857143 0.1428571429 0.1428571429 0.0000000


# l2 length normalization: divide document X by its l2 length
mydtm.l2 = mydtm/sqrt(rowSums(mydtm^2))
dist.l2 = sqrt(rowSums((scale(mydtm.l2,center=mydtm.l2[9,],scale=F)^2)))
mat.l2 = cbind(mydtm.l2[,q],dist.l2)

colnames(mat.l2) = c(q,"dist.l2")
rownames(mat.l2) = paste(rownames(mat.l2),
c("(tmnt leo)","(tmnt rap)","(tmnt mic)","(tmnt don)",
"(real leo)","(real rap)","(real mic)","(real don)", "query"))

mat.l2
##                     but        cool       dude       party michelangelo     raphael        rude  dist.l2
##1 (tmnt leo) 0.061370841 0.000000000 0.00000000 0.000000000  0.012920177 0.077521062 0.000000000 1.373041
##2 (tmnt rap) 0.043126841 0.005390855 0.00000000 0.000000000  0.037735986 0.242588482 0.005390855 1.321871
##3 (tmnt mic) 0.021128664 0.000000000 0.01207352 0.009055142  0.232415303 0.069422753 0.000000000 1.319048
##4 (tmnt don) 0.009080128 0.000000000 0.00000000 0.000000000  0.018160256 0.049940705 0.000000000 1.393433
##5 (real leo) 0.018309146 0.000000000 0.00000000 0.000000000  0.009693077 0.006462051 0.000000000 1.404972
##6 (real rap) 0.055666627 0.000000000 0.00000000 0.000000000  0.026287018 0.156175816 0.000000000 1.349070
##7 (real mic) 0.018954698 0.000000000 0.00000000 0.000000000  0.301379698 0.003790940 0.000000000 1.324758
##8 (real don) 0.010197182 0.000000000 0.00000000 0.000000000  0.000000000 0.000000000 0.000000000 1.411486
##9 query      0.377964473 0.377964473 0.37796447 0.377964473  0.377964473 0.377964473 0.377964473 0.000000


# Compare two normalization schemes
a = cbind(mat.dl[,8],mat.l2[,8])
colnames(a) = c("dist/doclen","dist/l2len")

a
##             dist/doclen dist/l2len
##1 (tmnt leo)   0.3852650   1.373041
##2 (tmnt rap)   0.3777634   1.321871
##3 (tmnt mic)   0.3781194   1.319048
##4 (tmnt don)   0.3887862   1.393433
##5 (real leo)   0.3906030   1.404972
##6 (real rap)   0.3820197   1.349070
##7 (real mic)   0.3812202   1.324758
##8 (real don)   0.3935327   1.411486
##9 query        0.0000000   0.000000