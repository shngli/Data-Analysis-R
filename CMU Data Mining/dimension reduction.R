# Example: Principal component analysis in 3-D space
# Shown are the 3 principal component directions and the scores from projecting
# onto the first two directions.

library(scatterplot3d)
set.seed(0)

# Create 2000 random points in 3d space
n = 2000
tt = seq(0,2*pi,length=n)
x = cos(tt)+rnorm(n,sd=0.1)
y = sin(tt)+rnorm(n,sd=0.1)
z = rnorm(n,sd=0.1)
lim = range(c(x,y,z))

xx = cbind(x,y,z)
a = princomp(xx)
v = a$loadings # directions
s = a$scores   # scores

s3d = scatterplot3d(x,y,z,xlim=lim,ylim=lim,zlim=lim,
  xlab="",ylab="",zlab="",grid=FALSE,
  main="First three principal component directions")
s3d$points3d(rbind(c(0,0,0),v[,1]),type="l",lwd=3,col="blue")
s3d$points3d(rbind(c(0,0,0),v[,2]),type="l",lwd=3,col="red")
s3d$points3d(rbind(c(0,0,0),v[,3]),type="l",lwd=3,col="darkgreen")

plot(s[,1],s[,2],main="First two principal component scores",
     xlab="Score 1",ylab="Score 2")


###############################################################################################


# Example: proportion of variance explained as a function of k, for the donut data
pv = cumsum(a$sdev^2)/sum(a$sdev^2)
plot(1:ncol(xx),pv,type="b",ylim=c(0,1),
xlab="Number of component directions",
ylab="Proportion of variance explained")


###############################################################################################


# Example: Principal component analysis with 12 features
# Players stats from 2012 Cadillac Championship golf tournament, 72 golfers with 12 features

load("playerstats.Rdata")

# Remove the first 5 columns and column 11 from players stats
x = as.matrix(playerstats[,-c(1:5,11)])
a = prcomp(x,center=TRUE,scale=TRUE)
dirs = a$rotation
scrs = a$x

round(dirs[,1:2],3)
# Create namefinish from cloumn 3 and 5 of players stats (last name and finish)
namefinish = paste(playerstats[,3],playerstats[,5])

##namefinish
## [1] "Mickelson T43"    "Stricker T8"      "Singh T66"        "Bjorn T24"        "Clarke T43"       "Lawrie T60"      
## [7] "Jimenez T45"      "Karlsson T20"     "Goosen T45"       "Westwood T29"     "Chalmers T20"     "Senden T6"       
##[13] "Hansen T29"       "Garcia T60"       "Jacobson 68"      "Wilson T45"       "Howell III T17"   "Ogilvy T55"      
##[19] "Baddeley 12"      "Otto 50"          "Rose 1"           "Hanson T4"        "Kuchar T8"        "Colsaerts T35"   
##[25] "Crane T51"        "Hiratsuka 70"     "Van Pelt T8"      "Donald T6"        "Johnson T17"      "Fraser T24"      
##[31] "Poulter T60"      "Choi T35"         "Scott T13"        "Dyson 72"         "Yang 59"          "Mahan T24"       
##[37] "Haas T29"         "Byrd T35"         "Fdez-Castano T55" "Molinari T13"     "Casey T51"        "McDowell T13"    
##[43] "Dufner T29"       "Watson 2"         "Oosthuizen T60"   "Schwartzel T4"    "Pagunsan T35"     "Reavie T35"      
##[49] "Cabrera Bello 65" "Rock T24"         "Takayama T60"     "Wagner T13"       "Quiros T57"       "Watney T17"      
##[55] "Mulroy T29"       "Noren 69"         "Kaymer T20"       "Snedeker T45"     "Laird T24"        "Day T20"         
##[61] "McIlroy 3"        "Kim T51"          "Bae 71"           "Simpson T35"      "Larrazabal T66"   "Grace T35"       
##[67] "Kruger T57"       "Stanley T51"      "Johnson T35"      "Woodland T29"     "Fowler T45"       "Bradley T8"  

# Projecting the first 2 principal component scores 
plot(scrs[,1],scrs[,2],xlim=range(scrs[,1])*1.5,ylim=range(scrs[,2])*1.5)
# long process to plot the last line of code
identify(scrs[,1],scrs[,2],labels=namefinish,n=10)

