####################
# Variables
####################

# 1. create a new variable called 'my.num' that contains 5 numbers
my.num = c(5,4,6,8,9)

# 2. mulitply 'my.num' by 2
my.num*2
#[1] 10  8 12 16 18

# 3. create a second variable called 'my.char' that contains 3 character strings
my.char = c("abc","def","ghi")

# 4. combine the two variables 'my.num' and 'my.char' into a variable called 'both'
both = c(my.num, my.char)
#> both
#[1] "5"   "4"   "6"   "8"   "9"   "abc" "def" "ghi"

# 5. what is the length of 'both'?
length(both)
#[1] 8

# 6. what class is 'both'?
class(both)
#[1] "character"

# 7. divide 'both' by 3, what happens?
both/3 
#Error in both/3 : non-numeric argument to binary operator

# 8. create a vector with elements 1 2 3 4 5 and call it x
x = 1:5
x = c(1,2,3,4,5)
#> x
#[1] 1 2 3 4 5

# 9. create another vector with elements 10 20 30 40 and call it y
y = c(10,20,30,40)
y = seq(10,40,by=10)
#> y
#[1] 10 20 30 40

# 10. what happens if you try to add x and y together? why?
x+y 	# y[1] gets recycled
#[1] 11 22 33 44 15
#Warning message:
#In x + y : longer object length is not a multiple of shorter object length

# 11. append the value 50 onto the vector y (hint: you can use the c() function)
y = c(y,50)
y = seq(10,50,by=10)
y = c(10,20,30,40,50)
#> y
#[1] 10 20 30 40 50

# 12. add x and y together
x+y
#[1] 11 22 33 44 55

# 13. multiple x and y together. pay attention to how R performs operations on vectors of the same length.
x*y
#[1]  10  40  90 160 250
x %*% y
#     [,1]
#[1,]  550
sum(x*y)
#[1] 550



