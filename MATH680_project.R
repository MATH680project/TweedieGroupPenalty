setwd("C:/Users/paul/Desktop/McGill-Graduate/F2016/MATH 680/Project/typed/code and data")

data = load("dat_clean.rda")
head(dat)
str(dat)
 
set.seed(680)
p      = runif(1, 1,2)
lambda = runif(1, 1,10)
n      = nrow(dat)
v.i    = 1/n

# need to set w.j

# need to create the groups 



# y.i is the i-th response
# x.i is the i-th row 

v.tilde.i = v.i*((p-1)*y.i*exp(-(p-1)*(b0.tilde+b.tilde%*%x.i))+(2-p)*exp((2-p)*(b0.tilde+b.tilde%*%x.i)))

y.tilde.i = b0.tilde + 
            b.tilde*x.i+
            (v.i/v.tilde.i)*(y.i*exp(-(p-1)*(b0.tilde+b.tilde%*%x.i))+exp((2-p)*(b0.tilde+b.tilde%*%x.i)))

l.Q = (1/2)*v.tilde.i*(y.tilde.i-b0-b%*%x.i)^2 # missing that constant terms he has, not sure what it is 

# x.ij is for the i-th observation and ?j-th block? 

U.tilde.j = -v.tilde.i*(y.tilde.i-b0-b%*%x.i)*x.ij

H.tilde.j = v.tilde.i*x.ij%*%t(x.ij)

# from H.tilde.j you need to somehow find the ?max eigenvalue, call it gamma.j?
# NOTE : need to take only the positive part which is a bit confusing (look eq.12 in his paper).  
# Is not that just the absolute value here?  

b.moon.j.new  = ((gamma.j*beta.moon.j-U.moon.j)*(1-(gamma*w.j)/(<some crap>)))/gamma.j

# update the intercept using U.moon.0 and gamma.tilde.0 = H.tilde.0 = tralalala 

# try to change 
