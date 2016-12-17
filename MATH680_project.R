#MATH 680 – Computation Intensive Statistics
#Final Project
#The Tweedie and Group Penalty Problem

#December 22, 2016

#Prepared by:
#Pavel Slavchec & Roba Bairakdar

#Submitted to:
#Yi Yang

######################################################################
#Data manipulation
load(paste(getwd(),"/dat_clean.rda",sep=""))
data=dat

#response variable
y=data[,"CLM_AMT5"]

#numerical variables
x_num=cbind(data[,"KIDSDRIV"],data[,"TRAVTIME"],data[,"CAR_USE"],data[,"BLUEBOOK"],
            data[,"NPOLICY"],data[,"RED_CAR"],data[,"REVOLKED"],data[,"MVR_PTS"],data[,"AGE"],
            data[,"HOMEKIDS"],data[,"YOJ"],data[,"INCOME"],data[,"GENDER"],data[,"MARRIED"],
            data[,"PARENT1"],data[,"HOME_VAL"],data[,"SAMEHOME"],data[,"AREA"])
name=c("KIDSDRIV","TRAVTIME","CAR_USE","BLUEBOOK","NPOLICY","RED_CAR","REVOLKED",
       "MVR_PTS","AGE","HOMEKIDS","YOJ","INCOME","GENDER","MARRIED","PARENT1","HOME_VAL","SAMEHOME"
       ,"AREA")
colnames(x_num)=name

#categorical variables
x_cat=cbind(data[,"CAR_TYPE_2"],data[,"CAR_TYPE_3"],data[,"CAR_TYPE_4"],data[,"CAR_TYPE_5"],
            data[,"CAR_TYPE_6"],data[,"JOBCLASS_2"],data[,"JOBCLASS_3"],data[,"JOBCLASS_4"],
            data[,"JOBCLASS_5"],data[,"JOBCLASS_6"],data[,"JOBCLASS_7"],data[,"JOBCLASS_8"],
            data[,"JOBCLASS_9"],data[,"MAX_EDUC_2"],data[,"MAX_EDUC_3"],data[,"MAX_EDUC_4"],
            data[,"MAX_EDUC_5"])
name=c("CAR_TYPE_2","CAR_TYPE_3","CAR_TYPE_4","CAR_TYPE_5","CAR_TYPE_6","JOBCLASS_2",
       "JOBCLASS_3","JOBCLASS_4","JOBCLASS_5","JOBCLASS_6","JOBCLASS_7","JOBCLASS_8",
       "JOBCLASS_9","MAX_EDUC_2","MAX_EDUC_3","MAX_EDUC_4","MAX_EDUC_5")
colnames(x_cat)=name

#predictors
x=cbind(x_num,x_cat)

#dimentions of variables
n=nrow(x)
p=ncol(x)
grouping=c(seq(1:ncol(x_num)),rep(ncol(x_num)+1,5),rep(ncol(x_num)+2,8),rep(ncol(x_num)+3,4))

######################################################################

# added on Dec. 16 

size.group = as.numeric(table(grouping)) # has length 21
J          = length(size.group)
w.j        = round(sqrt(as.numeric(table(grouping))), 3)
w.j        = c(w.j[1:ncol(x_num)], rep(w.j[ncol(x_num)+1], 5), rep(w.j[ncol(x_num)+2], 8), rep(w.j[ncol(x_num)+3], 4))
length(w.j) # check, has length 35 
#########################################################################################################

#Data analysis 
set.seed(680)
p      = runif(1, 1,2)
lambda = runif(1, 1,10)
n      = nrow(dat)
v.i    = 1/n

# y.i is the i-th response
# x.i is the i-th row 

#########################################################################################################

# added on Dec. 15 


b0.tilde  = rnorm(1)
b.tilde   = rnorm(p)
v.tilde = rep(0, 100)
v.i       = 1/100


v.tilde = v.i*((rho-1)*y[1:100]*exp(-(rho-1)*(b0.tilde+b.tilde%*%t(x[1:100,])))+
                 (2-rho)*exp((2-rho)*(b0.tilde+b.tilde%*%t(x[1:100,]))))

y.tilde = b0.tilde + 
  b.tilde%*%t(x[1:100,])+
  (rep(v.i, 100)/v.tilde)*(y[1:100]*exp(-(rho-1)*(b0.tilde+ b.tilde%*%t(x[1:100,])))
                           -exp((2-rho)*(b0.tilde+ b.tilde%*%t(x[1:100,]))))



#########################################################################################################
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
