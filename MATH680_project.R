#MATH 680 – Computation Intensive Statistics
#Final Project
#The Tweedie and Group Penalty Problem

#December 22, 2016

#Prepared by:
#Pavel Slavchec & Roba Bairakdar

#Submitted to:
#Yi Yang

#######################################################################################
## Perform a Group Penalty Elastic Net algorithm
#######################################################################################
##  GroupLasso Function:
##    Arguments
##    
##      x:              an n row by p column data matrix, each row is
##                      an observed data point
##      y:              an n row by 1 column predictors
##      lambda:         tuning parameter, a randomly generated value from a 
##                      uniform distribution between 1 and 10
##      rho:            the power of the variance-mean. It is a 
##                      randomly generated value from a uniform
##                      distribution between 1 and 2
##      maxit:          the maximum number of iterations allowed
##      tol:            the convergence tolerance for the algorithm
##      grouping:       the groups
##      n:              the number of observations
##      b0.tilde.new:   the initialized intercept
##      b.tilde.new:    the initialized coefficients
##      x_num:          a matrix with only the numerical variables in the x matrix
##      x_cat:          a matrix with only the categorical variables in the x matrix
##      v.i:            the weight of each observation
##      tau:            tuning parameter, a randomly generated value from a 
##                      uniform distribution between 0 and 1
##
##   The function returns a list with elements
##      total.iterations: the total number of outer loop iterations taken
##      b0.tilde.new:     the intercept estimate
##      b.tilde.new:      the coefficient estimates
##
##  ConvCheck Function:
##    Arguments
##    
##      B.old:          the old coefficient estimates
##      B.new:          the new coefficient estimates
##      tol:            the convergence tolerance for the algorithm
#######################################################################################

#Needed Libraries
library(reshape)
library(ggplot2)

#######################################################################################
# simulation from Tweedie for Figure 1 in the PDF 
y <- rtweedie(1000, xi=1.75, mu=1, phi=2.5)

ggplot() + aes(y)+geom_histogram(aes(fill = ..count..),binwidth=1)+
  scale_fill_gradient("Count", low = "green", high = "red")+
  labs(title = "1000 Samples from a Tweedie Dsitribution")+ 
  xlab("Samples")+
  theme_classic() + theme(
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

#######################################################################################
#Checking codition of convergence
ConvCheck=function(B.old,B.new,tol){
  check=rep(NA,length(B.old))
  for (i in 1:length(B.old)){
    if(abs(B.old[i]-B.new[i])>tol){
      check[i]=1
    }else check[i]=0
  }
  return(sum(check))
}
            
#######################################################################################

#Group Lasso Function 
GroupLasso=function(y,x,lambda,rho,maxit,tol,grouping,n,b0.tilde.new,b.tilde.new,x_num,x_cat,v.i,tau){
  
  size.group = as.numeric(table(grouping)) # has length 21
  J          = length(size.group)
  w.j        = round(sqrt(as.numeric(table(grouping))), 3)
  w.j        = c(w.j[1:ncol(x_num)], rep(w.j[ncol(x_num)+1], 5), rep(w.j[ncol(x_num)+2], 8), rep(w.j[ncol(x_num)+3], 4))
  
  
  iterating_outer = TRUE
  total.iterations.outer=0
  
  while(iterating_outer){
    
    ###################### Outer layer #############################
    b0.tilde.old=b0.tilde.new
    b.tilde.old=b.tilde.new
    
    total.iterations.outer=total.iterations.outer+1
    
    v.tilde = v.i*((rho-1)*y*exp(-(rho-1)*(b0.tilde.old+b.tilde.old%*%t(x)))+
                     (2-rho)*exp((2-rho)*(b0.tilde.old+b.tilde.old%*%t(x))))
    
    y.tilde = b0.tilde.old + 
      b.tilde.old%*%t(x)+
      (rep(v.i, n)/v.tilde)*(y*exp(-(rho-1)*(b0.tilde.old+ b.tilde.old%*%t(x)))-
                               exp((2-rho)*(b0.tilde.old+ b.tilde.old%*%t(x))))
    
    H = vector("list", J)                 
    initial_g=0
    gammaj=rep(0,J)
    
    for(j in 1:J)
    {
      size   = size.group[j]
      initial_g=match(j,grouping)
      H[[j]] = matrix(0, nrow = size , ncol = size)
      for(i in 1:size)
      {
        for(k in 1:i)
        {
          H[[j]][i, k] = sum(v.tilde*x[,initial_g+i-1]*x[,initial_g+k-1])
          H[[j]][k, i] = H[[j]][i, k]
        }
      }
      gammaj[j]=eigen(H[[j]])$values[1]
    }
    gamma.0 = sum(v.tilde) 
    gamma   = c(gammaj[1:ncol(x_num)], rep(gammaj[ncol(x_num)+1], 5), rep(gammaj[ncol(x_num)+2], 8), rep(gammaj[ncol(x_num)+3], 4))
    ###################### Innter layer #############################
    iterating_inner=TRUE
    total.iterations.inner=0
    b.moon.new=b.tilde.old
    b0.moon.new=b0.tilde.old
    
    while(iterating_inner){
      
        total.iterations.inner=total.iterations.inner+1
      
        b.moon.old=b.moon.new
        b0.moon.old=b0.moon.new
        
        for (j in 1:J)
          # update block coefficients 
        {
          size   = size.group[j]
          initial_g=match(j,grouping)
          
          # compute U_j expression
          U.moon = matrix(-v.tilde*(y.tilde-b0.moon.old-b.moon.old%*%t(x)), nrow = 1 , ncol = n )%*%x[,initial_g:(initial_g+size-1)]
          
          # compute beta moon new 
          
          if((1-((tau)*lambda*w.j[initial_g])/( sqrt(sum(abs(gamma[initial_g] - U.moon)^2))))>0){
            temp=(1-((tau)*lambda*w.j[initial_g])/( sqrt(sum(abs(gamma[initial_g] - U.moon)^2))))
          }else temp=0
          
          b.moon.new[initial_g:(initial_g+size-1)] = ((gamma[initial_g]*b.moon.old[initial_g:(initial_g+size-1)]-U.moon)*
                                                        temp)/(gamma[initial_g]+lambda*(1-tau))
                        
        }
        # update beta.moon.new
        U.moon.0    = sum(-v.tilde*(y.tilde-b0.moon.old-b.moon.new%*%t(x)))
        b0.moon.new = b0.moon.old-(1/gamma.0)*U.moon.0
        
        check_inner=ConvCheck(c(b0.moon.old,b.moon.old),c(b0.moon.new,b.moon.new),tol)
        
        if( (check_inner==0) | (total.iterations.inner>maxit) ) iterating_inner=FALSE
       
    }
    
    b.tilde.new=b.moon.new
    b0.tilde.new=b0.moon.new
    
    check_outer=ConvCheck(c(b0.tilde.old,b.tilde.old),c(b0.tilde.new,b.tilde.new),tol)
    
    if( (check_outer==0) | (total.iterations.inner>maxit) ) iterating_outer=FALSE
    
  }
  output=list(total.iterations.outer, c(b0.tilde.new,b.tilde.new))
  return(output)
}      
                
#######################################################################################
#Convergence conditions
maxit    =10000
tol      =10^-8

#######################################################################################
#Data manipulation
setwd("/Users/robabairakdar/Documents/Masters/Fall 2016/MATH 680 - Comp Intens Stat - McGill/Final Project")
load("dat_clean.rda")
data=dat

#response variable
y=data[,"CLM_AMT5"]
hist(y,main="Auto Insurance Claim Amounts",xlab="Claim Amount in '000s",breaks=30)

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

#dimensions of variables
n=nrow(x)
p=ncol(x)
grouping=c(seq(1:ncol(x_num)),rep(ncol(x_num)+1,5),rep(ncol(x_num)+2,8),rep(ncol(x_num)+3,4))

########################################################################################
#Data analysis      
set.seed(680)
rho    = runif(1, 1,2)
lambda = runif(1, 1,10)
v.i    = 1/n

b0.tilde  = rnorm(1)
b.tilde   = rnorm(p)
tau           = runif(1,0,1)
      
output_GroupLasso      =GroupLasso(y,x,lambda,rho,maxit,tol,grouping,n,b0.tilde.new,b.tilde.new,x_num,x_cat,v.i,1)
output_GroupElasticNet =GroupLasso(y,x,lambda,rho,maxit,tol,grouping,n,b0.tilde.new,b.tilde.new,x_num,x_cat,v.i,tau)

system.time(GroupLasso(y,x,lambda,rho,maxit,tol,grouping,n,b0.tilde.new,b.tilde.new,x_num,x_cat,v.i,1))
system.time(GroupLasso(y,x,lambda,rho,maxit,tol,grouping,n,b0.tilde.new,b.tilde.new,x_num,x_cat,v.i,tau))

########################################################################################
#Comparing output with a sequence of lambdas
lambda = seq(4,30,1)
GrpLas = matrix(rep(NA), nrow = ncol(x)+1, ncol = length(lambda))
GrpElasNet = matrix(rep(NA), nrow = ncol(x)+1, ncol = length(lambda))


for(l in 1:length(lambda))
{
  GrpLas[,l] =GroupLasso(y,x,lambda[l],rho,maxit,tol,grouping,n,b0.tilde.new,b.tilde.new,x_num,x_cat,v.i,1)[[2]]
  GrpElasNet[,l] =GroupLasso(y,x,lambda[l],rho,maxit,tol,grouping,n,b0.tilde.new,b.tilde.new,x_num,x_cat,v.i,tau)[[2]]
}


rownames(GrpLas) <- c("Intercept", colnames(x))
rownames(GrpElasNet) <- c("Intercept", colnames(x))

Plot1 <- data.frame(t(GrpLas))
Plot1 <- cbind(lambda, Plot1)
Plot1 <- melt(Plot1, id = "lambda", variable_name = "Covariate")

Plot2 <- data.frame(t(GrpElasNet))
Plot2 <- cbind(lambda, Plot2)
Plot2 <- melt(Plot2, id = "lambda", variable_name = "Covariate")

# Plot for Group Penalty
ggplot(Plot1, aes(lambda, value)) + 
  geom_line(aes(colour = Covariate)) +
  scale_colour_manual(values = c('pink','orange','cadetblue','chartreuse','cornflowerblue','coral3',
                                 'red','blue','cyan','darkblue','darkgoldenrod1','darkmagenta',
                                 'darkgreen','darkolivegreen1','darkorchid','gold3','black','gray',
                                 'deeppink3','lightcyan4','indianred4','hotpink2','lightsalmon3','lightslateblue',
                                 'mediumvioletred','midnightblue','mediumorchid2','lightseagreen','lightsalmon4','olivedrab1',
                                 'rosybrown3','seagreen2','sienna1','yellow2','tan','olivedrab4'))+
  ggtitle("A Plot of Coefficients vs. Lambda for Auto Insurance Claims Data \n Group Penalty") + 
  xlab("Lambda") + 
  ylab("Coefficient Value") + 
  theme(legend.position = "right")

# Plot for Group Elastic Net Penalty
ggplot(Plot2, aes(lambda, value)) + 
  geom_line(aes(colour = Covariate)) +
  scale_colour_manual(values = c('pink','orange','cadetblue','chartreuse','cornflowerblue','coral3',
                                 'red','blue','cyan','darkblue','darkgoldenrod1','darkmagenta',
                                 'darkgreen','darkolivegreen1','darkorchid','gold3','black','gray',
                                 'deeppink3','lightcyan4','indianred4','hotpink2','lightsalmon3','lightslateblue',
                                 'mediumvioletred','midnightblue','mediumorchid2','lightseagreen','lightsalmon4','olivedrab1',
                                 'rosybrown3','seagreen2','sienna1','yellow2','tan','olivedrab4'))+
  ggtitle("A Plot of Coefficients vs. Lambda for Auto Insurance Claims Data \n Group Elastic Net Penalty") + 
  xlab("Lambda") + 
  ylab("Coefficient Value") + 
  theme(legend.position = "right")
