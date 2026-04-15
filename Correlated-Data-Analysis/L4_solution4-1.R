## Exercise 4.1, eNote-4.

sex <- factor(c(rep("female", 4), rep("male", 4)))
tmt <- factor(c(0, 0, 1, 1, 0, 0, 1, 1))
y <- c(-9.27, -1.28, 3.98, 7.06, 1.02, -1.79, 3.64, 1.94)

(df<- data.frame(y=y,sex=sex, tmt=tmt))
#Let alpha1=0 if female, else 0. beta1=1 if treatment 0 else 0.
#    alpha2=0 if male, else 0. beta2=1 if treatment 1 else 0.
# y    sex tmt
# 1 -9.27 female   0  #Obs 1
# 2 -1.28 female   0. #Obs 2
# 3  3.98 female   1
# 4  7.06 female   1
# 5  1.02   male   0
# 6 -1.79   male   0
# 7  3.64   male   1
# 8  1.94   male   1. #Obs 8

X <- matrix(data = 0, nrow = 8, ncol = 9)
colnames(X)<-c("mu", "alpha1", "alpha2", "beta1", "beta2", "alpa1:beta1", "alpha1:beta2", "alpha2:beta1", "alpha2:beta2")
X[1,]<-c(1,1,0,1,0,1,0,0,0)  #Obs 1
X[2,]<-c(1,1,0,1,0,1,0,0,0)  #Obs 1
X[3,]<-c(1,1,0,0,1,0,1,0,0)
X[4,]<-c(1,1,0,0,1,0,1,0,0)
X[5,]<-c(1,0,1,1,0,0,0,1,0)
X[6,]<-c(1,0,1,1,0,0,0,1,0)
X[7,]<-c(1,0,1,0,1,0,0,0,1)  #Obs 8
X

ftable(sex,tmt) # There are 2 observations per combination of sex and tmt

qr(X)$rank ## rank 4
Matrix::rankMatrix(X) ### rank 4
# The rank of X is 4. There are four lienearly independent columns in the design matrix X.
# Only four parameters can be estimated.

# The rank of the design matrix is 4, the four parameters will be distributed as:
# For the overall mean:    1
# For sex(female/male):    1
# For tmt(0,1):            1
# There is only one parameter left for the interaction sex with tmt.

options("contrasts")
?contrasts()

options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
summary(m1<-lm(y ~ sex*tmt))

options(contrasts = c(unordered = "contr.SAS", ordered = "contr.poly"))
summary(m1B<-lm(y ~ sex*tmt))

options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly")) # back to default

summary(m1); summary(m1B) #two different parametrizations of the same model.
# In model m1: (Intercept)    -5.275, i.e. the average response value for 
# a female receiving treatment 0 (reference categories)

## In model m1B: (Intercept)       2.790, i.e. the average response value for 
# a male receiving treatment 1 (reference categories).
# 
anova(m1); anova(m1B) #same model different parametrizations
drop1(m1,test = "F"); drop1(m1B, test="F") #same model different parametrizations

#To see all average values of the response for four different combinations of 
#sex and treatment:
  
emmeans::emmeans(m1, "sex", by = c("tmt")) #Estimated values
# Alternalivelly:
emmeans::emmeans(m1B, "sex", by = c("tmt")) ##Estimated values

#----end-----
