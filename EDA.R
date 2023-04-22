# Import data
df <- read.table("data.csv",sep=';',header = TRUE)
dim(df)
colnames(df)

# Select columns
df <- df[,c('Life.expectancy','Alcohol','BMI','GDP','Total.expenditure','Schooling','Status')]
df <- na.omit(df)
dim(df)
attach(df)

# Creating the regression model
model <- lm(Life.expectancy~Alcohol+BMI+GDP+Total.expenditure+Schooling)
betaHat <- model$coeff
summary(model) # Total.expenditure is not significant 

# New model
model <- lm(Life.expectancy~Alcohol+BMI+GDP+Schooling)
betaHat <- model$coeff
summary(model)
sigma2 <- (summary(model)$sigma)^2
sigma2 # 36.03475

# Transform regressors
mean(BMI)  # 38.08735
BMI <- BMI - mean(BMI)
mean(GDP)  # 7579.039
GDP <- GDP - mean(GDP)
mean(Schooling) # 12.0922
Schooling <- Schooling - mean(Schooling)

# update model for better interpretation of the coefficients
model <- lm(Life.expectancy~Alcohol+BMI+GDP+Schooling)
betaHat <- model$coeff
summary(model)
sigma2 <- (summary(model)$sigma)^2
sigma2 # 36.03475
betaHat

# R squared and adjusted R squared
n <- 2308
k <- 5
X <- cbind(rep(1,n),Alcohol,BMI,GDP,Schooling)
epsilonHAT <- Life.expectancy - X%*%betaHat
RSS <- sum(epsilonHAT^2)
TSS <- sum((Life.expectancy - mean(Life.expectancy))^2)

R2 <- 1- (RSS/TSS)
R2 #0.6176763

R2Adj <- 1 - ((RSS/(n-k))/(TSS/(n-1)))
R2Adj #0.6170122

# Testing the significance of the regression
R <- diag(k)[-1,]
q <- 4
r <- c(0,0,0,0)
s2 <- RSS/(n-k)

inv <- solve(t(X)%*%X)
m <- solve(R %*% inv %*% t(R))
m2 <- R%*%betaHat - r
F <- (t(m2)%*%m%*%m2)/(s2*q)
F # 930.1728
qf(0.95,q,n-k) # 2.375792

# Chow breakpoint test

# split data
detach(df)
df_dp <- df[df$Status=='Developed',]
dim(df_dp)
n1 <- 420
df_dg <- df[df$Status=='Developing',]
dim(df_dg)
n2 <- 1888

# vectors and matrices
y <- c(df_dp$Life.expectancy,df_dg$Life.expectancy)
length(y)

X1 <- cbind(rep(1,n1),df_dp$Alcohol,df_dp$BMI,df_dp$GDP,df_dp$Schooling)
X2 <- cbind(rep(1,n2),df_dg$Alcohol,df_dg$BMI,df_dg$GDP,df_dg$Schooling)

X <- rbind(cbind(X1,matrix(0, n1, k)),cbind(matrix(0, n2, k),X2))
dim(X)

# calculate betaHat using OLS
betaHat <- solve(t(X)%*%X)%*%t(X)%*%y
betaHat

# calculations for f test
R <- cbind(diag(k),-diag(k))
q <- 5
r <- rep(0, k)
epsilonHAT <- y - X%*%betaHat
RSS <- sum((epsilonHAT)^2)
s2 <- RSS/(n-(2*k))

# f test
inv <- solve(t(X)%*%X)
m <- solve(R %*% inv %*% t(R))
m2 <- R%*%betaHat - r
F <- (t(m2)%*%m%*%m2)/(s2*q)
F # 45.46596
qf(0.95,q,n-(2*k)) # 2.3758
