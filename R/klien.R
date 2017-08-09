# !#/bin/R
# Program: Calculates LIML regression with applied Bekker variances.
# Author: Nicholas Potter
# Date: 11/15/2011
#
# Using KLEIN data set (from http://econ413.wustl.edu/benchmarks/klein.xls,
# should give the following results:
# Var	Coef	Std Err	Bekker
# C	17.1477	2.04537	2.07396
# P	-.22251	.224230	.235078
# P(-1)	.396027	.192943	.202107
# W	.822559	.061549	.061756
###########
require(foreign)
data_raw = read.csv("/Users/potterzot/data/test/klein.csv")
data = data_raw[complete.cases(data_raw),]

Y = as.matrix(cbind(data$CX, data$C))
#Instrumented
X = matrix(Y[,2])
#Dependent variable
y = matrix(Y[,1])
#Instruments
Z = as.matrix(cbind(data$TM, data$W2, data$G, data$TX)) 
#Exogenous non-instrumented variables
W = as.matrix(cbind(1, data$P, data$W))

# Define our dimensions
N = nrow(X)
K = ncol(Z)
L = ncol(W)

# Weight vector
# weights = as.matrix(cbind(data$f_wt_totsvy))
weights = as.matrix(W[,1])
wt = diag(N) %*% weights
wt = as.matrix(diag(N))

# 
I = diag(N)

Pw = W %*% solve(t(W) %*% wt %*% W) %*% t(W) %*% wt
Mw = I - Pw
# Regression of Y on W
Yp = Mw %*% Y
# Regression of Z on W
Zp = Mw %*% Z
# Regression of X on W
Xp = Mw %*% X
# Regression on Z_hat (by Yp in YMY)
Pz_bar = Zp %*% solve(t(Zp) %*% wt %*% Zp) %*% t(Zp) %*% wt
Mzp = I - Pz_bar
YY = t(Yp) %*% wt %*% Yp
YMY = t(Yp) %*% wt %*% Mzp %*% Yp
YPY = YY - YMY

# Omega from mroz with no constant should be [0, -42; -42, 569512]
Omega = YMY/(N-K-L)

A = YY / YMY
eigenvalues = eigen(A)
kliml = min(eigenvalues$val)
err = solve(t(Xp) %*% (I - kliml * Mzp) %*% Xp)
B0 = err %*% (t(Xp) %*% (I - kliml * Mzp) %*% Yp)
B1 = (YY[1,2] - kliml*YMY[1,2]) / (YY[2,2] - kliml*YMY[2,2])

# Bekker coefficients
ak = K/N
al = L/N
h1 = ak/(1-ak)
h2 = ak*(1-al)/(1-ak-al)

G = matrix(c(1, B1, 0, 1), 2,2) 
Sigma = t(G) %*% solve(Omega) %*% G 

#Lambda. mroz data with no constant should give us [0,-10.7; -10.7, 48216)
Lambda = t(G) %*% ((YPY/N)-ak*Omega) %*% G
Lambda2 = (t(Xp) %*% Pz_bar %*% Xp)/N - ak*Omega[2,2]
# Error variance - May or may not correctly account for exogenous variables
s2 = t(Yp[,1]-Xp*B1) %*% (Yp[,1]-Xp*B1)/(N-L)
J = N*(crossprod(Omega[1,2]) - Omega[2,2]*B1*Omega[1,2] - t(Omega[1,2])*t(B1)*Omega[2,2] + Omega[2,2]*B1*t(B1)*Omega[2,2])/(Omega[1,1] - 2*Omega[1,2]*B1 + t(B1)*Omega[2,2]*B1)
v_std = s2 * err
v_michal = Sigma[1,1]/Lambda[2,2] + h1*(det(Sigma)/Lambda[2,2]^2)
#v_imbens = v_std*(1+h1*(solve()*solve(Sigma)))
v_imbens2 = s2*crossprod(err,(t(Xp) %*% Pz_bar %*% Xp-(kliml-1) %*% J)) %*% err
v_robust = v_std 

se_std = sqrt(v_std)
se_michal = sqrt(v_michal)
se_imbens = sqrt(v_imbens)
se_robust = sqrt(v_robust)




