# !#/bin/R
# Program: Calculates LIML regression with applied Bekker variances.
# Author: Nicholas Potter
# Date: 11/15/2011
#
# Using MROZ data set should give the following results:
# Var	Coef	 	Std Err		Bekker
# hours	.00178946 	.00114792	.00203246
# exper	-.092979	.145624		.254861
###########
library("foreign")
library("corpcor")
library("Matrix")
data_raw = read.dta("/Users/potterzot/data/test/mroz.dta")
data_complete = data_raw[complete.cases(data_raw),]
data = subset(data_complete, age < 32)
N = nrow(data)

Y = as.matrix(cbind(data$lwage, data$hours))
M = ncol(Y)
#Instrumented (endogenous variable)
X1 = matrix(Y[,2:M], N,M-1)
#Dependent variable (lhs)
y = matrix(Y[,1])
#Instruments (Z1: excluded exogenous variables)
Z1 = as.matrix(cbind(data$educ, data$kidslt6, data$kidsge6, data$age, data$nwifeinc))
#Exogenous (X2 = Z2: included exogenous variables)
#X2 = matrix(cbind(1,data$exper),N,2)
X2 = matrix(data$exper)
Z2 = X2

# Define our dimensions
K1 = ncol(X1)
K2 = ncol(X2)
K = K1 + K2
L1 = ncol(Z1)
L2 = ncol(Z2)
L = L1 + L2

# Weight vector
# weights = matrix(data$f_wt_totsvy)
weights = matrix(rep(1,N))
wt = diag(N)
wt = as.matrix(diag(N))

#
I = diag(N)


df.total = N
df.ess = ncol(Y)+ncol(X2)-1
df.rss = df.total - df.ess

A = matrix(c(y,X1,X2,Z1),N,K+L1+1)
X = matrix(c(X1,X2),N,K)
Z = matrix(c(Z1,Z2),N,L)

AA = t(A) %*% wt %*% A
M = nrow(AA)

# Now define the commonly used matrices.
Xy = as.matrix(AA[2:(K+1),1])
XX = as.matrix(AA[2:(K+1), 2:(K+1)])
if (K1 > 0) {
    X1X1 = as.matrix(AA[2:(K1+1), 2:(K1+1)])
    }
if (L1 > 0) {
    Z1Z1 = as.matrix(AA[(K+2):M,(K+2):M])
    }
if (L2 > 0) {
    Z2Z2 = as.matrix(AA[(K1+2):(K+1), (K1+2):(K+1)])
    X2y = as.matrix(AA[(K1+2):(K+1), 1])
    }

# Get ZZ
if ((L1 > 0) && (L2 > 0)) {
    Z2Z1 = as.matrix(AA[(K1+2):(K+1),(K+2):M])
    ZZ2 = matrix(c(Z2Z1, Z2Z2),L,L2)
    ZZ1 = matrix(c(Z1Z1, Z2Z1),L1,L)
	ZZ = matrix(rbind(ZZ1, t(ZZ2)),L,L)
} else if (L1>0) {
	ZZ = Z1Z1
} else {
	ZZ = Z2Z2
}

if (K1>0) {
	X1Z1 = as.matrix(AA[(2:(K1+1)), ((K+2):M)])
	}

if ((K1>0) && (L2>0)) {
	X1Z2 = as.matrix(AA[2:(K1+1), (K1+2):(K+1)])
	X1Z = matrix(c(X1Z1, X1Z2),1,L)
	XZ = matrix(rbind(X1Z, t(ZZ2)),2,L)
} else if (K1>0) { 
	XZ = X1Z1
	X1Z= X1Z1
} else if (L1>0) {
	XZ = matrix(AA[2:(K+1),(K+2):M], AA[(2:K+1),(2:(K+1))])
} else {
	XZ = ZZ
}

if ((L1>0) & (L2>0)) {
	Zy = matrix(c(AA[(K+2):M, 1],AA[(K1+2):(K+1)]),L,1)
	ZY = matrix(rbind(AA[(K+2):M, 1:(K1+1)], AA[(K1+2):(K+1), 1:(K1+1)]), L, L2+1)
	Z2Y = matrix(c(AA[(K1+2):(K+1), 1:(K1+1)]),L2,K1+1)
} else if (L1>0) {
	Zy = AA[(K+2):M, 1]
	ZY = AA[(K+2):M, 1:(K1+1)]
} else {
	Zy = AA[(K1+2):(K+1), 1]
	ZY = AA[(K1+2):(K+1), 1:(K1+1)]
	Z2Y = ZY
}

YY  = AA[1:(K1+1), 1:(K1+1)]
yy  = AA[1,1]
ym    = sum(wt %*% y)/N
yyc   = t(y %*% t(ym)) %*% wt %*% y %*% ym

YYinv	= pseudoinverse(YY)
XXinv   = pseudoinverse(XX)
ZZinv   = pseudoinverse(ZZ)
PZ      = Z %*% ZZinv %*% t(Z)
MZ      = I-PZ
XPZX    = XZ %*% ZZinv %*% t(XZ)
XPZy	= t(X) %*% PZ %*% y
XPZXinv = pseudoinverse(XPZX)
YPZY	= t(Y) %*% PZ %*% Y

if (ncol(X)==ncol(Z)) {
	if (X==Z) {
		ZZinv = XXinv
		XPZXinv = XXinv
	} else {
		ZZinv = pseudoinverse(ZZ)
		XPZX  = XZ %*% ZZinv %*% t(XZ)
		XPZXinv=pseudoinverse(XPZX)
	}
}

QZZ = ZZ/N
QXX = XX/N
QXZ = XZ/N
QZy = Zy/N
QZ2Z2 = Z2Z2/N
QYY = YY/N
QZY = ZY/N
QZ2Y = Z2Y/N
QXy = Xy/N
QXPZX = XPZX/N
QZZinv = ZZinv*N


QWW = QYY - t(QZY) %*% QZZinv %*% QZY
WW = (YY - t(ZY) %*% ZZinv %*% ZY)
Omega = WW/(N-K-L)

if (K2>0) {
	Z2Z2inv = pseudoinverse(Z2Z2)
	QZ2Z2inv = pseudoinverse(QZ2Z2)
	QWW1 = QYY - t(QZ2Y) %*% QZ2Z2inv %*% QZ2Y
	YPY = YY - (t(Z2Y) %*% Z2Z2inv %*% Z2Y) - WW
} else {
	QWW1 = QYY
	YPY = YY - WW
}
# equivalent to YMY/N in kolesar
QWWsym = as.matrix(forceSymmetric(QWW))
# equivalent to YY/N in kolesar
QWW1sym = as.matrix(forceSymmetric(QWW1))

QWWsqrt = mpower(QWWsym, -0.5)
AA2 = QWWsqrt %*% QWW1 %*% QWWsqrt

eigenvalues = eigen(AA2, only.values=TRUE)
lambda = min(Re(eigenvalues$val))

# for now just set C = 2
C = 2
# set model_type
model_type = "liml"
if (model_type=="fuller") {
	kclass = (lambda-lambda*C/N)/(1-lambda*C/N)
} else if (model_type=="liml") {
	kclass = lambda
} else if (model_type=="tsls") {
	kclass = 0
}

XMZX    = t(X) %*% (I-kclass*MZ) %*% X 
XMZXinv = pseudoinverse(XMZX)



QXhXh = (1-kclass) * QXX + kclass * QXPZX
QXhXhinv = pseudoinverse(QXhXh)
#newey :: 1/(1-kclass2) = kclass from ivreg2
Y2 = matrix(c(y,X),N,K+1)
Y2Y2 = t(Y2) %*% Y2
Y2Y2inv = pseudoinverse(Y2Y2)
Y2PZY2 = t(Y2) %*% PZ %*% Y2
AAA = Y2Y2inv %*% Y2PZY2
eigenvalues2 = eigen(AAA)
kclass2 = min(Re(eigenvalues2$val))
X.beta2 = pseudoinverse(XPZX- kclass2*XX) %*% (XPZy - kclass2*Xy)
X.beta1 = t(t(X) %*% (I-kclass*MZ) %*% y) %*% XMZXinv
X.beta = t(QXy) %*% QXhXhinv * (1-kclass) + kclass * t(QZy) %*% QZZinv %*% t(QXZ) %*% QXhXhinv
Z.beta = ZZinv %*% t(XZ)
e = y - X %*% t(X.beta)
rss = t(e) %*% wt %*% e
rss.ms = rss/df.rss
rss.es = rss - rss.ms
rss.V = rss/(N-df.ess)
s2 = rss/(N-df.ess)

# LIML Variance
X.varliml = s2[1,1] * XMZXinv
# TSLS Variance
X.vartsls = s2[1,1] * XPZXinv

Z.var = sigma2[1,1] * ZZinv
X.seliml = sqrt(X.varliml)
X.setsls = sqrt(X.vartsls)

# Bekker coefficients
ak = K/N
al = L/N
h1 = ak/(1-ak)
h2 = ak*(1-al)/(1-ak-al)

# WORKS TO THIS POINT
PZ2 = t(Z1) %*% pseudoinverse(Z1 %*% t(Z1)) %*% Z1
PW = t(Z2) %*% pseudoinverse(Z2 %*% t(Z2)) %*% Z2

#QWW = t(Y) %*% (I - PZ - PW) %*% Y

Gamma = matrix(c(1, -X.beta[1,1], 0, 1), 2,2)
Sigma = t(Gamma) %*% Omega %*% Gamma
Lambda = t(Gamma) %*% (YPY/N-ak*Omega) %*% Gamma
X.varbekker = Sigma[1,1] / Lambda[2,2] + h2 * det(Sigma)/(Lambda[2,2]**2)
X.sebekker = sqrt(X.varbekker)

# equivalent to e above
u = y - X %*% X.beta2
# this alpha is equivalent to kclass2 above
alpha = t(e) %*% PZ %*% e / rss[1]
a2 = alpha * alpha
PZX = PZ %*% X
Xbar = X - e %*% (t(e) %*% X) / rss[1]
PZXbar = PZ %*% Xbar
MZXbar = (I-PZ) %*% Xbar

G = K
K = 6
Kn = sum(diag(PZ)**2)/K
Tn = K/N
s2 = rss/(N-G)
S2 = matrix(c(s2),N,1)
H = XPZX - alpha[1] * XX
Hinv = pseudoinverse(H)
HA = sum(diag(PZ)-Tn) * t(PZX) %*% (rss[1] * MZXbar/N)
HB = K * (Kn[1] - Tn) * sum(e*e-S2) * t(MZXbar) %*% MZXbar / (N*(1 - 2*Tn + Kn[1]*Tn))
SigmaB = s2[1] * (((1 - alpha[1])**2)*t(Xbar)%*%PZXbar + a2[1]*t(Xbar)%*%MZXbar) 
# according to Newey, should also add HB but it only matches TSP if HB is not included 
Sigma3 = SigmaB + HA + t(HA) + HB
Sigma2 = SigmaB + HA + t(HA)

X.varnewey = Hinv %*% Sigma2 %*% Hinv
X.senewey = sqrt(X.varnewey)

X.varnewey3 = Hinv %*% Sigma3 %*% Hinv
X.senewey3 = sqrt(X.varnewey3)

