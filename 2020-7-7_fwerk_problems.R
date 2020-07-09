
##### Set Up Observed and Bootstrapped Stats #####
my_tstat = structure(c(5.29643402646158, 2.44339631321577), .Dim = 2:1)

my_bstat = structure(c(-2.62538356059634, -0.998015836683857, -1.23216216140277, 
                     -0.928223719072409, -0.17173300266917, 0.0730022570605579, -0.523068993747715, 
                     -0.644930730004201, 0.321862934866585, 0.501323148771146, -2.49261246745561, 
                     -0.352468496859048, 0.480333935235628, -0.160018779114225, 0.0492266040535515, 
                     0.639922677140874, 1.16679377447727, 0.477478403453262, -1.53016208038827, 
                     2.08008808489326, -0.175506850839836, 1.21866691902372, -1.06230313457949, 
                     -2.33413502502094, 1.08249864936573, 0.518898131059902, -0.530305418712476, 
                     1.25088728670225, -0.353562886125801, -0.146543384495462, -1.35658439289897, 
                     -1.54752124583265, 1.85388814485448, 0.572281376439199, 1.80076155624165, 
                     0.255289811703204, 0.468401280593835, 0.0856423601856926, 1.32623876884518, 
                     -1.35685698381936, 1.36961414341979, -0.385822434792065, 0.00905846241009908, 
                     0.353448654898236, 0.470290866497102, -0.0328555646616792, 0.558367269407892, 
                     -2.08112568432747, -0.374021367874549, -1.85273076188956, 0.876092063036564, 
                     0.215745591044136, 0.0625192699245789, -1.5540111212382, -0.0206554452831147, 
                     1.29739111912877, 0.382070566474067, 0.12713501648462, 1.61424396887645, 
                     -0.41193074970025, 1.44344267345991, -0.868024413111194, 0.511408876696691, 
                     0.475360444972226, 0.140984971585348, -1.01443266875733, 3.42402133207041, 
                     1.49285772923265, -0.342620233818604, 0.0602912987730182, 1.27909026319408, 
                     -0.669437962372414, -0.0998886591196407, -0.511190657855458, 
                     1.90049768649951, 0.457351419552682, -0.255168180089023, -2.89361388651408, 
                     2.20183808566618, 0.437876037732558, 0.786561441005918, 0.42079678703038, 
                     0.344379610765469, 2.23934213671945, -1.06997603553258, -0.0961967837648953, 
                     0.724051954633872, 1.38023808722179, 0.245065482470204, 1.65557010456055, 
                     -0.141010506127495, 0.0506350853996294, -0.119684369237794, -0.216954481623603, 
                     -0.271549961470695, 0.933757466399921, -0.733597404238393, 0.963285023205711, 
                     0.356425627683335, -3.72409660361582), .Dim = c(2L, 50L), .Dimnames = list(
                       NULL, c("result.1", "result.2", "result.3", "result.4", "result.5", 
                               "result.6", "result.7", "result.8", "result.9", "result.10", 
                               "result.11", "result.12", "result.13", "result.14", "result.15", 
                               "result.16", "result.17", "result.18", "result.19", "result.20", 
                               "result.21", "result.22", "result.23", "result.24", "result.25", 
                               "result.26", "result.27", "result.28", "result.29", "result.30", 
                               "result.31", "result.32", "result.33", "result.34", "result.35", 
                               "result.36", "result.37", "result.38", "result.39", "result.40", 
                               "result.41", "result.42", "result.43", "result.44", "result.45", 
                               "result.46", "result.47", "result.48", "result.49", "result.50"
                       )))


##### This Breaks #####
FWERkControl(my_tstat, my_bstat, k=1, alpha=0.05)




##### Compare Arguments to the Working Example in Docs #####

# verbatim from ?FWERkControl:
# Specify the model parameters
m_null = 3
m_alt  = 7
m = m_null + m_alt
mu = c( rep(0, m_null), rep(0.5,m_alt) )
rho = 0.25
omega= (1-rho)*diag(1,m) + rho*matrix(1,m,m)
v=t(chol(omega))

# generate the data
n = 100
y = mu%*%matrix(1,1,n)+ v %*% matrix(rnorm(m*n),m,n)

# calculate the test statistics and bootstrap statistics
library(foreach)
library(tseries)
B = 100
y_mean = apply(y,1,mean)
y_sig = apply(y,1,sd)
t_stat = as.matrix(sqrt(n)*y_mean/y_sig)
s = tsbootstrap(1:n,B,b=2,type="stationary")
b_stat = foreach(i=1:B,.combine=cbind) %do% {
  y_boot = y[, s[,i]]
  y_mean_boot = apply(y_boot,1,mean)
  sqrt(n)*(y_mean_boot - y_mean)/y_sig
}

# compare
str(my_tstat); str(t_stat)
str(my_bstat); str(b_stat)


# email from Kendro:
# This is the case when the steps are running out of hypotheses to be rejected (all of them are non-null). I didn't add a stopping rule for this scenario, which should prevent such error.

