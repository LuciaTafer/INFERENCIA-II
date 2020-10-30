#Inf II: clase 2/10 
## gracias a Martín Olivera
#
# comentarios agregados
#
# grilla de valores para \theta
theta<-seq(0,1,0.001)
previa<-dnorm(theta, mean=0.5, sd=0.2)

  pnorm(0, mean=0.5, sd=0.2) # queda muy poca probabilidad 
1-pnorm(1, mean=0.5, sd=0.2) # fuera

sum_y<-6
n<-9
vero<-(theta^sum_y) * (1-theta)^(n-sum_y)
post<-previa*vero
a<-sum(post) #No suma 1
post<-post/a #Corrijo para que sume 1
b<-print(sum(post))
Post<-cumsum(post) #Dist acumulada
c<-print(which(Post>=0.603))
min(which(Post>=0.603)) 
theta[min(which(Post>=0.603))]
theta[min(which(Post>=0.136))]
###

# simulo por Monte Carlo S extracciones de post
S <- 100e3
# como lo hicimos en clase del 2 de octubre
thetaSimulado <- rep(0,S)
for (i in 1:S) thetaSimulado[i] <- theta[min(which(Post>=runif(1)))]

# usando sample() de R
thetaSample <- sample(theta,size = S, replace = TRUE, prob = post)

par(mfrow=c(3,1))
## dibuja línea que une las 1001 barritas
plot(theta,post, type='l', main = "Post con previa N(0.5,0.2") 
hist(thetaSimulado, breaks = 100, xlim = c(0,1))
hist(thetaSample,   breaks = 100, xlim = c(0,1))
