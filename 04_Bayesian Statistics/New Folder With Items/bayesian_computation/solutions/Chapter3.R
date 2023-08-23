# Exercise 2
#a) P(\theta<=x)=P(1/\lambda <=x)=P(\lambda>=1/x)=
#   int_{1/x}^{\infty} \lambda^{-n-1}}exp(-s/\lambda) d\lambda=
#   variable change \lambda=1/\theta
#   int_{0}^{x}\theta^{n+1}exp(-s\theta)1/theta^2 d\theta=
#   int_{0}^{x}\theta^{n-1}exp(-s\theta) d\theta - gamma density with shape n and rate s.

#b) 
obs=c(751,594,1213,1126,819)
post_sim_theta=rgamma(1000, shape = length(obs),rate = sum(obs))
#c)
post_sim_lambda=1/post_sim_theta
#d)
p=sum(post_sim_lambda>1000)/1000

# Exercise 3
#a)
B=200
y=c(43,24,100,35,85)
ymax=max(y)
n=length(y)
post=(1:B)^(-n)
if (ymax>1)
{
	post[1:ymax-1]=0
}
post=post/sum(post)
#b)
m=sum((1:B)*post)                    #mean
s=sqrt(sum((1:B)^2*post)-m^2)        #standard deviation
#c)
p=sum(post[151:B])

# Exercise 4
#a)
sim1=rbeta(1000,100,100)
p=seq(0,1,length=1000)
prior2=0.9*dbeta(p,500,500)+0.1*dbeta(p,1,1)
sim2=sample(p, size = 1000, replace = TRUE, prob = prior)
summary1=c(mean(sim1),sum((0.44<sim1)&(sim1<.56))/1000)
summary2=c(mean(sim2),sum((0.44<sim2)&(sim2<.56))/1000)
rbind(summary1,summary2)
#b)
post1=dbeta(p,100+45,100+55)
post1=post1/sum(post1)
post2=dbeta(p,45+1,55+1)*prior2
post2=post2/sum(post2)
simpost1=sample(p, size = 1000, replace = TRUE, prob = post1)
simpost2=sample(p, size = 1000, replace = TRUE, prob = post2)
CI1=quantile(simpost1,c(0.05,0.95))
CI2=quantile(simpost2,c(0.05,0.95))
rbind(CI1,CI2)
# Alternatively, you can use discint(dist,prob):
#CI1=discint(cbind(p,post1),.9)
#CI2=discint(cbind(p,post2),.9)
#rbind(CI1,CI2)
#c)
post1=dbeta(p,100+30,100+70)
post1=post1/sum(post1)
post2=dbeta(p,30+1,70+1)*prior2
post2=post2/sum(post2)
simpost1=sample(p, size = 1000, replace = TRUE, prob = post1)
simpost2=sample(p, size = 1000, replace = TRUE, prob = post2)
CI1=quantile(simpost1,c(0.05,0.95))
CI2=quantile(simpost2,c(0.05,0.95))
rbind(CI1,CI2)
#d)          45 heads               30 heads
#           5%       95%           5%       95%
#CI1 0.4374374 0.5325325    0.3843844 0.4775275
#CI2 0.4684685 0.5215716    0.2362362 0.4245746
#
# in the first case the inference is robust to the choice of prior,
# in the second - not (the interval that corresponds to prior 2 is shifted
# to the left, compared to the interval for the prior 1).



