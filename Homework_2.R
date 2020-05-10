library(ggplot2)
library(tibble)
library(evd)

#Question 1)
"Simulate 2 samples of size n of independent random variables,
normal and exponential distributions, respectively, having the
same mean and variance."

n=1000
mu=1
sd=1
sample_normal=rnorm(n,mu,sd)
# For exponential, mean = 1/lambda and variance = 1/(lambda^2)
# In  the function rexp the parameter lambda is called rate.
lambda=1
sample_exp=rexp(n,rate=lambda)
hist(sample_normal)
hist(sample_exp)

#Question 2)
"Simulate the probability density function of the exponential
sample for n = 5, 50, 500, 50000, 100000. What do you observe
for the empirical mean?"


n_vec=c(5,50,500,50000,100000)
for (n in n_vec){
  print("for n = ")
  print(n)
  sample_exp=rexp(n, rate = lambda)
  density_exp=density(sample_exp)
  print("the mean is : ")
  print(mean(sample_exp))
  plot(density_exp, main="density for an exponential sample")
}

#For n increasing , the density is getting closer to the real density of the exponantial distribution.
#Furthermore, for n increasing, the empirical mean is getting closer to the true mean, this is an application
# of the Law of Large Numbers.


#Question 3)
"Focus on the maxima: what do you observe when taking the
case not only of the exponential, but also the normal case,
representing them on the same plot. Add also on this plot the
Gumbel distribution and compare the 3 distribution tails."
n=100000
sample_normal = rnorm(n,mu,sd)
sample_exp = rexp(n, rate = lambda)
sample_gumbel = rgumbel(n, loc=1, scale=1) #mean= loc, sigma=scale
density_normal = density(sample_normal)
density_exp = density(sample_exp)
density_gumbel = density(sample_gumbel)

normal_data = tibble(density_normal$x, density_normal$y)
eponential_data = tibble(density_exp$x, density_exp$y)
gumbel_data= tibble(density_gumbel$x, density_gumbel$y)

ggplot(normal_data, aes(x = density_normal$x, y = density_normal$y, color = 'Normal')) + 
  geom_line() +
  geom_line(aes(x = density_exp$x, y = density_exp$y, color = 'exponential'), data = eponential_data) + 
  geom_line() +
  geom_line(aes(x = density_gumbel$x, y = density_gumbel$y, color = 'Gumbel'), data = gumbel_data) + 
  labs(x = "X", y = "Frequency") + 
  scale_color_manual(name="Legend", values = c("black","red","green")) + 
  theme(legend.position = c(0.985, 0.985), legend.justification = c("right", "top"))

#Exponential has very small tail, normal has smaller tail, and gumbel has a heavier tail. 

number_iteration=10000
n=10000
max_normal=c()
max_exp=c()
max_gumbel=c()
for (i in 1:number_iteration){
  sample_normal = rnorm(n,mu,sd)
  sample_exp = rexp(n, rate = lambda)
  sample_gumbel = rgumbel(n, loc=1, scale=1)
  max_normal[i]=max(sample_normal)
  max_exp[i]=max(sample_exp)
  max_gumbel[i]=max(sample_gumbel)
}

density_normal = density(max_normal)
density_exp = density(max_exp)
density_gumbel = density(max_gumbel)

normal_data = tibble(density_normal$x, density_normal$y)
eponential_data = tibble(density_exp$x, density_exp$y)
gumbel_data= tibble(density_gumbel$x, density_gumbel$y)

ggplot(normal_data, aes(x = density_normal$x, y = density_normal$y, color = 'Normal')) + 
  geom_line() +
  geom_line(aes(x = density_exp$x, y = density_exp$y, color = 'exponential'), data = eponential_data) + 
  geom_line() +
  geom_line(aes(x = density_gumbel$x, y = density_gumbel$y, color = 'Gumbel'), data = gumbel_data) + 
  labs(x = "X", y = "Frequency") + 
  scale_color_manual(name="Legend", values = c("black","red","green")) + 
  theme(legend.position = c(0.985, 0.985), legend.justification = c("right", "top"))


