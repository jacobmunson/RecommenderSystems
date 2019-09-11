### Likelihood Ratio Test
# This script serves as a personal refresher for LRT and related methods
# 9/9/2019

## Example 1.
# Through the lens of hypothesis testing:
# Ho: mu = mu0
# Ha: mu != mu0
# and assume X~N(mu, sigma^2 = 1)
# In common statistical LRT parlance: Assuming unknown mean and known variance

set.seed(1)
mu_0 = 0
n = 100
x = rnorm(n = n, mean = mu_0, sd = 1)
mu_a = mean(x)
mu_a # 0.02967

# LLR MLE
mu = seq(-1, 1, by = 0.0001)
llk = c()
for(i in 1:length(mu)){llk[i] = (-n/2)*log(2*pi) - 0.5*sum((x - mu[i])^2)}
plot(llk)
llk.max = max(llk) # - 144.8443

# LLR Null Value
llk.null = (-n/2)*log(2*pi) - 0.5*sum((x - mu_0)^2)
llk.null # - 144.8883

# X2_LRT = -2*[L(mu_0) - L(mu)]
X2.lrt = -2*(llk.null - llk.max)
X2.lrt # 0.0880518

# Getting a p-value
df.lrt = 1 
pval.lrt = pchisq(q = X2.lrt, df = df.lrt, lower.tail = FALSE)
pval.lrt # 0.7666

# Standard conclusion: p !< 0.05 => Fail to reject Ho
# There's evidence to believe that's mu = mu0


