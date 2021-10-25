# Examples adapted from: https://github.com/rmcelreath/causal_salad_2021/blob/main/1_causal_salad.r

require(rethinking)
require(tidyverse)
theme_set(theme_classic())
setwd("~/Documents/Analytics/R/Causal Modelling Presentation")

# d-separation plots
a <- 0.7
cols <- c( col.alpha(1,a) , col.alpha(2,a) )

# pipe

N <- 1000
X <- rnorm(N)
Z <- rnorm(N, X)
Z_bn <- as.numeric(cut_number(Z,5))
# Z <- rbern(N,inv_logit(X*10))
Y <- rnorm(N, Z)
dat1 <- tibble(X, Z, Z_bn = factor(Z_bn), Y)


(p1 <- dat1 %>%
  ggplot(aes(X, Y, color = Z)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c(limits = c(-5,5), breaks = c(-4,0,4), labels = c("Low", "Medium", "High")) +
  geom_smooth(method = "lm", formula = "y~x", se=F, color = "black", linetype = "dashed") + 
  labs(x = "Sleep level", color = "Alertness", y = "Test result") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) )

pdf(file = "figures/sleep_performance.pdf", 5, 4)
p1
dev.off()

(p2 <- dat1 %>%
  ggplot(aes(X, Y)) +
  geom_point(aes(color = Z_bn), alpha = 0.5) +
  scale_color_viridis_d(breaks = c(1,3,5), labels = c("Low", "Medium", "High"),
                        guide = guide_legend(reverse = TRUE)) +
  # geom_smooth(method = "lm", formula = "y~x", se=F, color = "black", linetype = "dashed") + 
  geom_smooth(aes(color = Z_bn), method = "lm", formula = "y~x", se=F, linetype = "solid") + 
  labs(x = "Sleep level", color = "Alertness", y = "Test result") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()))

pdf(file = "figures/sleep_performance2.pdf", 5, 4)
p2
dev.off()


dat1 %>% rename(test_result = Y, sleep = X, alertness = Z) %>%
  lm(test_result ~ sleep + alertness, data = .) %>% summary
dat1 %>% rename(test_result = Y, sleep = X, alertness = Z) %>%
  lm(test_result ~ sleep, data = .) %>% summary


# fork

N <- 1000
Z <- rbern(N)
X <- rnorm(N,2*Z-1)
Y <- rnorm(N,(2*Z-1))

plot( X , Y , col=cols[Z+1] , pch=16 )
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=1,lwd=3)
abline(lm(Y~X),lwd=3,lty=3)

# collider

N <- 1000
U <- rnorm(N)
U_bn <- factor(as.numeric(cut_number(U,2)))
G <- rnorm(N)
P <- rnorm(N, 2*U + G)
P_bn <- factor(as.numeric(cut_number(P,9)))
C <- rnorm(N, 4*U + P)

dat2 <- tibble(G, P, C, U, P_bn, U_bn)

dat2 %>% lm(C ~ G, data = .) %>% summary
dat2 %>% lm(C ~ G * P, data = .) %>% summary
dat2 %>% lm(C ~ G + P + U, data = .) %>% summary


(p3 <- dat2 %>%
		ggplot(aes(G, C)) +
		geom_point(color = "turquoise", alpha = 0.5) +
		labs(x = "Grandparent education level", y = "Child education level") +
		theme(axis.text = element_blank(),
		      axis.ticks = element_blank()) +
		geom_smooth(method = "lm", formula = "y~x", se=F, linetype = "solid", color = "black"))

pdf(file = "figures/grandparents1.pdf", 5, 4)
p3
dev.off()

(p4 <- dat2 %>%
  ggplot(aes(G, C)) +
  geom_point(aes(color = P_bn), alpha = 0.5) +
  scale_color_viridis_d(breaks = c(1,5,9), labels = c("Low", "Medium", "High"),
                        guide = guide_legend(reverse = TRUE)) +
  labs(x = "Grandparent education level", color = "Parent education\nlevel", y = "Child education level") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_smooth(aes(color = P_bn, group = P_bn), method = "lm", formula = "y~x", se=F, linetype = "solid"))

pdf(file = "figures/grandparents2.pdf", 6, 4)
p4
dev.off()

dat2 %>% lm(C ~ G * P, data = .) %>% summary

(p5 <- dat2 %>%
		ggplot(aes(G, C)) +
		geom_point(aes(color = interaction(P_bn), fill = U_bn), shape = 21, stroke = 2, alpha = 0.5) +
		scale_color_viridis_d(breaks = c(1,5,9), labels = c("Low", "Medium", "High"),
				      guide = guide_legend(reverse = TRUE)) +
		scale_fill_manual(labels = c("Bad", "Good"), values = c( "purple", "yellow"),
				  guide = guide_legend(reverse = TRUE)) +
		labs(x = "Grandparent education level", color = "Parent education\nlevel", fill = "Neighbhourhood", y = "Child education level") +
		# geom_smooth(aes(group = interaction(P_bn,U_bn)), color = "black", method = "lm", formula = "y~x", se=F, linetype = "solid") +
		theme(axis.text = element_blank(),
		      axis.ticks = element_blank()))

pdf(file = "figures/grandparents3.pdf", 6, 4)
p5
dev.off()





