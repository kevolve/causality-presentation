# Examples 1 and 2 adapted from: https://github.com/rmcelreath/causal_salad_2021/blob/main/1_causal_salad.r

require(rethinking)
require(tidyverse)
# Set default theme for plots:
ggplot2::theme_set(ggplot2::theme_classic() + 
                     theme(panel.border = element_rect(fill = NA, colour = "black", linewidth = rel(1)),
                           panel.background = element_rect(fill="transparent"),
                           plot.background =  element_rect(fill="transparent", color = "transparent"),
                           strip.background = element_blank(),
                           legend.background = element_rect(fill='transparent'),
                           legend.box.background = element_rect(fill='transparent', color = "transparent"),
                           plot.title = element_text(hjust = 0.5),
                           axis.text = element_text(color = "black"),
                           strip.text.y = element_text(angle = 0)))
                           # axis.text.x.top = element_blank(),
                           # axis.ticks.x.top = element_blank(),
                           # axis.line.x.top = element_blank(),
                           # axis.text.y.right = element_blank(),
                           # axis.ticks.y.right = element_blank(),
                           # axis.line.y.right = element_blank()))

#### Example 1: Sleep ####
# Sleep->Alertness->Test Results pipe example

set.seed(1)
N <- 1000
sleep <- rnorm(N)
alert <- rnorm(N, sleep)
alert_bn <- as.numeric(cut_number(alert,9))
test_score <- rnorm(N, alert)
dat1 <- tibble(sleep, alert, alert_bn = factor(alert_bn), test_score)

tibble(sleep, alert, test_score)


# Plot of sleep level accounting for alertness
(p1 <- dat1 %>%
    ggplot(aes(sleep, test_score)) +
    geom_point(aes(color = alert_bn), alpha = 0.5) +
    scale_color_viridis_d(breaks = c(1,5,9), labels = c("Low", "Medium", "High"),
                          guide = guide_legend(reverse = TRUE)) +
    geom_smooth(aes(color = alert_bn), method = "lm", formula = "y~x", se=F, linetype = "solid") + 
    geom_abline(aes(intercept = 0.01724, slope = 0.00809),  linetype = "dashed", linewidth = 1) + 
    labs(x = "Sleep level", color = "Alertness", y = "Test result") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()))
ggsave("sleep_performance1.pdf", width = 5, height = 4, plot = p1)

# Plot of sleep level, line if ignoring alertness
(p2 <- dat1 %>%
    ggplot(aes(sleep, test_score, color = alert)) +
    geom_point(alpha = 0.5) +
    scale_color_viridis_c(limits = c(-5,5), breaks = c(-4,0,4), labels = c("Low", "Medium", "High")) +
    geom_smooth(method = "lm", formula = "y~x", se=F, color = "black", linetype = "dashed") + 
    labs(x = "Sleep level", color = "Alertness", y = "Test result") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) )
ggsave("sleep_performance2.pdf", width = 5, height = 4, plot = p2)


dat1 %>%
  lm(test_score ~ sleep + alert, data = .) %>% summary
dat1 %>%
  lm(test_score ~ sleep, data = .) %>% summary
dat1 %>% rename(test_result = Y, sleep = X, alertness = Z) %>%
  lm(test_result ~ sleep, data = .) %>% summary


#### Example 2: NBA players ####

# Basketball player height vs scoring ability

set.seed(1)
N <- 10000
id <- 1:N
H <- rnorm(N)
S <- rnorm(N, H*0.8)
Q <- plogis(rnorm(N, H + S)) # basketball player quality scoring, on logit scale
Q_cat <- as.numeric(cut_number(Q,n=100))

dat1 <- tibble(id, H, S, Q, Q_cat) %>%
	mutate(is_nba = ifelse(Q_cat == 100, 1, 0))

# nba_players <- sample(id, size = 100, replace=F, prob = B) # select 100 random NBA players weighted by their scoring ability
# dat1 <- tibble(id, H, S, B) %>%
# 	mutate(is_nba = ifelse(id %in% nba_players, 1, 0))
# Subset plot:
(p1 <- dat1 %>%
		filter(is_nba == 1) %>% # select only NBA players
		ggplot(aes(H, S)) +
		geom_point(show.legend = FALSE, 
			   alpha = 0.5, aes(color = factor(is_nba))) +
		
		scale_color_manual(values = c("#26828EFF")) +
		# scale_color_manual(values = c("#440154FF","#26828EFF")) +
		
		# scale_color_viridis_c(limits = c(-5,5), breaks = c(-4,0,4), labels = c("Low", "Medium", "High")) +
		geom_smooth(method = "lm", formula = "y~x", se=F, color = "#26828EFF", linetype = "dashed") + 
		labs(x = "Player height", y = "Average points per game") +
		theme(axis.text = element_blank(),
		      axis.ticks = element_blank()))
ggsave("nba1.pdf", width = 4.5, height = 4, plot = p1)


# Including entire population plot:
(p2 <- dat1 %>%
		mutate(is_nba = ifelse(is_nba == 1, "NBA player", "General population")) %>%
		ggplot(aes(H, S)) +
		geom_point(show.legend = TRUE, 
			   alpha = 0.3, aes(color = is_nba)) +
		geom_smooth(show.legend = FALSE, aes(color = is_nba), method = "lm", formula = "y~x", se=F, linetype = "dashed") + 
		geom_smooth(show.legend = FALSE, color = "black", method = "lm", formula = "y~x", se=F,  linetype = "dashed") + 
		
		scale_color_manual(values = c("#440154FF","#26828EFF")) +
		scale_fill_manual(values = c("#440154FF","#26828EFF")) +
		
		labs(x = "Player height", y = "Average points per game",
		     color = NULL) +
		theme(axis.text = element_blank(),
		      axis.ticks = element_blank()))
ggsave("nba2.pdf", width = 6, height = 4, plot = p2)

# Stats:
dat1 %>% filter(is_nba == 1) %>%
	rename(H, S, is_nba ) %>%
	lm(test_result ~ sleep + alertness, data = .) %>% summary
dat1 %>% rename(test_result = Y, sleep = X, alertness = Z) %>%
	lm(test_result ~ sleep, data = .) %>% summary


#### Example 3: Bad grandparents example ####

# Grandparent-grandchildren hidden collider

set.seed(2)
N <- 1000
U <- rnorm(N)
U_bn <- factor(as.numeric(cut_number(U,3)))
G <- rnorm(N)
P <- rnorm(N, 2*U + G)
P_bn <- factor(as.numeric(cut_number(P,9)))
C <- rnorm(N, 4*U + P + G)

dat2 <- tibble(G, P, C, U, P_bn, U_bn)

parent_only_mod <- lm(C ~ P, data = dat2)
grandparent_only_mod <- lm(C ~ G, data = dat2)
grandparent_plus_parent_mod <- lm(C ~ G + P, data = dat2)
grandparent_x_parent_mod <- lm(C ~ G * P, data = dat2)
best_model <- lm(C ~ G + P + U, data = dat2)

MuMIn::AICc(parent_only_mod,
            grandparent_only_mod, 
            grandparent_plus_parent_mod, 
            grandparent_x_parent_mod) %>% arrange(AICc) %>%
  mutate("ΔAICc" = AICc - AICc[1])
# Best model is grandparent + parent as explanatory factor


(p1 <- dat2 %>%
    ggplot(aes(P, C)) +
    geom_point(color = "#96B0E1", alpha = 0.5) +
    labs(x = "Parents' education level", y = "Child's education level") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    geom_smooth(method = "lm", formula = "y~x", se=F, linetype = "solid", color = "black"))
ggsave("grandparents1.pdf", width = 4, height = 3, plot = p1)


(p2 <- dat2 %>%
    ggplot(aes(G, C)) +
    geom_point(color = "turquoise", alpha = 0.5) +
    labs(x = "Grandparents' education level", y = "Child's education level") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    geom_smooth(method = "lm", formula = "y~x", se=F, linetype = "solid", color = "black"))
ggsave("grandparents2.pdf", width = 4, height = 3, plot = p2)

(p3 <- dat2 %>%
  ggplot(aes(G, C)) +
  geom_point(aes(color = P_bn), alpha = 0.5) +
  scale_color_viridis_d(breaks = c(1,5,9), labels = c("Low", "Medium", "High"),
                        guide = guide_legend(reverse = TRUE),
                        option="C") +
  labs(x = "Grandparents' education level", color = "Parents'\neducation", y = "Child's education level") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_smooth(aes(color = P_bn, group = P_bn), method = "lm", formula = "y~x", se=F, linetype = "solid"))
ggsave("grandparents3.pdf", width = 4.5, height = 3, plot = p3)


dat2 %>% lm(C ~ G + P, data = .) %>% summary

(p4 <- dat2 %>%
		ggplot(aes(G, C)) +
		geom_point(aes(fill = P_bn, stroke = (as.numeric(U_bn)-1)), shape = 21, alpha = 0.5, color = "red") +
    geom_smooth(aes(color = P_bn, group = paste0(P_bn,U_bn)), method = "lm", formula = "y~x", se=F, linetype = "solid",
                show.legend=FALSE) +
    scale_fill_viridis_d(breaks = c(1,5,9), labels = c("Low", "Medium", "High"),
				      guide = guide_legend(reverse = TRUE),
				      option = "C") +
    scale_color_viridis_d(breaks = c(1,5,9), labels = c("Low", "Medium", "High"),
                         guide = guide_legend(reverse = TRUE),
                         option = "C") +
		scale_linewidth_manual(labels = c("Bad", "Ok", "Good"),
				  guide = guide_legend(reverse = TRUE)) +
		labs(x = "Grandparents' education level", fill = "Parents' education\nlevel", color = "Neighbhourhood", y = "Child's education level") +
		theme(axis.text = element_blank(),
		      axis.ticks = element_blank()))
ggsave("grandparents4.pdf", width = 5, height = 3, plot = p4)

dat2 %>% lm(C ~ G + P + U, data = .) %>% summary



