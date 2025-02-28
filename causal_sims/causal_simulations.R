
#### Set-up ####

# Examples 1 and 2 adapted from: 
# https://github.com/rmcelreath/causal_salad_2021/blob/main/1_causal_salad.r

# Load (or install if not present) some libraries:
require(tidyverse)
require(dagitty)
require(ggdag)


# Set default theme for ggplots:
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

#### Example 1: Sleep ####
# Sleep->Alertness->Test Results pipe example


##### Simulate data #####

set.seed(1)
N <- 1000
sleep <- rnorm(N)             # mean of 1, sd of 1
alert <- rnorm(N, sleep)      # mean of sleep, sd of 1
alert_bn <- as.numeric(cut_number(alert,9)) # categorical alertness score
test_score <- rnorm(N, alert) # mean of alertness, sd of 1

dat1 <- tibble(sleep, alert, alert_bn = factor(alert_bn), test_score)
head(dat1)


##### Data plots #####

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
ggsave("plots/sleep_performance1.pdf", width = 5, height = 4, plot = p1)

# Plot of sleep level, line if ignoring alertness
(p2 <- dat1 %>%
    ggplot(aes(sleep, test_score, color = alert)) +
    geom_point(alpha = 0.5) +
    scale_color_viridis_c(limits = c(-5,5), breaks = c(-4,0,4), labels = c("Low", "Medium", "High")) +
    geom_smooth(method = "lm", formula = "y~x", se=F, color = "black", linetype = "dashed") + 
    labs(x = "Sleep level", color = "Alertness", y = "Test result") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) )
ggsave("plots/sleep_performance2.pdf", width = 5, height = 4, plot = p2)


##### Stats #####

dat1 %>%
  lm(test_score ~ sleep + alert, data = .) %>% summary
dat1 %>%
  lm(test_score ~ sleep, data = .) %>% summary


##### DAG and paths #####

sleep_dag <- ggdag::dagify(
  alert ~ sleep,
  test_score ~ alert)

sleep_dag %>% 
  ggdag_paths(from = "sleep", to = "test_score",
              shadow = TRUE)
ggsave("plots/sleep_dag.pdf", width = 5, height = 4)


# Return minimum adjustment set 
# (variables needed to condition on for y-var identifiability)
dagitty::adjustmentSets(sleep_dag, 
                        exposure = c("sleep"), # x-variable(s) that we're conditioning on
                        outcome = "test_score", # y-variable
                        type = "minimal")
# {} = No other vars required!

# Return canonical adjustment set
# (gives variables that will better explain variation in y-variable)
dagitty::adjustmentSets(sleep_dag, 
                        exposure = c("sleep"), # x-variable(s) that we're conditioning on
                        outcome = "test_score", # y-variable
                        type = "canonical")
# {} = No other vars required!

# How to measure the direct effect (path coefficient) of habhet on gendiv:
dagitty::adjustmentSets(sleep_dag, 
                        exposure = c("sleep"), # x-variable(s) that we're conditioning on
                        outcome = "test_score", # y-variable
                        effect = "direct") # measure 'direct' effect (not effect going through alertness) of sleep on test score
# {alert} = must add alertness to determine direct effect of sleep on test score


#### Example 2: NBA players ####


##### Simulate data #####
# Basketball player height vs scoring ability

set.seed(1)
N <- 10000
id <- 1:N
Height <- rnorm(N) # mean = 1, sd = 1
Scoring <- rnorm(N, Height*0.8) # mean of 80% of height, sd = 1
Quality <- plogis(rnorm(N, Height + Scoring)) # mean of H + S, sd = 1, transformed to logit scale
Quality_cat <- as.numeric(cut_number(Quality, n=100))
NBA_player <- ifelse(Quality_cat == 100, "yes", "no")
  

dat2 <- tibble(id, Height, Scoring, Quality, Quality_cat, NBA_player)
head(dat2)

##### Data plots #####

# Subset plot:
(p1 <- dat2 %>%
		filter(NBA_player == "yes") %>% # select only NBA players
		ggplot(aes(Height, Scoring)) +
		geom_point(show.legend = FALSE, 
			   alpha = 0.5, aes(color = factor(NBA_player))) +
		scale_color_manual(values = c("#440154FF")) +
geom_smooth(method = "lm", formula = "y~x", se=F, color = "#440154FF", linetype = "dashed") + 
		labs(x = "Player height", y = "Average points per game") +
		theme(axis.text = element_blank(),
		      axis.ticks = element_blank()))
ggsave("plots/nba_plot1.pdf", width = 3.5, height = 3, plot = p1)

# Including entire population plot:
(p2 <- dat2 %>%
    ggplot(aes(Height, Scoring)) +
    geom_point(show.legend = FALSE, 
               alpha = 0.5, aes(color = factor(NBA_player))) +
		geom_smooth(show.legend = FALSE, aes(color = NBA_player), method = "lm", formula = "y~x", se=F, linetype = "dashed") + 
		geom_smooth(show.legend = FALSE, color = "black", method = "lm", formula = "y~x", se=F,  linetype = "dashed") + 
		
		scale_color_manual(values = c("#26828EFF", "#440154FF")) +
		scale_fill_manual(values = c("#26828EFF", "#440154FF")) +
		
		labs(x = "Player height", y = "Average points per game",
		     color = NULL) +
		theme(axis.text = element_blank(),
		      axis.ticks = element_blank()))
ggsave("plots/nba_plot2.pdf", width = 6, height = 4, plot = p2)

##### Stats #####

# Conditioning on only NBA players:
dat2 %>% 
  filter(NBA_player == "yes") %>% # select only NBA players
	lm(Scoring ~ Height, data = .) %>% summary
# No significant effect of height detected!

# Including entire population:
dat2 %>% 
  lm(Scoring ~ Height, data = .) %>% summary
# Height effect detected and strong!

##### DAG and paths #####

nba_dag <- ggdag::dagify(
  Scoring ~ Height,
  NBA_player ~ Scoring + Height)

nba_dag %>% 
  ggdag_paths(from = "Height", to = "Scoring",
              shadow = TRUE)
ggsave("plots/nba_dag.pdf", width = 5, height = 4)


# Return minimum adjustment set 
# (variables needed to condition on for y-var identifiability)
dagitty::adjustmentSets(nba_dag, 
                        exposure = c("Height"), # x-variable(s) that we're conditioning on
                        outcome = "Scoring", # y-variable
                        type = "minimal")
# {} = No other vars required (i.e., don't condition on whether they are an NBA player!)

# Return canonical adjustment set
# (gives variables that will better explain variation in y-variable)
dagitty::adjustmentSets(nba_dag, 
                        exposure = c("Height"), # x-variable(s) that we're conditioning on
                        outcome = "Scoring", # y-variable
                        type = "canonical")
# {} = No other vars required (i.e., don't condition on whether they are an NBA player!)

# How to measure the direct effect (path coefficient) of habhet on gendiv:
dagitty::adjustmentSets(nba_dag, 
                        exposure = c("Height"), # x-variable(s) that we're conditioning on
                        outcome = "Scoring", # y-variable
                        effect = "direct") # measure 'direct' effect (not effect going through alertness) of sleep on test score
# {} = No other vars required (i.e., don't condition on whether they are an NBA player!)


#### Example 3: Bad grandparents example ####
# Grandparent-grandchildren hidden collider

##### Simulate data #####

set.seed(2)
N <- 1000
U <- rnorm(N)
U_bn <- factor(as.numeric(cut_number(U,3)))
G <- rnorm(N)
P <- rnorm(N, 2*U + G)
P_bn <- factor(as.numeric(cut_number(P,9)))
C <- rnorm(N, 4*U + P + G)

dat3 <- tibble(G, P, C, U, P_bn, U_bn)

##### Stats #####
(parent_only_mod <- lm(C ~ P, data = dat2))
(grandparent_only_mod <- lm(C ~ G, data = dat2))
(grandparent_parent_mod <- lm(C ~ G + P, data = dat2))
(grandparent_x_parent_mod <- lm(C ~ G * P, data = dat2))
(best_model <- lm(C ~ G + P + U, data = dat2))

MuMIn::AICc(parent_only_mod,
            grandparent_only_mod, 
            grandparent_parent_mod, 
            grandparent_x_parent_mod) %>% arrange(AICc) %>%
  mutate("ΔAICc" = AICc - AICc[1])
# Best model is grandparent + parent as explanatory factor

##### Data plots #####
(p1 <- dat3 %>%
    ggplot(aes(P, C)) +
    geom_point(color = "#96B0E1", alpha = 0.5) +
    labs(x = "Parents' education level", y = "Child's education level") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    geom_smooth(method = "lm", formula = "y~x", se=F, linetype = "solid", color = "black"))
ggsave("plots/grandparents1.pdf", width = 4, height = 3, plot = p1)
summary(parent_only_mod)

(p2 <- dat3 %>%
    ggplot(aes(G, C)) +
    geom_point(color = "turquoise", alpha = 0.5) +
    labs(x = "Grandparents' education level", y = "Child's education level") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    geom_smooth(method = "lm", formula = "y~x", se=F, linetype = "solid", color = "black"))
ggsave("plots/grandparents2.pdf", width = 4, height = 3, plot = p2)
summary(grandparent_only_mod)

(p3 <- dat3 %>%
  ggplot(aes(G, C)) +
  geom_point(aes(color = P_bn), alpha = 0.5) +
  scale_color_viridis_d(breaks = c(1,5,9), labels = c("Low", "Medium", "High"),
                        guide = guide_legend(reverse = TRUE),
                        option="C") +
  labs(x = "Grandparents' education level", color = "Parents'\neducation", y = "Child's education level") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_smooth(aes(color = P_bn, group = P_bn), method = "lm", formula = "y~x", se=F, linetype = "solid"))
ggsave("plots/grandparents3.pdf", width = 4.5, height = 3, plot = p3)
summary(grandparent_parent_mod)
summary(grandparent_x_parent_mod)


(p4 <- dat3 %>%
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
ggsave("plots/grandparents4.pdf", width = 5, height = 3, plot = p4)
summary(best_model)


#### Example 4: Berkeley Admissions (incomplete) ####
##### DAG and paths #####

# Dag1 with Quality affecting Department choice:

berkeley_dag1 <- ggdag::dagify(
  Department ~ Gender, # this represents societal biases in gender
  Acceptance ~ Gender + Department + Quality)

berkeley_dag1 %>% 
  ggdag_paths(from = "Gender", to = "Acceptance",
              shadow = TRUE)
ggsave("plots/berkeley_dag1.pdf", width = 5, height = 4)

# Interested specifically in the DIRECT effect:
dagitty::adjustmentSets(berkeley_dag1, 
                        exposure = c("Gender"), # x-variable(s) that we're conditioning on
                        outcome = "Acceptance", # y-variable
                        effect = "direct") # measure 'direct' effect (not effect going through alertness) of sleep on test score
# { Department } = Need to condition on department!


# Dag2 with Quality affecting Department choice:
berkeley_dag2 <- ggdag::dagify(
  Department ~ Gender + Quality,
  Acceptance ~ Gender + Department + Quality)

berkeley_dag2 %>% 
  ggdag_paths(from = "Gender", to = "Acceptance",
              shadow = TRUE)
ggsave("plots/berkeley_dag2.pdf", width = 5, height = 4)

# Interested specifically in the DIRECT effect:
dagitty::adjustmentSets(berkeley_dag2, 
                        exposure = c("Gender"), # x-variable(s) that we're conditioning on
                        outcome = "Acceptance", # y-variable
                        effect = "direct") # measure 'direct' effect (not effect going through alertness) of sleep on test score
# { Department, Quality } Need to condition on department & Quality!


##### Simulate data #####

set.seed(2)
N <- 20000
N_Dept <- 30
Gender <- c(rep("Female", N/2), rep("Male", N/2)) 
Quality <- rnorm(N) # mean of 1, sd of 1
Department_difficulty <- rnorm(N_Dept, ifelse(Gender == "Male", 1.5, 1)) # difficulty in getting into some departments due to societal biases towards men/women

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




