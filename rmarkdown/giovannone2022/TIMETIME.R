# Last updated 06-02-2022; rmt.

# Load packages.
# Data manipulation.
library(tidyverse)

# Analysis. 
library(lme4)
library(lmerTest)
library(ez)
library(cocor)
library(jtools)

# Plots.
library(ggplot2)
library(cowplot)
library(interactions)

# Import data. -----
setwd("/Users/rachelmtheodore/Desktop/TIMETIME/TIMETIME-OSF")
demographics <- read.csv("TIMETIME-Demographics.csv", stringsAsFactors = TRUE)
ganong <- read.csv("TIMETIME-Ganong.csv", stringsAsFactors = TRUE)
pr <- read.csv("TIMETIME-PR.csv", stringsAsFactors = TRUE)
ltrs <- read.csv("TIMETIME-LTRS.csv", stringsAsFactors = TRUE)

# Demographic report.
demographics %>%
  filter(N.Sessions == 2) %>% 
  group_by(Sex) %>% 
  summarise(Count = length(ID)) # 50 women, 21 men, 2 prefer not to say.

demographics %>%
  filter(N.Sessions == 2) %>% 
  summarise(Age.Mean = round(mean(Age), digits=0), # 26 years.
            Age.SD = round(sd(Age), digits=0), # 5 years.
            Age.Min = min(Age), # 18 - 35 years.
            Age.Max = max(Age))

demographics %>% 
  group_by(N.Sessions) %>% 
  summarise(Count = length(ID)) # 73 complete both sessions.

demographics %>% 
  group_by(Headphone) %>% 
  summarise(Count = length(ID)) # All pass headphone screen.

demographics %>% 
  filter(N.Sessions == 2) %>% 
  summarise(Mean = round(mean(Days), digits=0), # 17
            SD = round(sd(Days), digits=0), # 4
            Min = min(Days), # 14
            Max = max(Days)) # 3.

# Primary analyses include participants who completed both sessions.
# At the end of this script, we run parallel analysis for session 1
# with all participants; reported in the supplement.

# Primary analyses. -----
# Ganong. -----
# Order vectors.
ganong$Continuum <- factor(ganong$Continuum, levels=c("giss", "gift"))

# Summary by ID.
ganong.summary.ID <- ganong %>%
  filter(N.Sessions == 2) %>% 
  group_by(ID, Session, Continuum, VOT) %>%
  summarise(Mean.K = mean(Response)) 
ganong.summary.ID <- droplevels(ganong.summary.ID) # Confirm n = 73.

# Summary over ID.
ganong.summary <- ganong.summary.ID %>%
  group_by(Session, Continuum, VOT) %>%
  summarise(K.Mean = mean(Mean.K),
            K.SE = sd(Mean.K)/(sqrt(length(Mean.K))))

# Plot.
labels.Session <- c(`1` = "Session 1", `2` = "Session 2")
ganong.plot <- ggplot(ganong.summary, aes(x=VOT, y=K.Mean, group=Continuum, color=Continuum)) +
  theme_cowplot(font_size=20) +
  geom_point() +
  geom_line(aes(linetype=Continuum), size=0.5, show.legend = FALSE) +
  geom_errorbar(aes(y = K.Mean, ymin = K.Mean-K.SE, ymax = K.Mean+K.SE), width = 1) +
  ylim(0, 1) +
  labs(x="VOT (ms)", y = "p(k)") +
  scale_color_manual(values=c("#00846b", "#6a0084"),
                     labels = c(expression(italic("giss - kiss")),
                                expression(italic("gift - kift")))) +
  theme(legend.text.align = 0) +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  facet_wrap(. ~ Session, labeller=labeller(Session = labels.Session))
ganong.plot

# Models.
# Set contrasts for continuum and session.
contrasts(ganong$Continuum) <- c(0.5, -0.5)
contrasts(ganong$Continuum) # gift on the negative.

ganong$Session <- factor(ganong$Session)
contrasts(ganong$Session) <- c(-.5, .5) # Session 1 on the negative.
contrasts(ganong$Session)

# Scale.center VOT.
ganong$VOT.s <- scale(ganong$VOT)

# Run the models.
# Session 1.
glmm.ganong.S1 <- glmer(Response ~ (VOT.s * Continuum) + (VOT.s * Continuum|ID),
                        data=ganong %>% filter(Session == "1" & N.Sessions == 2), # Limit to people who did both sessions.
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(glmm.ganong.S1)
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.4493     0.1640  -2.739  0.00616 ** 
# VOT.s              2.8450     0.1312  21.679  < 2e-16 ***
# Continuum1         3.0793     0.2690  11.446  < 2e-16 ***
# VOT.s:Continuum1   0.3988     0.1884   2.117  0.03427 *  

# Session 2.
glmm.ganong.S2 <- glmer(Response ~ (VOT.s * Continuum) + (VOT.s * Continuum|ID),
                        data=ganong %>% filter(Session == "2" & N.Sessions == 2), # Limit to people who did both sessions.
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(glmm.ganong.S2)
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.8510     0.1544  -5.510 3.58e-08 ***
# VOT.s              3.5307     0.2005  17.606  < 2e-16 ***
# Continuum1         1.9252     0.2901   6.637 3.20e-11 ***
# VOT.s:Continuum1   0.2692     0.2042   1.318    0.188  

# Now compare across sessions, for those who did both sessions.
glmm.ganong.Sx <- glmer(Response ~ (VOT.s * Continuum * Session) + (VOT.s * Continuum * Session|ID),
                        data=ganong %>% filter(N.Sessions == 2),
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(glmm.ganong.Sx)
#                           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                -0.6688     0.1438  -4.651 3.30e-06 ***
# VOT.s                       3.1585     0.1350  23.403  < 2e-16 ***
# Continuum1                  2.5635     0.2688   9.538  < 2e-16 ***
# Session1                   -0.3776     0.1322  -2.856 0.004287 ** 
# VOT.s:Continuum1            0.3246     0.1463   2.219 0.026504 *  
# VOT.s:Session1              0.6506     0.1825   3.565 0.000364 ***
# Continuum1:Session1        -1.1772     0.2327  -5.059 4.22e-07 ***
# VOT.s:Continuum1:Session1  -0.0808     0.2608  -0.310 0.756664    

# Consistent with direction of beta estimate for continuum x session interaction;
# the Ganong effect gets smaller from session 1 to session 2.
cat_plot(glmm.ganong.Sx, pred = Session, modx = Continuum)

# Phonemic restoration. -----
# Format data structure. 
pr$Session <- factor(pr$Session)
pr$Type <- factor(pr$Type, levels=c("Replaced", "Added"))
pr$Block <- factor(pr$Block, levels=c("Word", "Nonword"))

# Summary by ID.
pr.summary.ID <- pr %>%
  filter(Item != "Filler") %>% # Remove filler trials.
  filter(N.Sessions == 2) %>% # Limit to those who did both sessions.
  group_by(ID, Session, Block, Type) %>%
  summarise(Mean.Rating = mean(Response))
pr.summary.ID <- droplevels(pr.summary.ID)

# Summary over ID.
pr.summary <- pr.summary.ID %>% 
  group_by(Session, Block, Type) %>%
  summarise(Rating.Mean = mean(Mean.Rating),
            Rating.SE = sd(Mean.Rating)/(sqrt(length(Mean.Rating))))

# Plot.
pr.plot <- ggplot(pr.summary, aes(x=Block, y=Rating.Mean, fill = Type)) +
  #theme_cowplot(font_size=20) +
  geom_bar(stat="identity", position = position_dodge(width=0.9), width=0.9) +
  geom_errorbar(aes(y = Rating.Mean, ymin = Rating.Mean-Rating.SE, ymax = Rating.Mean+Rating.SE),
                position = position_dodge(width=0.9), width=0.4) +
  coord_cartesian(ylim = c(4.5, 8), expand = FALSE) +
  scale_fill_manual(values=c("#00846b", "#6a0084")) +
  labs(y = "Similarity rating") +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  theme(legend.text.align = 0) +
  theme(panel.grid.major.y = element_line(colour="grey")) + 
  facet_wrap(. ~ Session, labeller=labeller(Session = labels.Session))
pr.plot

# ANOVAs to parallel analysis in Ishida et al.; reported in footnote.
pr.aov.S1 <- ezANOVA(pr.summary.ID %>% filter(Session == "1"),
                     dv = Mean.Rating,
                     wid = ID,
                     within = .(Block, Type),
                     detailed = TRUE)
pr.aov.S1
#         Effect DFn DFd          SSn        SSd          F            p p<.05         ges
# 1 (Intercept)   1  72 13144.755899 302.178476 3131.99814 4.321307e-61     * 0.971361352
# 2       Block   1  72    25.535351  66.824024   27.51324 1.503052e-06     * 0.061816634
# 3        Type   1  72     8.374324  11.467551   52.57891 3.796169e-10     * 0.021151492
# 4  Block:Type   1  72     1.620077   7.076798   16.48281 1.233153e-04     * 0.004162936

pr.aov.S2 <- ezANOVA(pr.summary.ID %>% filter(Session == "2"),
                     dv = Mean.Rating,
                     wid = ID,
                     within = .(Block, Type),
                     detailed = TRUE)
pr.aov.S2
#         Effect DFn DFd          SSn        SSd           F            p p<.05         ges
# 1 (Intercept)   1  72 1.302159e+04 293.701901 3192.198130 2.210510e-61     * 0.970973693
# 2       Block   1  72 1.243172e+01  82.260154   10.881136 1.511882e-03     * 0.030947816
# 3        Type   1  72 4.159324e+00   6.982551   42.888521 7.419648e-09     * 0.010572032
# 4  Block:Type   1  72 6.012414e-01   6.323134    6.846192 1.081750e-02     * 0.001542163

pr.aov.Sx <- ezANOVA(pr.summary.ID,
                     dv = Mean.Rating,
                     wid = ID,
                     within = .(Block, Type, Session),
                     detailed = TRUE)
pr.aov.Sx
#               Effect DFn DFd          SSn        SSd            F            p p<.05          ges
# 1        (Intercept)   1  72 2.616620e+04 537.059058 3507.9319843 7.953079e-63     * 0.9711682415
# 2              Block   1  72 3.680062e+01 118.489384   22.3618716 1.092651e-05     * 0.0452309841
# 3               Type   1  72 1.216865e+01  13.828853   63.3561308 1.823632e-11     * 0.0154232013
# 4            Session   1  72 1.449315e-01  58.821318    0.1774028 6.748695e-01       0.0001865368
# 5         Block:Type   1  72 2.097603e+00   7.332397   20.5972742 2.218334e-05     * 0.0026929900
# 6      Block:Session   1  72 1.166455e+00  30.594795    2.7450681 1.019067e-01       0.0014993366
# 7       Type:Session   1  72 3.650000e-01   4.621250    5.6867731 1.972891e-02     * 0.0004696469
# 8 Block:Type:Session   1  72 1.237158e-01   6.067534    1.4680649 2.296128e-01       0.0001592350

# Models.
# Set contrasts.
contrasts(pr$Type) <- c(-0.5, 0.5) # Replaced on the negative.
contrasts(pr$Type)
contrasts(pr$Block) <- c(-0.5, 0.5) # Word on the negative.
contrasts(pr$Block)
contrasts(pr$Session) <- c(-0.5, 0.5) # Session 1 on the negative.
contrasts(pr$Session)

# Session 1.
pr.lmer.S1 <- lmer(Response ~ Type * Block + (Type * Block|ID),
                   data = pr %>% filter(Session == "1" & N.Sessions == 2 & Item != "Filler"),
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(pr.lmer.S1)
summ(pr.lmer.S1, digits=3)
#               Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)   6.70942    0.11989 72.00028  55.964  < 2e-16 ***
# Type1         0.33870    0.04744 74.53081   7.140 5.22e-10 ***
# Block1       -0.59144    0.11279 72.00664  -5.244 1.51e-06 ***
# Type1:Block1  0.29795    0.07961 96.22800   3.742  0.00031 ***

# Consistent with the direction of the beta estimate for the type x block interaction,
# the type effect gets larger from word to nonword block.
cat_plot(pr.lmer.S1, pred="Block", modx="Type")

pr.lmer.S1.ss <- lmer(Response ~ Block / Type + (Type * Block|ID),
                      data = pr %>% filter(Session == "1" & N.Sessions == 2 & Item != "Filler"),
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(pr.lmer.S1.ss)
# BlockWord:Type1      0.18973    0.05005 139.40214   3.790 0.000223 ***
# BlockNonword:Type1   0.48767    0.07186  72.68902   6.786 2.63e-09 ***

# Session 2.
pr.lmer.S2 <- lmer(Response ~ Type * Block + (Type * Block|ID),
                   data = pr %>% filter(Session == "2" & N.Sessions == 2 & Item != "Filler"),
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(pr.lmer.S2)
#               Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   6.67791    0.11819 72.00032  56.500  < 2e-16 ***
# Type1         0.23870    0.03645 71.99963   6.549 7.42e-09 ***
# Block1       -0.41267    0.12510 72.00030  -3.299  0.00151 ** 
# Type1:Block1  0.18151    0.06937 71.99973   2.617  0.01082 *  

# Consistent with the direction of the beta estimate for the type x block interaction,
# the type effect gets larger from word to nonword block.
cat_plot(pr.lmer.S2, pred="Block", modx="Type")

pr.lmer.S2.ss <- lmer(Response ~ Block / Type + (Type * Block|ID),
                      data = pr %>% filter(Session == "2" & N.Sessions == 2 & Item != "Filler"),
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(pr.lmer.S2.ss)
# BlockWord:Type1     0.14795    0.04314 71.98764   3.429  0.00100 ** 
# BlockNonword:Type1  0.32945    0.05659 72.00006   5.822  1.5e-07 ***

# Across sessions.
pr.lmer.Sx <- lmer(Response ~ Type * Block * Session + (Type * Block * Session|ID),
                   data = pr %>% filter(N.Sessions == 2 & Item != "Filler"),
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(pr.lmer.Sx)
summ(pr.lmer.Sx, digits=3)
#                       Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)             6.69366    0.11302  72.00022  59.228  < 2e-16 ***
# Type1                   0.28870    0.03633  72.77928   7.946 1.81e-11 ***
# Block1                 -0.50205    0.10618  72.01932  -4.728 1.09e-05 ***
# Session1               -0.03151    0.07481  71.99985  -0.421   0.6749    
# Type1:Block1            0.23973    0.05420  91.24005   4.423 2.68e-05 ***
# Type1:Session1         -0.10000    0.04722 149.53022  -2.118   0.0359 *  
# Block1:Session1         0.17877    0.10794  72.02001   1.656   0.1020    
# Type1:Block1:Session1  -0.11644    0.10116  92.43226  -1.151   0.2527    

# Type x block interaction gets weaker from session 1 to session 2.
cat_plot(pr.lmer.Sx, pred="Block", modx="Type", mod2="Session")

# Simple slopes analysis; is there incomplete restoration in both blocks?
pr.lmer.Sx.ss <- lmer(Response ~ Block / Type + (Type * Block|ID),
                      data = pr %>% filter(N.Sessions == 2 & Item != "Filler"),
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(pr.lmer.Sx.ss)
# BlockWord:Type1      0.16884    0.03622 145.62017   4.662 7.02e-06 ***
# BlockNonword:Type1   0.40856    0.05363  72.38993   7.619 7.66e-11 ***

# LTRS. -----
# Set vectors. 
ltrs$Session <- as.factor(ltrs$Session)
ltrs$Reversal <- as.factor(ltrs$Reversal)

# Need to code to calculate sensitivity (d').
# When target is a word, then:
# hit == same responses to WW trials
# FA == same responses to WN trials

# When target is a nonword, then:
# hit == same responses to NN trials
# FA == same responses to NW trials

# To code hit and FA, first separate by structure.
target_w <- filter(ltrs, Structure == "WW" | Structure == "WN")
target_n <- filter(ltrs, Structure == "NN" | Structure == "NW")

# Add vector to mark hits and FAs.
target_w$Hit <- ifelse(target_w$Structure == "WW" & target_w$Response == "Same",
                       1, 0)
target_n$Hit <- ifelse(target_n$Structure == "NN" & target_n$Response == "Same",
                       1, 0)
target_w$FA <- ifelse(target_w$Structure == "WN" & target_w$Response == "Same",
                      1, 0)
target_n$FA <- ifelse(target_n$Structure == "NW" & target_n$Response == "Same",
                      1, 0)

# Let's bring these back together.
ltrs <- full_join(target_w, target_n)
rm(target_n, target_w)

# With hits and FAs coded, now we can calculate hit rate and FA rate by
# reversal window and lexical status of the TARGET.
# First, add vector to code word vs. nonword status of target.
ltrs$Target <- ifelse(ltrs$Structure == "WW" | ltrs$Structure == "WN",
                         "Word", "Nonword")
ltrs$Target <- factor(ltrs$Target, levels = c("Word", "Nonword"))

# Now create summaries that will calculate hit and FA rates by subject and
# by condition.
# Calculate hit rate for "same" trials.
ltrs.hit <- filter(ltrs, Structure == "WW" | Structure == "NN")
ltrs.hit <- ltrs.hit %>%
  group_by(ID, Session, N.Sessions, Target, Reversal) %>%
  summarise(Hit.Rate = mean(Hit))

# Calculate FA rate for "different" trials.
ltrs.fa <- filter(ltrs, Structure == "WN" | Structure == "NW")
ltrs.fa <- ltrs.fa %>%
  group_by(ID, Session, N.Sessions, Target, Reversal) %>%
  summarise(FA.Rate = mean(FA))

# Bring them together.
ltrs.d <- full_join(ltrs.hit, ltrs.fa)

# Correct for dPrime = inf; Hit 1 = .99, FA 0 = .01.
ltrs.d$Hit.Rate <- ifelse(ltrs.d$Hit.Rate == 1,
                          0.99, ltrs.d$Hit.Rate)
ltrs.d$FA.Rate <- ifelse(ltrs.d$FA.Rate == 0,
                         0.01, ltrs.d$FA.Rate)
ltrs.d$Hit.Rate <- ifelse(ltrs.d$Hit.Rate == 0,
                          0.01, ltrs.d$Hit.Rate)
ltrs.d$FA.Rate <- ifelse(ltrs.d$FA.Rate == 1,
                         0.99, ltrs.d$FA.Rate)

# Calculate d' by ID.
ltrs.d$Dprime <- qnorm(ltrs.d$Hit.Rate) - qnorm(ltrs.d$FA.Rate)

# Summary by ID.
ltrs.summary.ID <- ltrs.d %>%
  filter(N.Sessions == 2) %>% 
  group_by(ID, Session, Target, Reversal) %>%
  summarise(Mean.D = mean(Dprime))
ltrs.summary.ID <- droplevels(ltrs.summary.ID)

# Summary over ID.
ltrs.summary <- ltrs.summary.ID %>%
  group_by(Session, Target, Reversal) %>%
  summarise(D.Mean = mean(Mean.D),
            D.SE = sd(Mean.D)/(sqrt(length(Mean.D))))

# Plot. 
ltrs.plot <- ggplot(ltrs.summary, aes(x=Reversal, y=D.Mean, group=Target, color=Target)) +
  theme_cowplot(font_size=20) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(y = D.Mean, ymin = D.Mean-D.SE, ymax = D.Mean+D.SE), width = 0.3) +
  scale_color_manual(values=c("#00846b", "#6a0084")) +
  ylim(-3.3, 5) +
  labs(y="Sensitivity (d')", x = "Reversal window (ms)") +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  facet_wrap(. ~ Session, labeller=labeller(Session = labels.Session))
ltrs.plot 

# ANOVAs to parallel analysis in Ishida et al.; mixed effects models not ideal given
# that sensitivity (d') is aggregated across trials.
# Session 1.
ltrs.aov.S1 <- ezANOVA(ltrs.summary.ID %>% filter(Session == "1"),
                       dv = Mean.D,
                       wid = ID,
                       within = .(Target, Reversal),
                       detailed = TRUE)
ltrs.aov.S1
#           Effect DFn DFd       SSn       SSd         F            p p<.05       ges
# 1     (Intercept)   1  72 421.99948  64.34676 472.19103 2.391181e-33     * 0.5656948
# 2          Target   1  72 535.11111 182.32162 211.31887 4.125803e-23     * 0.6228769
# 3        Reversal   1  72  41.47276  38.25941  78.04717 4.248251e-13     * 0.1134817
# 4 Target:Reversal   1  72  56.31631  39.05713 103.81650 1.330467e-15     * 0.1480834

# Post-hocs for interaction.
ltrs.posthoc.S1.word <- ltrs.summary.ID %>% filter(Session == 1 & Target == "Word")
ltrs.posthoc.S1.nonword <- ltrs.summary.ID %>% filter(Session == 1 & Target == "Nonword")
ltrs.posthoc.S1.40 <- ltrs.summary.ID %>% filter(Session == 1 & Reversal == "40")
ltrs.posthoc.S1.60 <- ltrs.summary.ID %>% filter(Session == 1 & Reversal == "60")

t.test(ltrs.posthoc.S1.40$Mean.D ~ ltrs.posthoc.S1.40$Target,
       paired=TRUE, var.equal=TRUE) # Word effect @ 40.
t.test(ltrs.posthoc.S1.60$Mean.D ~ ltrs.posthoc.S1.60$Target,
       paired=TRUE, var.equal=TRUE) # Word effect @ 60.
t.test(ltrs.posthoc.S1.word$Mean.D ~ ltrs.posthoc.S1.word$Reversal,
       paired=TRUE, var.equal=TRUE) # Reversal affects words.
t.test(ltrs.posthoc.S1.nonword$Mean.D ~ ltrs.posthoc.S1.nonword$Reversal,
       paired=TRUE, var.equal=TRUE) # Reversal doesn't affect nonwords.

# Session 2.
ltrs.aov.S2 <- ezANOVA(ltrs.summary.ID %>% filter(Session == "2"),
                       dv = Mean.D,
                       wid = ID,
                       within = .(Target, Reversal),
                       detailed = TRUE)
ltrs.aov.S2
#           Effect DFn DFd       SSn       SSd         F            p p<.05        ges
# 1     (Intercept)   1  72 668.17823  70.13576 685.93868 1.548708e-38     * 0.68338491
# 2          Target   1  72 436.57889 130.55469 240.77021 1.155301e-24     * 0.58510979
# 3        Reversal   1  72  42.41143  45.95772  66.44417 8.000330e-12     * 0.12049345
# 4 Target:Reversal   1  72  25.47021  62.92160  29.14508 8.211787e-07     * 0.07602139

# Post-hocs for interaction.
ltrs.posthoc.S2.word <- ltrs.summary.ID %>% filter(Session == 2 & Target == "Word")
ltrs.posthoc.S2.nonword <- ltrs.summary.ID %>% filter(Session == 2 & Target == "Nonword")
ltrs.posthoc.S2.40 <- ltrs.summary.ID %>% filter(Session == 2 & Reversal == "40")
ltrs.posthoc.S2.60 <- ltrs.summary.ID %>% filter(Session == 2 & Reversal == "60")

t.test(ltrs.posthoc.S2.40$Mean.D ~ ltrs.posthoc.S2.40$Target,
       paired=TRUE, var.equal=TRUE) # Word effect @ 40.
t.test(ltrs.posthoc.S2.60$Mean.D ~ ltrs.posthoc.S2.60$Target,
       paired=TRUE, var.equal=TRUE) # Word effect @ 60.
t.test(ltrs.posthoc.S2.word$Mean.D ~ ltrs.posthoc.S2.word$Reversal,
       paired=TRUE, var.equal=TRUE) # Reversal affects words.
t.test(ltrs.posthoc.S2.nonword$Mean.D ~ ltrs.posthoc.S2.nonword$Reversal,
       paired=TRUE, var.equal=TRUE) # Reversal doesn't affect nonwords.

# Across sessions.
ltrs.aov.Sx <- ezANOVA(ltrs.summary.ID,
                       dv = Mean.D,
                       wid = ID,
                       within = .(Target, Reversal, Session),
                       detailed = TRUE)
ltrs.aov.Sx
#                     Effect DFn DFd          SSn       SSd            F            p p<.05          ges
# 1             (Intercept)   1  72 1.076098e+03 109.29210 7.089173e+02 5.276723e-39     * 6.294249e-01
# 2                  Target   1  72 9.691857e+02 270.21954 2.582395e+02 1.620581e-25     * 6.047053e-01
# 3                Reversal   1  72 8.388157e+01  58.58424 1.030904e+02 1.546340e-15     * 1.169185e-01
# 4                 Session   1  72 1.407957e+01  25.19042 4.024263e+01 1.749216e-08     * 2.174000e-02
# 5         Target:Reversal   1  72 7.876657e+01  67.90164 8.352072e+01 1.151946e-13     * 1.105773e-01
# 6          Target:Session   1  72 2.504318e+00  42.65678 4.227017e+00 4.341168e-02     * 3.937242e-03
# 7        Reversal:Session   1  72 2.625992e-03  25.63289 7.376124e-03 9.317967e-01       4.144837e-06
# 8 Target:Reversal:Session   1  72 3.019942e+00  34.07709 6.380704e+00 1.373699e-02     * 4.744051e-03

# Make figure 1.
Figure1 <- plot_grid(ganong.plot, pr.plot, ltrs.plot,
                     ncol=1, rel_heights = c(1/3, 1/3, 1/3),
                     labels=c("A", "B", "C"))
Figure1
pdf("Figure1.pdf", 8, 11, bg="transparent")
plot(Figure1)
dev.off()  

# Prep for individual differences analyses. -----
# Ganong.
# Extract coefficients from S1 and S2 models..
coef.ganong1 <- dplyr::add_rownames(coef(glmm.ganong.S1)[["ID"]])
coef.ganong1$ID <- coef.ganong1$rowname

coef.ganong1$ganong.Continuum.S1 <- coef.ganong1$Continuum1
coef.ganong1$ganong.VOT.S1 <- coef.ganong1$VOT.s
coef.ganong1 <- coef.ganong1 %>% 
  select(ID, ganong.Continuum.S1, ganong.VOT.S1)

coef.ganong2 <- dplyr::add_rownames(coef(glmm.ganong.S2)[["ID"]])
coef.ganong2$ID <- coef.ganong2$rowname

coef.ganong2$ganong.Continuum.S2 <- coef.ganong2$Continuum1
coef.ganong2$ganong.VOT.S2 <- coef.ganong2$VOT.s
coef.ganong2 <- coef.ganong2 %>% 
  select(ID, ganong.Continuum.S2, ganong.VOT.S2)

# For calculating an acoustic-phonetic score that is completely independent from
# the Ganong effect, run separate models for each continuum at each session.
# S1.
glmm.VOT.S1.gift <- glmer(Response ~ (VOT.s) + (VOT.s|ID),
                          data=ganong %>% filter(Session == "1" & N.Sessions == 2 & Continuum == "gift"), # gift continuum.
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(glmm.VOT.S1.gift)

glmm.VOT.S1.giss <- glmer(Response ~ (VOT.s) + (VOT.s|ID),
                          data=ganong %>% filter(Session == "1" & N.Sessions == 2 & Continuum == "giss"), # giss continuum.
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(glmm.VOT.S1.giss)

coef.glmm.VOT.S1.gift <- dplyr::add_rownames(coef(glmm.VOT.S1.gift)[["ID"]])
coef.glmm.VOT.S1.gift$ID <- coef.glmm.VOT.S1.gift$rowname

coef.glmm.VOT.S1.gift$ganong.VOT.S1.gift <- coef.glmm.VOT.S1.gift$VOT.s
coef.glmm.VOT.S1.gift <- coef.glmm.VOT.S1.gift %>% 
  select(ID, ganong.VOT.S1.gift)

coef.glmm.VOT.S1.giss <- dplyr::add_rownames(coef(glmm.VOT.S1.giss)[["ID"]])
coef.glmm.VOT.S1.giss$ID <- coef.glmm.VOT.S1.giss$rowname

coef.glmm.VOT.S1.giss$ganong.VOT.S1.giss <- coef.glmm.VOT.S1.giss$VOT.s
coef.glmm.VOT.S1.giss <- coef.glmm.VOT.S1.giss %>% 
  select(ID, ganong.VOT.S1.giss)

# S2.
glmm.VOT.S2.gift <- glmer(Response ~ (VOT.s) + (VOT.s|ID),
                          data=ganong %>% filter(Session == "2" & N.Sessions == 2 & Continuum == "gift"), # gift continuum.
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(glmm.VOT.S2.gift)

glmm.VOT.S2.giss <- glmer(Response ~ (VOT.s) + (VOT.s|ID),
                          data=ganong %>% filter(Session == "2" & N.Sessions == 2 & Continuum == "giss"), # giss continuum.
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(glmm.VOT.S2.giss)

coef.glmm.VOT.S2.gift <- dplyr::add_rownames(coef(glmm.VOT.S2.gift)[["ID"]])
coef.glmm.VOT.S2.gift$ID <- coef.glmm.VOT.S2.gift$rowname

coef.glmm.VOT.S2.gift$ganong.VOT.S2.gift <- coef.glmm.VOT.S2.gift$VOT.s
coef.glmm.VOT.S2.gift <- coef.glmm.VOT.S2.gift %>% 
  select(ID, ganong.VOT.S2.gift)

coef.glmm.VOT.S2.giss <- dplyr::add_rownames(coef(glmm.VOT.S2.giss)[["ID"]])
coef.glmm.VOT.S2.giss$ID <- coef.glmm.VOT.S2.giss$rowname

coef.glmm.VOT.S2.giss$ganong.VOT.S2.giss <- coef.glmm.VOT.S2.giss$VOT.s
coef.glmm.VOT.S2.giss <- coef.glmm.VOT.S2.giss %>% 
  select(ID, ganong.VOT.S2.giss)

# Bring together; tidy the environment.
ganong.ind <- full_join(coef.ganong1, coef.glmm.VOT.S1.gift)
ganong.ind <- full_join(ganong.ind, coef.glmm.VOT.S1.giss)
ganong.ind <- full_join(ganong.ind, coef.ganong2)
ganong.ind <- full_join(ganong.ind, coef.glmm.VOT.S2.gift)
ganong.ind <- full_join(ganong.ind, coef.glmm.VOT.S2.giss)
rm(coef.ganong1, coef.glmm.VOT.S1.gift, coef.glmm.VOT.S1.giss,
   coef.ganong2, coef.glmm.VOT.S2.gift, coef.glmm.VOT.S2.giss)

# Add mean VOT slope across continua for each session.
ganong.ind$ganong.VOT.S1.mean <- (ganong.ind$ganong.VOT.S1.gift + ganong.ind$ganong.VOT.S1.giss) / 2
ganong.ind$ganong.VOT.S2.mean <- (ganong.ind$ganong.VOT.S2.gift + ganong.ind$ganong.VOT.S2.giss) / 2

# PR.
# Extract coefficients.
coef.pr1 <- dplyr::add_rownames(coef(pr.lmer.S1)[["ID"]])
coef.pr1$ID <- coef.pr1$rowname

coef.pr1$pr.Type.S1 <- coef.pr1$Type1
coef.pr1$pr.Block.S1 <- coef.pr1$Block1
coef.pr1$pr.TypeXBlock.S1 <- coef.pr1$`Type1:Block1`

coef.pr1 <- coef.pr1 %>% 
  select(ID, pr.Type.S1, pr.Block.S1, pr.TypeXBlock.S1)

coef.pr2 <- dplyr::add_rownames(coef(pr.lmer.S2)[["ID"]])
coef.pr2$ID <- coef.pr2$rowname

coef.pr2$pr.Type.S2 <- coef.pr2$Type1
coef.pr2$pr.Block.S2 <- coef.pr2$Block1
coef.pr2$pr.TypeXBlock.S2 <- coef.pr2$`Type1:Block1`

coef.pr2 <- coef.pr2 %>% 
  select(ID, pr.Type.S2, pr.Block.S2, pr.TypeXBlock.S2)

pr.ind <- full_join(coef.pr1, coef.pr2)
rm(coef.pr1, coef.pr2)

# LTRS.
# Collapse over reversal window, as in Ishida et al.
ltrs.ind <- ltrs.summary.ID %>% 
  group_by(ID, Session, Target) %>%
  summarise(D.Prime = mean(Mean.D)) %>% 
  spread(Target, D.Prime) %>% 
  mutate(LTRS = Word - Nonword)

ltrs.ind$Nonword <- NULL
ltrs.ind$Word <- NULL

ltrs.ind <- ltrs.ind %>% 
  spread(Session, LTRS) %>% 
  select(ID,
         ltrs.S1 = `1`,
         ltrs.S2 = `2`)

# Now that the lexical effect is coded for each session, add d' for word
# and nonword conditions separately for use in the phonetics vs. lexical analyses.
ltrs.xy.S1 <- ltrs.summary.ID %>% 
  filter(Session == "1") %>% 
  group_by(ID, Target) %>% 
  summarise(Mean.D = mean(Mean.D))
ltrs.xy.S1 <- ltrs.xy.S1 %>%
  spread(Target, Mean.D)
ltrs.xy.S1 <- ltrs.xy.S1 %>%
  select(ID,
         ltrs.Word.S1 = Word,
         ltrs.Nonword.S1 = Nonword)

ltrs.xy.S2 <- ltrs.summary.ID %>% 
  filter(Session == "2") %>% 
  group_by(ID, Target) %>% 
  summarise(Mean.D = mean(Mean.D))
ltrs.xy.S2 <- ltrs.xy.S2 %>%
  spread(Target, Mean.D)
ltrs.xy.S2 <- ltrs.xy.S2 %>%
  select(ID,
         ltrs.Word.S2 = Word,
         ltrs.Nonword.S2 = Nonword)

# Bring together.
correlations <- full_join(ganong.ind, pr.ind)
correlations <- full_join(correlations, ltrs.ind)
correlations <- full_join(correlations, ltrs.xy.S1)
correlations <- full_join(correlations, ltrs.xy.S2)
rm(ltrs.xy.S1, ltrs.xy.S2)

# Add some helper vectors that will be used in plots.
correlations$Facet.Ganong <- "Ganong"
correlations$Facet.PR <- "Phonemic restoration"
correlations$Facet.LTRS <- "LTRS"

# Lexical effect across sessions for each task. -----
cor.test(correlations$ganong.Continuum.S1, correlations$ganong.Continuum.S2)
ganong.Sx.plot <- ggplot(data = correlations, aes(x=ganong.Continuum.S1, y=ganong.Continuum.S2)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#00846b") +
  geom_point() +
  labs(x = "Session 1", y = "Session 2", title="Ganong") +
  scale_x_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.72, p < 0.001",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(panel.grid.major.y=element_line(colour="grey"))
ganong.Sx.plot

cor.test(correlations$pr.TypeXBlock.S1, correlations$pr.TypeXBlock.S2)
pr.Sx.plot <- ggplot(data = correlations, aes(x=pr.TypeXBlock.S1, y=pr.TypeXBlock.S2)) +
  theme_cowplot(font_size=20) + 
  geom_point() +
  geom_smooth(method="lm", color="#00846b") +
  labs(x = "Session 1", y = "Session 2", title="Phonemic restoration") +
  scale_x_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.37, p = 0.001",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(panel.grid.major.y=element_line(colour="grey"))
pr.Sx.plot

cor.test(correlations$ltrs.S1, correlations$ltrs.S2)
ltrs.Sx.plot <- ggplot(data = correlations, aes(x=ltrs.S1, y=ltrs.S2)) +
  theme_cowplot(font_size=20) + 
  geom_point() +
  geom_smooth(method="lm", color="#00846b") +
  labs(x = "Session 1", y = "Session 2", title="LTRS") +
  scale_x_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  scale_y_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.74, p < 0.001",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(panel.grid.major.y=element_line(colour="grey"))
ltrs.Sx.plot

# Confirm that the patterns still hold if limiting to >= 0 lexical reliance scores.
cor.test(~ ganong.Continuum.S1 + ganong.Continuum.S2, alternative="two.sided", data=correlations,
         subset=(ganong.Continuum.S1 >=0 & ganong.Continuum.S2 >=0)) # n = 62, r = 0.71, p < 0.001

cor.test(~ pr.TypeXBlock.S1 + pr.TypeXBlock.S2, alternative="two.sided", data=correlations,
         subset=(pr.TypeXBlock.S1 >=0 & pr.TypeXBlock.S2 >=0)) # n = 50, r = 0.40, p = 0.004

cor.test(~ ltrs.S1 + ltrs.S2, alternative="two.sided", data=correlations,
         subset=(ltrs.S1 >=0 & ltrs.S2 >=0)) # n = 70, r = 0.74, p < 0.001

# Make figure 2.
Figure2 <- plot_grid(ganong.Sx.plot, pr.Sx.plot, ltrs.Sx.plot,
                     nrow=1, align="v", rel_widths = c(1/3, 1/3, 1/3))
Figure2
pdf("Figure2.pdf", 15, 5, bg="transparent")
plot(Figure2)
dev.off()  

# Finally test whether the test-retest association for Ganong and LTRS are greater than the correlation
# for PR; following procedure outlined here: https://cran.r-project.org/web/packages/cocor/cocor.pdf
# cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n,
#                             alternative = "two.sided", test = "all", alpha = 0.05,
#                             conf.level = 0.95, null.value = 0, data.name = NULL,
#                             var.labels = NULL, return.htest = FALSE)
# r.jk and r.hm == the two correlations you want to compare.
# Ganong vs. PR.
cor.test(correlations$ganong.Continuum.S1, correlations$ganong.Continuum.S2) # jk
cor.test(correlations$pr.TypeXBlock.S1, correlations$pr.TypeXBlock.S2) # hm
cor.test(correlations$ganong.Continuum.S1, correlations$pr.TypeXBlock.S1) # jh
cor.test(correlations$ganong.Continuum.S1, correlations$pr.TypeXBlock.S2) # jm
cor.test(correlations$ganong.Continuum.S2, correlations$pr.TypeXBlock.S1) # kh
cor.test(correlations$ganong.Continuum.S2, correlations$pr.TypeXBlock.S2) # km

cocor.dep.groups.nonoverlap(0.72, 0.37, 0.01, -0.21, -0.06, -0.11, 73,
                            alternative = "two.sided", test = "all", alpha = 0.05,
                            conf.level = 0.95, null.value = 0, data.name = c("correlations"),
                            var.labels = c("ganong.Continuum.S1", "ganong.Continuum.S2",
                                           "pr.TypeXBlock.S1", "pr.TypeXBlock.S2"),
                            return.htest = FALSE)
# z = 3.0504, p-value = 0.0023

# LTRS vs. PR.
cor.test(correlations$ltrs.S1, correlations$ltrs.S2) # jk
cor.test(correlations$pr.TypeXBlock.S1, correlations$pr.TypeXBlock.S2) # hm
cor.test(correlations$ltrs.S1, correlations$pr.TypeXBlock.S1) # jh
cor.test(correlations$ltrs.S1, correlations$pr.TypeXBlock.S2) # jm
cor.test(correlations$ltrs.S2, correlations$pr.TypeXBlock.S1) # kh
cor.test(correlations$ltrs.S2, correlations$pr.TypeXBlock.S2) # km

cocor.dep.groups.nonoverlap(0.74, 0.37, -0.00, -0.08, -0.00, 0.09, 73,
                            alternative = "two.sided", test = "all", alpha = 0.05,
                            conf.level = 0.95, null.value = 0, data.name = c("correlations"),
                            var.labels = c("ltrs.S1", "ltrs.S2",
                                           "pr.TypeXBlock.S1", "pr.TypeXBlock.S2"),
                            return.htest = FALSE)
# z = 3.2601, p-value = 0.0011

# Within each session, across tasks.
# Session 1.
cor.test(correlations$ltrs.S1, correlations$pr.TypeXBlock.S1)
ltrs.pr.S1.plot <- ggplot(data = correlations, aes(x=ltrs.S1, y=pr.TypeXBlock.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "LTRS", y = "Phonemic restoration", title="Session 1") +
  scale_x_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.00, p = 0.992",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
ltrs.pr.S1.plot

cor.test(correlations$ganong.Continuum.S1, correlations$ltrs.S1)
ganong.ltrs.S1.plot <- ggplot(data = correlations, aes(x=ganong.Continuum.S1, y=ltrs.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "Ganong", y = "LTRS", title="") +
  scale_x_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.17, p = 0.159",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
ganong.ltrs.S1.plot

cor.test(correlations$ganong.Continuum.S1, correlations$pr.TypeXBlock.S1)
ganong.pr.S1.plot <- ggplot(data = correlations, aes(x=ganong.Continuum.S1, y=pr.TypeXBlock.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "Ganong", y = "Phonemic restoration", title="") +
  scale_x_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.01, p = 0.963",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
ganong.pr.S1.plot

# Session 2.
cor.test(correlations$ltrs.S2, correlations$pr.TypeXBlock.S2)
ltrs.pr.S2.plot <- ggplot(data = correlations, aes(x=ltrs.S2, y=pr.TypeXBlock.S2)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "LTRS", y = "Phonemic restoration", title="Session 2") +
  scale_x_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.09, p = 0.470",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
ltrs.pr.S2.plot

cor.test(correlations$ganong.Continuum.S2, correlations$ltrs.S2)
ganong.ltrs.S2.plot <- ggplot(data = correlations, aes(x=ganong.Continuum.S2, y=ltrs.S2)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "Ganong", y = "LTRS", title="") +
  scale_x_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.46, p < 0.001",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
ganong.ltrs.S2.plot

cor.test(correlations$ganong.Continuum.S2, correlations$pr.TypeXBlock.S2)
ganong.pr.S2.plot <- ggplot(data = correlations, aes(x=ganong.Continuum.S2, y=pr.TypeXBlock.S2)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "Ganong", y = "Phonemic restoration", title="") +
  scale_x_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = -0.11, p = 0.342",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
ganong.pr.S2.plot

# Confirm that the same patterns hold when limiting to >= 0 lexical reliance scores.
# S1.
cor.test(~ ltrs.S1 + pr.TypeXBlock.S1, alternative="two.sided", data=correlations,
         subset=(ltrs.S1 >=0 & pr.TypeXBlock.S1 >=0)) # n = 53, r = -0.07, p = 0.596

cor.test(~ ganong.Continuum.S1 + ltrs.S1, alternative="two.sided", data=correlations,
         subset=(ganong.Continuum.S1 >=0 & ltrs.S1 >=0)) # n = 70, r = 0.15, p = 0.209

cor.test(~ ganong.Continuum.S1 + pr.TypeXBlock.S1, alternative="two.sided", data=correlations,
         subset=(ganong.Continuum.S1 >=0 & pr.TypeXBlock.S1 >=0)) # n = 56, r = -0.08, p = 0.550

# S2.
cor.test(~ ltrs.S2 + pr.TypeXBlock.S2, alternative="two.sided", data=correlations,
         subset=(ltrs.S2 >=0 & pr.TypeXBlock.S2 >=0)) # n = 63, r = 0.10, p = 0.428

cor.test(~ ganong.Continuum.S2 + ltrs.S2, alternative="two.sided", data=correlations,
         subset=(ganong.Continuum.S2 >=0 & ltrs.S2 >=0)) # n = 61, r = 0.41, p = 0.001

cor.test(~ ganong.Continuum.S2 + pr.TypeXBlock.S2, alternative="two.sided", data=correlations,
         subset=(ganong.Continuum.S2 >=0 & pr.TypeXBlock.S2 >=0)) # n = 53, r = -0.11, p = 0.416


# Build Figure 3.
Figure3.R1 <- plot_grid(ltrs.pr.S1.plot, ganong.ltrs.S1.plot, ganong.pr.S1.plot,
                        nrow=1, rel_widths = c(1/3, 1/3, 1/3))
Figure3.R2 <- plot_grid(ltrs.pr.S2.plot, ganong.ltrs.S2.plot, ganong.pr.S2.plot,
                        nrow=1, rel_widths = c(1/3, 1/3, 1/3))
Figure3 <- plot_grid(Figure3.R1, Figure3.R2, ncol=1)
Figure3
pdf("Figure3.pdf", 15, 10, bg="transparent")
plot(Figure3)
dev.off() 

# Within each session, within each task, phonetic (+lexical) vs. lexical. -----
# Session 1.
cor.test(correlations$ganong.VOT.S1.mean, correlations$ganong.Continuum.S1)
ganong.xy.S1.plot <- ggplot(data = correlations,
                            aes(x=ganong.VOT.S1, y=ganong.Continuum.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#6a0084") +
  geom_point() +
  scale_x_continuous(limits=c(0, 6), breaks=c(0, 2, 4, 6)) +
  scale_y_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  labs(x = expression("VOT ("*hat(beta)*")"), y = expression("Continuum ("*hat(beta)*")")) +
  annotate(geom="text", x=Inf, y=-Inf, label="r = -0.84, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.Ganong)
ganong.xy.S1.plot

cor.test(correlations$pr.Type.S1, correlations$pr.TypeXBlock.S1)
pr.xy.S1.plot <- ggplot(data = correlations, aes(x=pr.Type.S1, y=pr.TypeXBlock.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#6a0084") +
  geom_point() +
  scale_x_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  labs(x = expression("Type ("*hat(beta)*")"), y = expression("Type x Block ("*hat(beta)*")")) +
  annotate(geom="text", x=Inf, y=-Inf, label="r = 0.65, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.PR)
pr.xy.S1.plot

cor.test(correlations$ltrs.Nonword.S1, correlations$ltrs.Word.S1)
ltrs.xy.S1.plot <- ggplot(data = correlations, aes(x=ltrs.Nonword.S1, y=ltrs.Word.S1)) +
  theme_cowplot(font_size=20) + 
  geom_point() +
  geom_smooth(method="lm", color="#6a0084") +
  scale_x_continuous(limits=c(-3.4, 4.8), breaks=c(-3, -1.5, 0, 1.5, 3, 4.5)) +
  scale_y_continuous(limits=c(-3.4, 4.8), breaks=c(-3, -1.5, 0, 1.5, 3, 4.5)) +
  labs(x = "Nonword (d')", y = "Word (d')") +
  annotate(geom="text", x=Inf, y=-Inf, label="r = -0.48, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.LTRS)
ltrs.xy.S1.plot

# xy.S1.plot <- plot_grid(ganong.xy.S1.plot, pr.xy.S1.plot, ltrs.xy.S1.plot, ncol=1)
# xy.S1.plot
# pdf("xy.S1.plot.pdf", 5, 15, bg="transparent")
# plot(xy.S1.plot)
# dev.off() 

# Within each session, within each task, phonetic (+lexical) vs. lexical.
# Session 2.
cor.test(correlations$ganong.VOT.S2.mean, correlations$ganong.Continuum.S2)
ganong.xy.S2.plot <- ggplot(data = correlations, aes(x=ganong.VOT.S2, y=ganong.Continuum.S2)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#6a0084") +
  geom_point() +
  scale_x_continuous(limits=c(0, 6), breaks=c(0, 2, 4, 6)) +
  scale_y_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  labs(x = expression("VOT ("*hat(beta)*")"), y = expression("Continuum ("*hat(beta)*")")) +
  annotate(geom="text", x=Inf, y=-Inf, label="r = -0.50, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.Ganong)
ganong.xy.S2.plot

cor.test(correlations$pr.Type.S2, correlations$pr.TypeXBlock.S2)
pr.xy.S2.plot <- ggplot(data = correlations, aes(x=pr.Type.S2, y=pr.TypeXBlock.S2)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#6a0084") +
  geom_point() +
  scale_x_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  labs(x = expression("Type ("*hat(beta)*")"), y = expression("Type x Block ("*hat(beta)*")")) +
  annotate(geom="text", x=Inf, y=-Inf, label="r = 0.88, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.PR)
pr.xy.S2.plot

cor.test(correlations$ltrs.Nonword.S2, correlations$ltrs.Word.S2)
ltrs.xy.S2.plot <- ggplot(data = correlations, aes(x=ltrs.Nonword.S2, y=ltrs.Word.S2)) +
  theme_cowplot(font_size=20) + 
  geom_point() +
  geom_smooth(method="lm", color="#6a0084") +
  scale_x_continuous(limits=c(-3.4, 4.8), breaks=c(-3, -1.5, 0, 1.5, 3, 4.5)) +
  scale_y_continuous(limits=c(-3.4, 4.8), breaks=c(-3, -1.5, 0, 1.5, 3, 4.5)) +
  labs(x = "Nonword (d')", y = "Word (d')") +
  annotate(geom="text", x=Inf, y=-Inf, label="r = -0.30, p = 0.010",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.LTRS)
ltrs.xy.S2.plot

# xy.S2.plot <- plot_grid(ganong.xy.S2.plot, pr.xy.S2.plot, ltrs.xy.S2.plot, ncol=1)
# xy.S2.plot
# pdf("xy.S2.plot.pdf", 5, 15, bg="transparent")
# plot(xy.S2.plot)
# dev.off() 

# Confirm same patterns with scores >= 0.
# S1.
cor.test(~ ganong.VOT.S1.mean + ganong.Continuum.S1, alternative="two.sided", data=correlations,
         subset=(ganong.VOT.S1.mean >=0 & ganong.Continuum.S1 >=0)) # n = 73, r = -0.84, p < 0.001

cor.test(~ pr.Type.S1 + pr.TypeXBlock.S1, alternative="two.sided", data=correlations,
         subset=(pr.Type.S1 >=0 & pr.TypeXBlock.S1 >=0)) # n = 52, r = 0.76, p < 0.001

cor.test(~ ltrs.Nonword.S1 + ltrs.Word.S1, alternative="two.sided", data=correlations,
         subset=(ltrs.S1 >=0)) # n = 70, r = -0.42, p < 0.001

# S2.
cor.test(~ ganong.VOT.S2.mean + ganong.Continuum.S2, alternative="two.sided", data=correlations,
         subset=(ganong.VOT.S2.mean >=0 & ganong.Continuum.S2 >=0)) # n = 62, r = -0.44, p < 0.001

cor.test(~ pr.Type.S2 + pr.TypeXBlock.S2, alternative="two.sided", data=correlations,
         subset=(pr.Type.S2 >=0 & pr.TypeXBlock.S2 >=0)) # n = 64, r = 0.86, p < 0.001

cor.test(~ ltrs.Nonword.S2 + ltrs.Word.S2, alternative="two.sided", data=correlations,
         subset=(ltrs.S1 >=0)) # n = 70, r = -0.29, p = 0.017

# Pull out representative participants from each task at Session 1 to illustrate
# relationships between phonetic/lexical trade-off.
# Ganong.
ganong.ex <- ganong.summary.ID %>% 
  filter(ID == "TIMETIME.068" | ID == "TIMETIME.097" | ID == "TIMETIME.119")
ganong.ex$ID <- factor(ganong.ex$ID, levels=c("TIMETIME.068", "TIMETIME.097", "TIMETIME.119"))
labels.ganong.ex <- c(TIMETIME.068 = "Subject 68",
                      TIMETIME.097 = "Subject 97",
                      TIMETIME.119 = "Subject 119")
                      
ganong.ex.plot <- ggplot(ganong.ex %>% filter(Session == 1),
                         aes(x=VOT, y=Mean.K, group=Continuum, color=Continuum)) +
  theme_cowplot(font_size=20) +
  geom_point() +
  geom_line(aes(linetype=Continuum), size=0.5, show.legend = FALSE) +
  ylim(0, 1) +
  labs(x="VOT (ms)", y = "p(k)") +
  scale_color_manual(values=c("#00846b", "#6a0084"),
                     labels = c(expression(italic("giss - kiss")),
                                expression(italic("gift - kift")))) +
  theme(legend.text.align = 0) +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  facet_wrap(. ~ ID, labeller=labeller(ID = labels.ganong.ex))
ganong.ex.plot

# PR.
pr.ex <- pr.summary.ID %>% 
  filter(ID == "TIMETIME.096" | ID == "TIMETIME.106" | ID == "TIMETIME.060")
pr.ex$ID <- factor(pr.ex$ID, levels=c("TIMETIME.096", "TIMETIME.106", "TIMETIME.060"))
labels.pr.ex <- c(TIMETIME.096 = "Subject 96",
                  TIMETIME.106 = "Subject 106",
                  TIMETIME.060 = "Subject 60")

pr.ex.plot <- ggplot(pr.ex %>% filter(Session == 1), aes(x=Block, y=Mean.Rating, fill = Type)) +
  theme_cowplot(font_size=20) +
  geom_bar(stat="identity", position = position_dodge(width=0.9), width=0.9) +
  coord_cartesian(ylim = c(4.5, 8)) +
  scale_fill_manual(values=c("#00846b", "#6a0084")) +
  labs(y = "Similarity rating") +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  theme(legend.text.align = 0) +
  theme(panel.grid.major.y = element_line(colour="grey")) + 
  facet_wrap(. ~ ID, labeller=labeller(ID = labels.pr.ex))
pr.ex.plot

# LTRS.
ltrs.ex <- ltrs.summary.ID %>% 
  filter(ID == "TIMETIME.018" | ID == "TIMETIME.082" | ID == "TIMETIME.001")
ltrs.ex$ID <- factor(ltrs.ex$ID, levels=c("TIMETIME.018", "TIMETIME.082", "TIMETIME.001"))
labels.ltrs.ex <- c(TIMETIME.018 = "Subject 18",
                    TIMETIME.082 = "Subject 82",
                    TIMETIME.001 = "Subject 1")

ltrs.ex.plot <- ggplot(ltrs.ex %>% filter(Session == 1),
                       aes(x=Reversal, y=Mean.D, group=Target, color=Target)) +
  theme_cowplot(font_size=20) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=c("#00846b", "#6a0084")) +
  ylim(-3.3, 5) +
  labs(y="Sensitivity (d')", x = "Reversal window (ms)") +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  facet_wrap(. ~ ID, labeller=labeller(ID = labels.ltrs.ex))
ltrs.ex.plot 

# Build figure 4.
Figure4.R1 <- plot_grid(ganong.xy.S1.plot, ganong.ex.plot, ganong.xy.S2.plot,
                        nrow=1, rel_widths = c(0.25, 0.5, 0.25),
                        labels=c("S1", "", "S2"), label_size = 20)
Figure4.R2 <- plot_grid(pr.xy.S1.plot, pr.ex.plot, pr.xy.S2.plot,
                        nrow=1, rel_widths = c(0.25, 0.5, 0.25),
                        labels=c("S1", "", "S2"), label_size = 20)
Figure4.R3 <- plot_grid(ltrs.xy.S1.plot, ltrs.ex.plot, ltrs.xy.S2.plot,
                        nrow=1, rel_widths = c(0.25, 0.5, 0.25),
                        labels=c("S1", "", "S2"), label_size = 20)
Figure4 <- plot_grid(Figure4.R1, Figure4.R2, Figure4.R3,
                     ncol=1)
Figure4
pdf("Figure4.pdf", 16, 12, bg="transparent")
plot(Figure4)
dev.off() 

# Reanalysis/comparison to Ishida et al. (2016). -----
# Data retrieved from the supplementary material provided at:
# https://www.sciencedirect.com/science/article/pii/S0010027716300634?casa_token=LJVHDUbEsLMAAAAA:rfdlrP4TgdGlai_0NyHMeVMFP73NYcK4gAAt34F2FOhV8FZI43dvmX1WWoeIEVwUK1IV3TZy
# Import Ishida et al. data; these are the values shown in the CORREL sheet of the workbook.
ishida <- read.csv("IshidaETAL.csv")

# Confirm correlation reported in Ishida et al. to make sure we
# brought in the right data.
cor.test(ishida$LTRS, ishida$PR) # r = 0.43, p = 0.002.

# Build a dataframe that collates across experiments.
ishida$Dataset <- "Ishida et al. (2016)"

# Prepare LTRS from current session 1 and session 2.
ltrs.ind.S1 <- ltrs.ind %>% 
  select(ID, LTRS = ltrs.S1) %>% 
  mutate(Dataset = "Session 1")
ltrs.ind.S2 <- ltrs.ind %>% 
  select(ID, LTRS = ltrs.S2) %>% 
  mutate(Dataset = "Session 2")
ltrs.temp <- full_join(ltrs.ind.S1, ltrs.ind.S2)
rm(ltrs.ind.S1, ltrs.ind.S2)

# Calculate by-subject lexical effect for PR following Ishida et al. methods.
# As described in the paper: "First, we took the difference between the mean score of words with a noise-added phoneme
# and of words with a noise-replaced phoneme (words/added – words/replaced).
# Next, we did the same thing for pseudowords (pseudowords/added – pseudowords/replaced).
# Finally, we took the difference between the difference score for words and the difference
# score for pseudowords." Based on personal correspondence, it was actually replaced - added,
# which matches data in supplemental materials.
pr.ind.S1 <- pr.summary.ID %>% 
  filter(Session == 1) %>% 
  spread(Type, Mean.Rating)
pr.ind.S1$ReplacedMINUSAdded <- pr.ind.S1$Replaced - pr.ind.S1$Added
pr.ind.S1$Added <- NULL
pr.ind.S1$Replaced <- NULL
pr.ind.S1 <- pr.ind.S1 %>% 
  spread(Block, ReplacedMINUSAdded)
pr.ind.S1$PR <- pr.ind.S1$Word - pr.ind.S1$Nonword
pr.ind.S1$Word <- NULL
pr.ind.S1$Nonword <- NULL
pr.ind.S1$Session <- NULL
pr.ind.S1$Dataset <- "Session 1"

pr.ind.S2 <- pr.summary.ID %>% 
  filter(Session == 2) %>% 
  spread(Type, Mean.Rating)
pr.ind.S2$ReplacedMINUSAdded <- pr.ind.S2$Replaced - pr.ind.S2$Added
pr.ind.S2$Added <- NULL
pr.ind.S2$Replaced <- NULL
pr.ind.S2 <- pr.ind.S2 %>% 
  spread(Block, ReplacedMINUSAdded)
pr.ind.S2$PR <- pr.ind.S2$Word - pr.ind.S2$Nonword
pr.ind.S2$Word <- NULL
pr.ind.S2$Nonword <- NULL
pr.ind.S2$Session <- NULL
pr.ind.S2$Dataset <- "Session 2"

pr.temp <- full_join(pr.ind.S1, pr.ind.S2)
rm(pr.ind.S1, pr.ind.S2)

# Collate to a single data frame.
collated <- full_join(pr.temp, ltrs.temp)
collated <- full_join(collated, ishida)
collated <- collated %>% 
  select(Dataset, ID, LTRS, PR)
rm(pr.temp, ltrs.temp, ishida)

# Perform correlations within each data set.
cor.test(~ LTRS + PR, alternative="two.sided", data=collated,
         subset=(Dataset == "Ishida et al. (2016)")) # r = 0.43, p = 0.002

cor.test(~ LTRS + PR, alternative="two.sided", data=collated,
         subset=(Dataset == "Session 1")) # r = 0.12, p = 0.308

cor.test(~ LTRS + PR, alternative="two.sided", data=collated,
         subset=(Dataset == "Session 2")) # r = 0.17, p = 0.148

# Perform correlations on LTRS and PR scores greater than or equal to zero.
# Perform correlations within each data set.
cor.test(~ LTRS + PR, alternative="two.sided", data=collated,
         subset=(Dataset == "Ishida et al. (2016)" & LTRS >= 0 & PR >= 0)) # n = 31, r = -0.07, p = 0.695
round((31/52 * 100), digits=0) # 60%

cor.test(~ LTRS + PR, alternative="two.sided", data=collated,
         subset=(Dataset == "Session 1" & LTRS >= 0 & PR >= 0)) # n = 53, r = -0.20, p = 0.145
round((53/73 * 100), digits=0) # 73%

cor.test(~ LTRS + PR, alternative="two.sided", data=collated,
         subset=(Dataset == "Session 2" & LTRS >= 0 & PR >= 0)) # n = 46, r = 0.18, p = 0.219
round((46/73 * 100), digits=0) # 63%

# Build figure 5.
# A helper for printing correlations to plots.
labels.collated <- data.frame(Dataset = c("Ishida et al. (2016)", "Session 1", "Session 2"),
                   Sample.Full = c("n = 52, r = 0.43, p = 0.002",
                                   "n = 73, r = 0.12, p = 0.308",
                                   "n = 73, r = 0.17, p = 0.148"),
                   Sample.Subset = c("n = 31, r = -0.07, p = 0.695",
                                   "n = 53, r = -0.20, p = 0.145",
                                   "n = 46, r = 0.18, p = 0.219"))

Figure5A <- ggplot(data = collated, aes(x=LTRS, y=PR)) +
  theme_cowplot(font_size=20) + 
  annotate("rect", xmin = 0, xmax = 7.5, ymin = 0, ymax = 2.5,
           alpha = .1, fill = "#FDE725FF") +
  geom_point() +
  geom_smooth(method="lm", color="#6a0084") +
  scale_x_continuous(limits=c(-1.5, 7.5), breaks=c(-1, 0, 1, 2, 3, 4, 5, 6, 7)) +
  scale_y_continuous(limits=c(-1.5, 2.5), breaks=c(-1, 0, 1, 2)) +
  labs(x = "LTRS", y = "Phonemic restoration") +
  geom_text(data = labels.collated, aes(x = Inf, y = -Inf, label = Sample.Full),
            hjust=1, vjust=-.8, size=6) +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Dataset)
Figure5A

Figure5B <- ggplot(data = collated %>% filter(LTRS >= 0 & PR >= 0), aes(x=LTRS, y=PR)) +
  theme_cowplot(font_size=20) + 
  annotate("rect", xmin = 0, xmax = 7.5, ymin = 0, ymax = 2.5,
           alpha = .1, fill = "#FDE725FF") +
  geom_point() +
  geom_smooth(method="lm", color="#6a0084") +
  scale_x_continuous(limits=c(-1.5, 7.5), breaks=c(-1, 0, 1, 2, 3, 4, 5, 6, 7)) +
  scale_y_continuous(limits=c(-1.5, 2.5), breaks=c(-1, 0, 1, 2)) +
  labs(x = "LTRS", y = "Phonemic restoration") +
  geom_text(data = labels.collated, aes(x = Inf, y = -Inf, label = Sample.Subset),
            hjust=1, vjust=-.8, size=6) +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Dataset)
Figure5B

# Collate to figure 5.
Figure5 <- plot_grid(Figure5A, Figure5B, ncol=1, labels=c("A", "B"))
pdf("Figure5.pdf", 12, 9, bg="transparent")
plot(Figure5)
dev.off() 

# Analyses for supplementary material; full sample (n = 120) at session 1. -----
# Supplement; Ganong. -----
# Summary by ID.
supp.ganong.summary.ID <- ganong %>%
  filter(Session == 1) %>% 
  group_by(ID, Session, Continuum, VOT) %>%
  summarise(Mean.K = mean(Response)) 
supp.ganong.summary.ID <- droplevels(supp.ganong.summary.ID) # Confirm n = 120.

# Summary over ID.
supp.ganong.summary <- supp.ganong.summary.ID %>%
  group_by(Session, Continuum, VOT) %>%
  summarise(K.Mean = mean(Mean.K),
            K.SE = sd(Mean.K)/(sqrt(length(Mean.K))))

# Plot.
labels.Session <- c(`1` = "Session 1", `2` = "Session 2")
supp.ganong.plot <- ggplot(supp.ganong.summary, aes(x=VOT, y=K.Mean, group=Continuum, color=Continuum)) +
  theme_cowplot(font_size=20) +
  geom_point() +
  geom_line(aes(linetype=Continuum), size=0.5, show.legend = FALSE) +
  geom_errorbar(aes(y = K.Mean, ymin = K.Mean-K.SE, ymax = K.Mean+K.SE), width = 1) +
  ylim(0, 1) +
  labs(x="VOT (ms)", y = "p(k)") +
  scale_color_manual(values=c("#00846b", "#6a0084"),
                     labels = c(expression(italic("giss - kiss")),
                                expression(italic("gift - kift")))) +
  theme(legend.text.align = 0) +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  facet_wrap(. ~ Session, labeller=labeller(Session = labels.Session))
supp.ganong.plot

# Models.
# Session 1.
supp.glmm.ganong.S1 <- glmer(Response ~ (VOT.s * Continuum) + (VOT.s * Continuum|ID),
                        data=ganong %>% filter(Session == "1"), # Limit to session 1.
                        family="binomial",
                        control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(supp.glmm.ganong.S1)
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.6243     0.1173  -5.325 1.01e-07 ***
# VOT.s              2.9016     0.1152  25.195  < 2e-16 ***
# Continuum1         2.7998     0.2017  13.879  < 2e-16 ***
# VOT.s:Continuum1   0.1602     0.1460   1.097    0.273    

# Supplement; phonemic restoration. -----
# Summary by ID.
supp.pr.summary.ID <- pr %>%
  filter(Item != "Filler") %>% # Remove filler items. 
  filter(Session == 1) %>% # Limit to S1.
  group_by(ID, Session, Block, Type) %>%
  summarise(Mean.Rating = mean(Response))
supp.pr.summary.ID <- droplevels(supp.pr.summary.ID)

# Summary over ID.
supp.pr.summary <- supp.pr.summary.ID %>% 
  group_by(Session, Block, Type) %>%
  summarise(Rating.Mean = mean(Mean.Rating),
            Rating.SE = sd(Mean.Rating)/(sqrt(length(Mean.Rating))))

# Plot.
supp.pr.plot <- ggplot(supp.pr.summary, aes(x=Block, y=Rating.Mean, fill = Type)) +
  theme_cowplot(font_size=20) +
  geom_bar(stat="identity", position = position_dodge(width=0.9), width=0.9) +
  geom_errorbar(aes(y = Rating.Mean, ymin = Rating.Mean-Rating.SE, ymax = Rating.Mean+Rating.SE),
                position = position_dodge(width=0.9), width=0.4) +
  coord_cartesian(ylim = c(4.5, 8), expand = FALSE) +
  scale_fill_manual(values=c("#00846b", "#6a0084")) +
  labs(y = "Similarity rating") +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  theme(legend.text.align = 0) +
  theme(panel.grid.major.y = element_line(colour="grey")) + 
  facet_wrap(. ~ Session, labeller=labeller(Session = labels.Session))
supp.pr.plot

# ANOVAs to parallel analysis in Ishida et al.; reported in footnote.
supp.pr.aov.S1 <- ezANOVA(supp.pr.summary.ID,
                     dv = Mean.Rating,
                     wid = ID,
                     within = .(Block, Type),
                     detailed = TRUE)
supp.pr.aov.S1
#         Effect DFn DFd          SSn       SSd          F             p p<.05         ges
# 1 (Intercept)   1 119 21899.710083 413.41117 6303.81013 6.348179e-105     * 0.975983827
# 2       Block   1 119    47.313521  97.61773   57.67711  7.709757e-12     * 0.080711863
# 3        Type   1 119    13.300021  17.58373   90.00949  3.067155e-16     * 0.024085980
# 4  Block:Type   1 119     2.002083  10.27667   23.18338  4.373370e-06     * 0.003701452

# Models.
# Session 1.
supp.pr.lmer.S1 <- lmer(Response ~ Type * Block + (Type * Block|ID),
                   data = pr %>% filter(Session == "1" & Item != "Filler"),
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(supp.pr.lmer.S1)
#               Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)    6.75458    0.08507 118.99988  79.396  < 2e-16 ***
# Type1          0.33292    0.03562 123.35953   9.348 4.98e-16 ***
# Block1        -0.62792    0.08270 119.00978  -7.593 7.78e-12 ***
# Type1:Block1   0.25833    0.05830 169.87321   4.431 1.68e-05 ***

# Consistent with the direction of the beta estimate for the type x block interaction,
# the type effect gets larger from word to nonword block.
cat_plot(supp.pr.lmer.S1, pred="Block", modx="Type")

supp.pr.lmer.S1.ssType <- lmer(Response ~ Block / Type + (Type * Block|ID),
                        data = pr %>% filter(Session == "1" & Item != "Filler"),
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=800000)))
summary(supp.pr.lmer.S1.ssType)
# BlockWord:Type1      0.20375    0.03785 240.94844   5.383 1.74e-07 ***
# BlockNonword:Type1   0.46208    0.05295 120.29382   8.727 1.77e-14 ***

# Supplement; LTRS. -----
# Summary by ID.
supp.ltrs.summary.ID <- ltrs.d %>%
  filter(Session == 1) %>% 
  group_by(ID, Session, Target, Reversal) %>%
  summarise(Mean.D = mean(Dprime))
supp.ltrs.summary.ID <- droplevels(supp.ltrs.summary.ID)

# Summary over ID.
supp.ltrs.summary <- supp.ltrs.summary.ID %>%
  group_by(Session, Target, Reversal) %>%
  summarise(D.Mean = mean(Mean.D),
            D.SE = sd(Mean.D)/(sqrt(length(Mean.D))))

# Plot. 
supp.ltrs.plot <- ggplot(supp.ltrs.summary, aes(x=Reversal, y=D.Mean, group=Target, color=Target)) +
  theme_cowplot(font_size=20) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(y = D.Mean, ymin = D.Mean-D.SE, ymax = D.Mean+D.SE), width = 0.3) +
  scale_color_manual(values=c("#00846b", "#6a0084")) +
  ylim(-3.3, 5) +
  labs(y="Sensitivity (d')", x = "Reversal window (ms)") +
  theme(panel.grid.major.y = element_line(colour="grey")) +
  facet_wrap(. ~ Session, labeller=labeller(Session = labels.Session))
supp.ltrs.plot 

# ANOVAs to parallel analysis in Ishida et al.; mixed effects models not ideal given
# that sensitivity (d') is aggregated across trials.
# Session 1.
supp.ltrs.aov.S1 <- ezANOVA(supp.ltrs.summary.ID,
                       dv = Mean.D,
                       wid = ID,
                       within = .(Target, Reversal),
                       detailed = TRUE)
supp.ltrs.aov.S1
#           Effect DFn DFd       SSn       SSd        F            p p<.05       ges
# 1     (Intercept)   1 119 733.25776 101.96075 855.7967 3.513074e-56     * 0.5957794
# 2          Target   1 119 831.87797 261.39108 378.7179 8.830955e-39     * 0.6257667
# 3        Reversal   1 119  83.22204  66.66682 148.5510 1.128609e-22     * 0.1433089
# 4 Target:Reversal   1 119  83.54497  67.47731 147.3362 1.482202e-22     * 0.1437850

# Post-hocs for interaction.
supp.ltrs.posthoc.S1.word <- supp.ltrs.summary.ID %>% filter(Target == "Word")
supp.ltrs.posthoc.S1.nonword <- supp.ltrs.summary.ID %>% filter(Target == "Nonword")
supp.ltrs.posthoc.S1.40 <- supp.ltrs.summary.ID %>% filter(Reversal == "40")
supp.ltrs.posthoc.S1.60 <- supp.ltrs.summary.ID %>% filter(Reversal == "60")

t.test(supp.ltrs.posthoc.S1.word$Mean.D ~ supp.ltrs.posthoc.S1.word$Reversal,
       paired=TRUE, var.equal=TRUE) # Reversal affects words.
t.test(supp.ltrs.posthoc.S1.nonword$Mean.D ~ supp.ltrs.posthoc.S1.nonword$Reversal,
       paired=TRUE, var.equal=TRUE) # Reversal doesn't affect nonwords.
t.test(supp.ltrs.posthoc.S1.40$Mean.D ~ supp.ltrs.posthoc.S1.40$Target,
       paired=TRUE, var.equal=TRUE) # Word effect @ 40.
t.test(supp.ltrs.posthoc.S1.60$Mean.D ~ supp.ltrs.posthoc.S1.60$Target,
       paired=TRUE, var.equal=TRUE) # Word effect @ 60.

# Make figure S1.
FigureS1 <- plot_grid(supp.ganong.plot, supp.pr.plot, supp.ltrs.plot,
                     ncol=1, rel_heights = c(1/3, 1/3, 1/3),
                     labels=c("A", "B", "C"))
FigureS1
pdf("FigureS1.pdf", 5, 10, bg="transparent")
plot(FigureS1)
dev.off()  

# Supplement; prep for individual differences analyses. -----
# Ganong.
# Extract coefficients from S1 model.
supp.coef.ganong1 <- dplyr::add_rownames(coef(supp.glmm.ganong.S1)[["ID"]])
supp.coef.ganong1$ID <- supp.coef.ganong1$rowname

supp.coef.ganong1$ganong.Continuum.S1 <- supp.coef.ganong1$Continuum1
supp.coef.ganong1$ganong.VOT.S1 <- supp.coef.ganong1$VOT.s
supp.coef.ganong1 <- supp.coef.ganong1 %>% 
  select(ID, ganong.Continuum.S1, ganong.VOT.S1)

supp.ganong.ind <- supp.coef.ganong1
rm(supp.coef.ganong1)

# Separate models by continuum to derive acoustic-phonetic score.
supp.glmm.VOT.S1.gift <- glmer(Response ~ (VOT.s) + (VOT.s|ID),
                          data=ganong %>% filter(Session == "1" & Continuum == "gift"), # gift continuum.
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(supp.glmm.VOT.S1.gift)

supp.glmm.VOT.S1.giss <- glmer(Response ~ (VOT.s) + (VOT.s|ID),
                          data=ganong %>% filter(Session == "1" & Continuum == "giss"), # giss continuum.
                          family="binomial",
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 800000)))
summary(supp.glmm.VOT.S1.giss)

supp.coef.glmm.VOT.S1.gift <- dplyr::add_rownames(coef(supp.glmm.VOT.S1.gift)[["ID"]])
supp.coef.glmm.VOT.S1.gift$ID <- supp.coef.glmm.VOT.S1.gift$rowname

supp.coef.glmm.VOT.S1.gift$ganong.VOT.S1.gift <- supp.coef.glmm.VOT.S1.gift$VOT.s
supp.coef.glmm.VOT.S1.gift <- supp.coef.glmm.VOT.S1.gift %>% 
  select(ID, ganong.VOT.S1.gift)

supp.coef.glmm.VOT.S1.giss <- dplyr::add_rownames(coef(supp.glmm.VOT.S1.giss)[["ID"]])
supp.coef.glmm.VOT.S1.giss$ID <- supp.coef.glmm.VOT.S1.giss$rowname

supp.coef.glmm.VOT.S1.giss$ganong.VOT.S1.giss <- supp.coef.glmm.VOT.S1.giss$VOT.s
supp.coef.glmm.VOT.S1.giss <- supp.coef.glmm.VOT.S1.giss %>% 
  select(ID, ganong.VOT.S1.giss)

# Bring together; tidy the environment.
supp.ganong.ind <- full_join(supp.ganong.ind, supp.coef.glmm.VOT.S1.gift)
supp.ganong.ind <- full_join(supp.ganong.ind, supp.coef.glmm.VOT.S1.giss)
rm(supp.coef.glmm.VOT.S1.gift, supp.coef.glmm.VOT.S1.giss)

# Add mean VOT slope across continua for each session.
supp.ganong.ind$ganong.VOT.S1.mean <- (supp.ganong.ind$ganong.VOT.S1.gift + supp.ganong.ind$ganong.VOT.S1.giss) / 2

# PR.
# Extract coefficients.
supp.coef.pr1 <- dplyr::add_rownames(coef(supp.pr.lmer.S1)[["ID"]])
supp.coef.pr1$ID <- supp.coef.pr1$rowname

supp.coef.pr1$pr.Type.S1 <- supp.coef.pr1$Type1
supp.coef.pr1$pr.Block.S1 <- supp.coef.pr1$Block1
supp.coef.pr1$pr.TypeXBlock.S1 <- supp.coef.pr1$`Type1:Block1`

supp.coef.pr1 <- supp.coef.pr1 %>% 
  select(ID, pr.Type.S1, pr.Block.S1, pr.TypeXBlock.S1)

supp.pr.ind <- supp.coef.pr1
rm(supp.coef.pr1)

# LTRS.
# Collapse over reversal window, as in Ishida et al.
supp.ltrs.ind <- supp.ltrs.summary.ID %>% 
  group_by(ID, Session, Target) %>%
  summarise(D.Prime = mean(Mean.D)) %>% 
  spread(Target, D.Prime) %>% 
  mutate(LTRS = Word - Nonword)

supp.ltrs.ind$Nonword <- NULL
supp.ltrs.ind$Word <- NULL

supp.ltrs.ind <- supp.ltrs.ind %>% 
  select(ID, ltrs.S1 = LTRS)
supp.ltrs.ind$Session <- NULL

# Now that the lexical effect is coded for each session, add d' for word
# and nonword conditions separately for use in the phonetics vs. lexical analyses.
supp.ltrs.xy.S1 <- supp.ltrs.summary.ID %>% 
  filter(Session == "1") %>% 
  group_by(ID, Target) %>% 
  summarise(Mean.D = mean(Mean.D))
supp.ltrs.xy.S1 <- supp.ltrs.xy.S1 %>%
  spread(Target, Mean.D)
supp.ltrs.xy.S1 <- supp.ltrs.xy.S1 %>%
  select(ID,
         ltrs.Word.S1 = Word,
         ltrs.Nonword.S1 = Nonword)

# Bring together.
supp.correlations <- full_join(supp.ganong.ind, supp.pr.ind)
supp.correlations <- full_join(supp.correlations, supp.ltrs.ind)
supp.correlations <- full_join(supp.correlations, supp.ltrs.xy.S1)
rm(supp.ltrs.xy.S1)

# Add some helper vectors that will be used in plots.
supp.correlations$Facet.Ganong <- "Ganong"
supp.correlations$Facet.PR <- "Phonemic restoration"
supp.correlations$Facet.LTRS <- "LTRS"

# Within each session, across tasks.
# Session 1.
cor.test(supp.correlations$ltrs.S1, supp.correlations$pr.TypeXBlock.S1)
supp.ltrs.pr.S1.plot <- ggplot(data = supp.correlations, aes(x=ltrs.S1, y=pr.TypeXBlock.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "LTRS", y = "Phonemic restoration", title="Session 1") +
  scale_x_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.06, p = 0.548",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
supp.ltrs.pr.S1.plot

cor.test(supp.correlations$ganong.Continuum.S1, supp.correlations$ltrs.S1)
supp.ganong.ltrs.S1.plot <- ggplot(data = supp.correlations, aes(x=ganong.Continuum.S1, y=ltrs.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "Ganong", y = "LTRS", title="") +
  scale_x_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits=c(-1.5, 7.4), breaks=c(0, 2, 4, 6)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.25, p = 0.006",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
supp.ganong.ltrs.S1.plot

cor.test(supp.correlations$ganong.Continuum.S1, supp.correlations$pr.TypeXBlock.S1)
supp.ganong.pr.S1.plot <- ggplot(data = supp.correlations, aes(x=ganong.Continuum.S1, y=pr.TypeXBlock.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#FDE725FF") +
  geom_point() +
  labs(x = "Ganong", y = "Phonemic restoration", title="") +
  scale_x_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  annotate(geom="text", x=-Inf, y=Inf, label="r = 0.01, p = 0.953",
           color="black", hjust=-.1, vjust=1, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey"))
supp.ganong.pr.S1.plot

# Build figure S2.
FigureS2 <- plot_grid(supp.ltrs.pr.S1.plot, supp.ganong.ltrs.S1.plot, supp.ganong.pr.S1.plot,
                      nrow=1, rel_widths = c(1/3, 1/3, 1/3))
FigureS2
pdf("FigureS2.pdf", 15, 5, bg="transparent")
plot(FigureS2)
dev.off() 

# Within each session, within each task, phonetic (+lexical) vs. lexical.
# Session 1.
cor.test(supp.correlations$ganong.VOT.S1.mean, supp.correlations$ganong.Continuum.S1)
supp.ganong.xy.S1.plot <- ggplot(data = supp.correlations,
                                 aes(x=ganong.VOT.S1, y=ganong.Continuum.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#6a0084") +
  geom_point() +
  scale_x_continuous(limits=c(0, 6), breaks=c(0, 2, 4, 6)) +
  scale_y_continuous(limits=c(-1.5, 8.5), breaks=c(0, 2, 4, 6, 8)) +
  labs(x = expression("VOT ("*hat(beta)*")"), y = expression("Continuum ("*hat(beta)*")")) +
  annotate(geom="text", x=Inf, y=-Inf, label="r = -0.71, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.Ganong)
supp.ganong.xy.S1.plot

cor.test(supp.correlations$pr.Type.S1, supp.correlations$pr.TypeXBlock.S1)
supp.pr.xy.S1.plot <- ggplot(data = supp.correlations, aes(x=pr.Type.S1, y=pr.TypeXBlock.S1)) +
  theme_cowplot(font_size=20) + 
  geom_smooth(method="lm", color="#6a0084") +
  geom_point() +
  scale_x_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  scale_y_continuous(limits=c(-.6, 1.6), breaks=c(-0.5, 0, .5, 1, 1.5)) +
  labs(x = expression("Type ("*hat(beta)*")"), y = expression("Type x Block ("*hat(beta)*")")) +
  annotate(geom="text", x=Inf, y=-Inf, label="r = 0.65, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.PR)
supp.pr.xy.S1.plot

cor.test(supp.correlations$ltrs.Nonword.S1, supp.correlations$ltrs.Word.S1)
supp.ltrs.xy.S1.plot <- ggplot(data = supp.correlations, aes(x=ltrs.Nonword.S1, y=ltrs.Word.S1)) +
  theme_cowplot(font_size=20) + 
  geom_point() +
  geom_smooth(method="lm", color="#6a0084") +
  scale_x_continuous(limits=c(-3.4, 4.8), breaks=c(-3, -1.5, 0, 1.5, 3, 4.5)) +
  scale_y_continuous(limits=c(-3.4, 4.8), breaks=c(-3, -1.5, 0, 1.5, 3, 4.5)) +
  labs(x = "Nonword (d')", y = "Word (d')") +
  annotate(geom="text", x=Inf, y=-Inf, label="r = -0.43, p < 0.001",
           color="black", hjust=1, vjust=-.8, size=6) +
  theme(plot.title.position = "plot") +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Facet.LTRS)
supp.ltrs.xy.S1.plot

# Build figure S3.
FigureS3 <- plot_grid(supp.ganong.xy.S1.plot, supp.pr.xy.S1.plot, supp.ltrs.xy.S1.plot,
                     nrow=1)
FigureS3
pdf("FigureS3.pdf", 15, 5, bg="transparent")
plot(FigureS3)
dev.off() 

# Supplement; reanalysis/comparison to Ishida et al. (2016). -----
# Data retrieved from the supplementary material provided at:
# https://www.sciencedirect.com/science/article/pii/S0010027716300634?casa_token=LJVHDUbEsLMAAAAA:rfdlrP4TgdGlai_0NyHMeVMFP73NYcK4gAAt34F2FOhV8FZI43dvmX1WWoeIEVwUK1IV3TZy
# Import Ishida et al. data; these are the values shown in the CORREL sheet of the workbook.
ishida <- read.csv("IshidaETAL.csv")

# Build a dataframe that collates across experiments.
ishida$Dataset <- "Ishida et al. (2016)"

# Prepare LTRS from current session 1 and session 2.
supp.ltrs.ind.S1 <- supp.ltrs.ind %>% 
  select(ID, LTRS = ltrs.S1) %>% 
  mutate(Dataset = "Session 1")
supp.ltrs.temp <- supp.ltrs.ind.S1
rm(supp.ltrs.ind.S1)

# Calculate by-subject lexical effect for PR following Ishida et al. methods.
# As described in the paper: "First, we took the difference between the mean score of words with a noise-added phoneme
# and of words with a noise-replaced phoneme (words/added – words/replaced).
# Next, we did the same thing for pseudowords (pseudowords/added – pseudowords/replaced).
# Finally, we took the difference between the difference score for words and the difference
# score for pseudowords." Based on personal correspondence, it was actually replaced - added,
# which matches data in supplemental materials.
supp.pr.ind.S1 <- supp.pr.summary.ID %>% 
  filter(Session == 1) %>% 
  spread(Type, Mean.Rating)
supp.pr.ind.S1$ReplacedMINUSAdded <- supp.pr.ind.S1$Replaced - supp.pr.ind.S1$Added
supp.pr.ind.S1$Added <- NULL
supp.pr.ind.S1$Replaced <- NULL
supp.pr.ind.S1 <- supp.pr.ind.S1 %>% 
  spread(Block, ReplacedMINUSAdded)
supp.pr.ind.S1$PR <- supp.pr.ind.S1$Word - supp.pr.ind.S1$Nonword
supp.pr.ind.S1$Word <- NULL
supp.pr.ind.S1$Nonword <- NULL
supp.pr.ind.S1$Session <- NULL
supp.pr.ind.S1$Dataset <- "Session 1"

supp.pr.temp <- supp.pr.ind.S1
rm(supp.pr.ind.S1)

# Collate to a single data frame.
supp.collated <- full_join(supp.pr.temp, supp.ltrs.temp)
supp.collated <- full_join(supp.collated, ishida)
supp.collated <- supp.collated %>% 
  select(Dataset, ID, LTRS, PR)
rm(supp.pr.temp, supp.ltrs.temp, ishida)

# Perform correlations within each data set.
cor.test(~ LTRS + PR, alternative="two.sided", data=supp.collated,
         subset=(Dataset == "Ishida et al. (2016)")) # r = 0.43, p = 0.002

cor.test(~ LTRS + PR, alternative="two.sided", data=supp.collated,
         subset=(Dataset == "Session 1")) # r = 0.14, p = 0.137

# Perform correlations on LTRS and PR scores greater than or equal to zero.
# Perform correlations within each data set.
cor.test(~ LTRS + PR, alternative="two.sided", data=supp.collated,
         subset=(Dataset == "Ishida et al. (2016)" & LTRS >= 0 & PR >= 0)) # n = 31, r = -0.07, p = 0.695
round((31/52 * 100), digits=0) # 60%

cor.test(~ LTRS + PR, alternative="two.sided", data=supp.collated,
         subset=(Dataset == "Session 1" & LTRS >= 0 & PR >= 0)) # n = 84, r = -0.07, p = 0.531
round((84/120 * 100), digits=0) # 70%

# Build figure S4.
# A helper for printing correlations to plots.
supp.labels.collated <- data.frame(Dataset = c("Ishida et al. (2016)", "Session 1"),
                              Sample.Full = c("n = 52, r = 0.43, p = 0.002",
                                              "n = 120, r = 0.14, p = 0.137"),
                              Sample.Subset = c("n = 31, r = -0.07, p = 0.695",
                                                "n = 84, r = -0.07, p = 0.531"))

FigureS4A <- ggplot(data = supp.collated, aes(x=LTRS, y=PR)) +
  theme_cowplot(font_size=20) + 
  annotate("rect", xmin = 0, xmax = 7.5, ymin = 0, ymax = 2.5,
           alpha = .1, fill = "#FDE725FF") +
  geom_point() +
  geom_smooth(method="lm", color="#6a0084") +
  scale_x_continuous(limits=c(-1.5, 7.5), breaks=c(-1, 0, 1, 2, 3, 4, 5, 6, 7)) +
  scale_y_continuous(limits=c(-1.5, 2.5), breaks=c(-1, 0, 1, 2)) +
  labs(x = "LTRS", y = "Phonemic restoration") +
  geom_text(data = supp.labels.collated, aes(x = Inf, y = -Inf, label = Sample.Full),
            hjust=1, vjust=-.8, size=6) +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Dataset)
FigureS4A

FigureS4B <- ggplot(data = supp.collated %>% filter(LTRS >= 0 & PR >= 0), aes(x=LTRS, y=PR)) +
  theme_cowplot(font_size=20) + 
  annotate("rect", xmin = 0, xmax = 7.5, ymin = 0, ymax = 2.5,
           alpha = .1, fill = "#FDE725FF") +
  geom_point() +
  geom_smooth(method="lm", color="#6a0084") +
  scale_x_continuous(limits=c(-1.5, 7.5), breaks=c(-1, 0, 1, 2, 3, 4, 5, 6, 7)) +
  scale_y_continuous(limits=c(-1.5, 2.5), breaks=c(-1, 0, 1, 2)) +
  labs(x = "LTRS", y = "Phonemic restoration") +
  geom_text(data = supp.labels.collated, aes(x = Inf, y = -Inf, label = Sample.Subset),
            hjust=1, vjust=-.8, size=6) +
  theme(panel.grid.major.y=element_line(colour="grey")) +
  facet_wrap(~Dataset)
FigureS4B

# Collate to figure S4.
FigureS4 <- plot_grid(FigureS4A, FigureS4B, ncol=1, labels=c("A", "B"))
pdf("FigureS4.pdf", 9, 9, bg="transparent")
plot(FigureS4)
dev.off() 

# sessionInfo(). -----
# sessionInfo()
# R version 4.2.0 (2022-04-22)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Big Sur 11.6
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] interactions_1.1.5 cowplot_1.1.1      jtools_2.2.0       cocor_1.1-3        ez_4.4-0          
# [6] lmerTest_3.1-3     lme4_1.1-29        Matrix_1.4-1       forcats_0.5.1      stringr_1.4.0     
# [11] dplyr_1.0.9        purrr_0.3.4        readr_2.1.2        tidyr_1.2.0        tibble_3.1.7      
# [16] ggplot2_3.3.6      tidyverse_1.3.1   
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.8.3        lubridate_1.8.0     lattice_0.20-45     assertthat_0.2.1    digest_0.6.29      
# [6] utf8_1.2.2          R6_2.5.1            cellranger_1.1.0    plyr_1.8.7          backports_1.4.1    
# [11] reprex_2.0.1        httr_1.4.3          pillar_1.7.0        rlang_1.0.2         readxl_1.4.0       
# [16] rstudioapi_0.13     minqa_1.2.4         car_3.0-13          nloptr_2.0.0        labeling_0.4.2     
# [21] splines_4.2.0       pander_0.6.5        munsell_0.5.0       broom_0.8.0         compiler_4.2.0     
# [26] numDeriv_2016.8-1.1 modelr_0.1.8        pkgconfig_2.0.3     mgcv_1.8-40         tidyselect_1.1.2   
# [31] fansi_1.0.3         crayon_1.5.1        tzdb_0.3.0          dbplyr_2.1.1        withr_2.5.0        
# [36] MASS_7.3-57         grid_4.2.0          nlme_3.1-157        jsonlite_1.8.0      gtable_0.3.0       
# [41] lifecycle_1.0.1     DBI_1.1.2           magrittr_2.0.3      scales_1.2.0        cli_3.3.0          
# [46] stringi_1.7.6       carData_3.0-5       farver_2.1.0        reshape2_1.4.4      fs_1.5.2           
# [51] xml2_1.3.3          ellipsis_0.3.2      generics_0.1.2      vctrs_0.4.1         boot_1.3-28        
# [56] tools_4.2.0         glue_1.6.2          hms_1.1.1           abind_1.4-5         colorspace_2.0-3   
# [61] rvest_1.0.2         haven_2.5.0      

# End of script.
# End of script.
# End of script.
# End of script.
# End of script.