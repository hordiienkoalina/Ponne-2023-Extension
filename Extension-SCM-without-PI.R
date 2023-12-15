#-----------------------------------------------------------------------------------------#
# Initial Setup
#-----------------------------------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(Synth)
library(kableExtra)
library(xtable)
library(ggpubr)

source("functions/plot_scm.R")

load("data/DATA_COMPLETE.RData")
load("data/abbr_code.RData")

#-----------------------------------------------------------------------------------------#
# Data Subsetting
#-----------------------------------------------------------------------------------------#

PRIMARY_M <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "math"))
PRIMARY_P <- as.data.frame(filter(DATA_COMPLETE, grade == "P", subject == "port"))
LOWERS_M <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "math"))
LOWERS_P <- as.data.frame(filter(DATA_COMPLETE, grade == "LS", subject == "port"))

#-----------------------------------------------------------------------------------------#
# Preparing Data for Synth (Excluding Piauí)
#-----------------------------------------------------------------------------------------#

prepare_p_ls_PI <- function(data) {
  library(Synth)
  
  predictors <- c("homicides", "TWh", "ln_pop", "unemployment", "edu_invest_pc")
  
  DATA <- dataprep(foo = data,
                   predictors = predictors,
                   dependent = "score",
                   unit.variable = "code_state",
                   time.variable = "year",
                   unit.names.variable = "abbr_state",
                   treatment.identifier = 23, # Ceará Code
                   controls.identifier = c(11:17, 21, 24:29, 31:33, 35, 41:43, 50:53), 
                   # Exclude Piauí (22) and Ceará (23)
                   time.predictors.prior = seq(1995, 2007, 2),
                   time.optimize.ssr = seq(1995, 2007, 2),
                   time.plot = seq(1995, 2019, 2))
  
  return(DATA)
}

#-----------------------------------------------------------------------------------------#
# Execution of Synthetic Control Method
#-----------------------------------------------------------------------------------------#

DATA_PM <- prepare_p_ls_PI(PRIMARY_M)
SCM_PM <- synth(DATA_PM)

DATA_PP <- prepare_p_ls_PI(PRIMARY_P)
SCM_PP <- synth(DATA_PP)

DATA_LSM <- prepare_p_ls_PI(LOWERS_M)
SCM_LSM <- synth(DATA_LSM)

DATA_LSP <- prepare_p_ls_PI(LOWERS_P)
SCM_LSP <- synth(DATA_LSP)

#-----------------------------------------------------------------------------------------#
# Graphs Generation in ggplot (Excluding Piauí)
#-----------------------------------------------------------------------------------------#

# Generate Plots for Each Category
PM <- plot_scm(PRIMARY_M, synth.tab(dataprep.res = DATA_PM, synth.res = SCM_PM))
PP <- plot_scm(PRIMARY_P, synth.tab(dataprep.res = DATA_PP, synth.res = SCM_PP))
LSM <- plot_scm(LOWERS_M, synth.tab(dataprep.res = DATA_LSM, synth.res = SCM_LSM))
LSP <- plot_scm(LOWERS_P, synth.tab(dataprep.res = DATA_LSP, synth.res = SCM_LSP))

#-----------------------------------------------------------------------------------------#
# Combining Data for Graphs and Plot Adjustments
#-----------------------------------------------------------------------------------------#

# Combine the Synthetic Control Graph Data
DATA_GRAPH <- rbind(PM[[1]], PP[[1]], LSM[[1]], LSP[[1]])

# Adjust the Labels and Factors
DATA_GRAPH$grade[DATA_GRAPH$grade=="P"] <- "Primary Education"
DATA_GRAPH$grade[DATA_GRAPH$grade=="LS"] <- "Lower Secondary Education"
DATA_GRAPH$grade[DATA_GRAPH$grade=="US"] <- "Upper Secondary Education"
DATA_GRAPH$grade <- factor(DATA_GRAPH$grade, levels = c("Primary Education", 
                                                        "Lower Secondary Education"))

DATA_GRAPH$subject[DATA_GRAPH$subject=="math"] <- "Mathematics"
DATA_GRAPH$subject[DATA_GRAPH$subject=="port"] <- "Portuguese"
DATA_GRAPH$subject <- factor(DATA_GRAPH$subject, levels = c("Mathematics", "Portuguese"))

#-----------------------------------------------------------------------------------------#
# Final Plotting and Saving
#-----------------------------------------------------------------------------------------#

# Assuming DATA_GRAPH and DATA_GAP are your combined datasets

# Figure for Primary Education
a_without_PI <- ggplot(data = filter(DATA_GRAPH, grade == "Primary Education"), aes(x = year, y = score, color = unit)) +
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = 2011, color = "#636363", linetype = "dashed", size = 0.9) +
  geom_line(size = 0.9) +
  scale_color_manual(values = c("#01665e", "#d8b365"), labels = c("Ceará", "Synthetic Ceará (without Piauí)"), name = "") +
  ylab("Score") +
  xlab("") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
  facet_grid(vars(grade), vars(subject))

# Figure for Lower Secondary Education
b_without_PI <- ggplot(data = filter(DATA_GRAPH, grade == "Lower Secondary Education"), aes(x = year, y = score, color = unit)) +
  geom_vline(xintercept = 2008, color = "#636363", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = 2015, color = "#636363", linetype = "dashed", size = 0.9) +
  geom_line(size = 0.9) +
  scale_color_manual(values = c("#01665e", "#d8b365"), labels = c("Ceará", "Synthetic Ceará (without Piauí)"), name = "") +
  ylab("Score") +
  xlab("Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
  facet_grid(vars(grade), vars(subject))

# Arrange and Save the Plots
ggarrange(a_without_PI, b_without_PI, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave(filename = "figures_without_PI.png", path = "plots", width = 21, height = 15, units = "cm")