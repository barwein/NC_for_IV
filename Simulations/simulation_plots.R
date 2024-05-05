# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(ggpubr)
library(latex2exp)
library(gridExtra)


# Linear and nonlinear DGP ----------------------------------------------------

# Read data
sim.no.kci <- fread("Simulations/Sim_results_linear_non_linear_DGP.csv")

# Save proportion of null rejections
dgp.mean <- sim.no.kci[,.(Bonf = sum(bonf_pval<=0.05, na.rm=T)/.N,
                                   F.test = sum(f_pval<=0.05, na.rm=T)/.N,
                                   Wald = sum(wald_pval<=0.05, na.rm=T)/.N,
                                    GAM.wald. = sum(gam_wald.no.p<=0.05,na.rm=T)/.N,
                                    GAM.wald.smooth.controls = sum(gam_wald.no.p.smooth.cntrl<=0.05,na.rm=T)/.N
                                  ),
                                by = c("n_obs","z_u_coef","nonlinear_weight")]
# Melt df
dgp.mean.melted <- melt.data.table(dgp.mean,
                        id.vars = c("n_obs","z_u_coef","nonlinear_weight"),
                        measure.vars = c("Bonf","F.test","Wald","GAM.wald.","GAM.wald.smooth.controls"))
                        


variable_labels <- c("Bonf" = "Bonferroni-Corrected\nPseudo-Outcome",
                     "F.test" = "F-Test",
                     "Wald" = "Linear Wald",
                     "GAM.wald." = "GAM,\nLinear Controls",
                     "GAM" = "GAM,\nLinear Controls",
                     "GAM.smooth.controls" = "GAM,\nNonlinear Controls",
                     "GAM.wald.smooth.controls" = "GAM,\nNonlinear Controls")

# Define common color and shape scales
common_color_scale <- scale_color_brewer(type = "qual", palette = 6)
common_shape_scale <- scale_shape_manual(values = 1:5)


# theme -------------------------------------------------------------------

theme_set(theme_pubclean(base_size = 20))
theme_update(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
      strip.background = element_rect(fill = NA, colour = "black", size = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = rel(1.1), hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text = element_text(size = rel(1)),
      legend.title = element_text(size = rel(1)),
      legend.text = element_text(size = rel(1)),
      legend.key = element_rect(fill = "white", colour = NA), # White background
      )
      
      
# Fig Linearity -----------------------------------------------------------


fig_linearity <- dgp.mean.melted %>% 
  mutate(Specification = factor(nonlinear_weight, levels = c(0, 1), labels = c("Linear IV-NCO Association", "Non-Linear IV-NCO Association")),
         variable = recode_factor(variable, !!!variable_labels)) %>%
  filter(variable %in% c("Bonferroni-Corrected\nPseudo-Outcome", "F-Test", "GAM,\nLinear Controls", "GAM,\nNonlinear Controls")) %>%
  ggplot(aes(x = factor(z_u_coef),
             y = value,
             group = variable,
             col = variable,
             shape = variable)) +
  geom_hline(yintercept = 0.05, lty = 2, linewidth = 1.1, alpha = 0.7) +
  geom_point(cex = 5, alpha = 0.8) +
  geom_line(linewidth = 1, alpha = 0.7) +
  scale_y_continuous(breaks = c(0,0.05,seq(0.1,1,0.1))) +
  common_color_scale + common_shape_scale  +
  labs(
    title = "A. Outcome Independence Violation",
    x = expression(paste("Strength of IV-NCO Association (", gamma[U], ")")),
    y = "Proportion of Null Rejections",
    color = "Test Method:",
    shape = "Test Method:"
  ) +
  facet_wrap(~Specification) +
  theme(legend.position = "none")

ggsave("Simulations/fig_simulations_linearity.pdf", w = 16, h = 9)


# Updated Plot 1: Rich covariates (RC) --------------------------------------------------

sim.RC <- fread("Simulations/Testing_RC_1000iter.csv")

sim.RC.summary <- sim.RC[,.(
  F.test = sum(nc.F.pval<= 0.05)/.N,
  Bonf = sum(nc.bonf.pval<=0.05)/.N,
  Wald = sum(nc.wald.pval<=0.05)/.N,
  GAM = sum(gam_wald <=0.05)/.N,
  GAM.smooth.controls = sum(gam_wald.smooth.cntrl<=0.05)/.N),
  by = c("n","nc_coef")]

prop.rej.melted <- melt.data.table(sim.RC.summary,
                                   id.vars = c("n","nc_coef"),
                                   measure.vars = c("F.test","Bonf","Wald","GAM","GAM.smooth.controls"))


fig_rc <- prop.rej.melted %>% 
  mutate(variable = recode_factor(variable, !!!variable_labels)) %>%
  filter(variable %in% c("Bonferroni-Corrected\nPseudo-Outcome", "F-Test", "GAM,\nLinear Controls", "GAM,\nNonlinear Controls")) %>%
  ggplot(aes(x = factor(nc_coef),
                          y = value,
                          group = variable,
                          col = variable,
                          shape = variable)) +
  geom_hline(yintercept = 0.05, lty = 2, linewidth = 1.1, alpha = 0.7) +
  geom_point(cex = 5, alpha = 0.8) +
  geom_line(linewidth = 1, alpha = 0.7) +
  scale_y_continuous(breaks = c(0,0.05,seq(0.1,1,0.1))) +
  common_color_scale + common_shape_scale  +
  labs(
    title = "B. Rich Covariates Violation",
    x = expression(paste("Extent of Non-Linearity of the IV in the Control (", eta, ")")),
    y = "Proportion of Null Rejections",
    color = "Test Method:",
    shape = "Test Method:"
  ) +
  theme(legend.position = "bottom")


ggsave("Simulations/fig_simulations_rich_covariates.pdf", width = 16, height = 9)



# save combined plot ------------------------------------------------------

empty_plot <- ggplot() + 
  theme_void() +
  theme(plot.background = element_rect(fill = NA, colour = NA)) # transparent

# Arrange the plots, using empty plots as spacers on the sides of fig_rc
fig_combined <- grid.arrange(
  fig_linearity,
  arrangeGrob(empty_plot, fig_rc, empty_plot, ncol=3, widths=c(1, 2, 1)),
  nrow=2,
  heights=c(1, 1)
)

# Save the combined plot
ggsave("Simulations/fig_simulations_combined.pdf", fig_combined, width = 16, height = 18)
