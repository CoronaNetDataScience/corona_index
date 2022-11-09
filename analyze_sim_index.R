# analyze simulation results 

library(tidyverse)
library(ggplot2)
library(stringr)
library(ggthemes)

theme_set(theme_tufte() +theme(text=element_text(family="")))


sim_vals <- readRDS("data/over_sims.rds")

sim_vals_err <- sapply(sim_vals, function(x) 'data.frame' %in% class(x))

# remove simulations that failed

sim_vals <- sim_vals[sim_vals_err] %>% 
  bind_rows

just_vals <- select(sim_vals, -policy_data) %>% 
  filter(idealpts_est<1,
         pca_est<1)

to_plot <- just_vals %>% 
  gather(key="Model Type", value="RMSE",RMSE_true_idealpts, 
         RMSE_true_pca,RMSE_true_idealpts_me) %>% 
  group_by(`Model Type`) %>% 
  mutate(mean_rmse=mean(RMSE)) %>% 
  ungroup %>% 
        mutate(`Model Type`=recode(`Model Type`,
                                   RMSE_true_idealpts="IRT",
                                   RMSE_true_idealpts_me="IRT with Error",
                                   RMSE_true_pca="PCA"),
          x_rmse=case_when(`Model Type`=="IRT"~0.4,
                          `Model Type`=="PCA"~1.75,
                          TRUE~0.11),size=10)
annotations <- distinct(to_plot, x_rmse, mean_rmse,`Model Type`)

tp1 <- to_plot %>% 
  ggplot(aes(x=RMSE)) +
  geom_histogram() +
  geom_text(data=annotations,
            aes(label=paste0("Mean RMSE:\n",round(mean_rmse, 2)),
                x=x_rmse),y=50) +
  geom_vline(aes(xintercept=mean_rmse),linetype=2, colour="red") +
  facet_wrap(~`Model Type`,scales="free_x") +
  labs(y="Number of Sims",
       caption=stringr::str_wrap("Plot shows 383 Monte Carlo simulations from an ideal point model (squared distance to policy positions). RMSE is calculated relative to the coefficient of the true value of the latent variable in a regression model with a binomial-distributed outcome. The true value of the regression coefficient is -2. Estimates greater than +1 have been removed as they signify a failure of model identification.",
                                  width = 100))
  
ggsave("monte_carlo_irt.pdf",dpi=500,plot=tp1)
tp2 <- tp1 + 
  ggtitle("Recovery of Effect of Latent Variable on Binomial Outcome",
          subtitle="True Effect is -2") +
  theme(plot.background = element_rect(colour="white"))
ggsave("monte_carlo_irt.png",dpi=500,plot=tp2)

