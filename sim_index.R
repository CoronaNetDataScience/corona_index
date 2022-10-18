# simulate and fit idealstan index model


.libPaths("/home/rmk7/other_R_libs2")

require(cmdstanr)

cmdstanr::set_cmdstan_path("/home/rmk7/cmdstan")

library(idealstan)
library(tidyverse)
library(freqdom)

# number of countries
params <- list()
params$N <- 20
params$num_countries <- 20
params$ideal_pts_sd <- 1
params$time_sd <- .25
params$mean <- 0
params$sd <- 1

params$time_points <- 60

# number of policies per day
params$num_policies <- 10


prior_func <- rnorm

# simulate 200 times

over_sims <- parallel::mclapply(1:200, function(i) {
  
  # countries 
  
  output <- rnorm(n=params$N,mean=params$mean,sd=params$sd)
  
  # First simulate ideal points for countries
  
  ideal_pts_mean <- NULL
  
  ideal_t1 <- rnorm(n=params$num_countries,mean=0,sd=params$ideal_pts_sd)
  
  ar_adj <- rep(1,params$num_countries)
  
  # drift parameters
  drift <- rnorm(n=params$num_countries,mean=0,sd=params$ideal_pts_sd)
  
  # R function to generate auto-correlated ideal points
  
  .gen_ts_data <- function(t,adj_in,alpha_int,sigma,init_sides) {
    current_val <- new.env()
    current_val$t1 <- 0
    
    out_vec <- lapply(1:t,function(t_1) {
      
      if(t_1==1) {
        t_11 <- alpha_int
        current_val$t1 <- t_11
        return(data_frame(t_11))
      } else {
        if(adj_in==1) {
          t_11 <- adj_in*current_val$t1 + rnorm(n=1,sd=sigma)
        } else {
          t_11 <- alpha_int + adj_in*current_val$t1 + rnorm(n=1,sd=sigma)
        }
        
      }
      current_val$t1 <- t_11
      return(data_frame(t_11))
    })  %>% bind_rows
    return(out_vec)
  }
  
  ideal_pts <- lapply(1:params$num_countries, function(i) {
    this_person <- .gen_ts_data(t=params$time_points,
                                adj_in=ar_adj[i],
                                alpha_int=drift[i],
                                sigma=params$time_sd,
                                init_sides=ideal_t1[i])
    return(this_person)
  }) %>% bind_cols %>% as.matrix
  ideal_pts <- t(ideal_pts)
  
  
  # First distinct combinations
  
  country_points <- rep(1:params$num_countries,times=params$num_policies) %>% 
    rep(times=params$time_points)
  policy_points <- rep(1:params$num_policies,each=params$num_countries) %>% 
    rep(times=params$time_points)
  
  # generate time points
  
  time_points <- rep(1:params$time_points,each=params$num_countries*params$num_policies)
  
  # need yes and no points on policies
  
  no_points <- rexp(n=params$num_policies, rate = 1)
  yes_points <- no_points+ rexp(n=params$num_policies, rate = 2)
  
  hist(yes_points - no_points)
  
  # errors in decisions
  
  # DGP from Clinton, Jackman and Rivers (2004) p. 356
  
  pr_vote <- sapply(1:length(country_points),function(n) {
    (ideal_pts[country_points[n],time_points[n]] - no_points[policy_points[n]])^2 - (ideal_pts[country_points[n],time_points[n]] - yes_points[policy_points[n]])^2
  })
  
  # 5 continuous and 5 ordinal measures
  
  norm_meas_error <- 0.5
  
  cont_meas <- lapply(1:5, function(i) {
    
    # additional measurement error
    c1 <- country_points[policy_points==i]
    c2 <- time_points[policy_points==i]
    tibble(outcome_cont=rnorm(n=length(country_points[policy_points==i]),
                              mean=pr_vote[policy_points==i],
                              sd=norm_meas_error),
           id=i,
           policy_points=policy_points[policy_points==i],
           country_points=c1,
           time_points=c2)
    
  }) %>% bind_rows
  
  disc_meas <- lapply(6:10, function(i) {
    
    # additional measurement error
    
    c1 <- country_points[policy_points==i]
    c2 <- time_points[policy_points==i]
    
    tibble(outcome_disc=as.numeric(runif(n=length(country_points[policy_points==i]))>plogis(pr_vote[policy_points==i] + rnorm(n=length(country_points[country_points==i]),
                                                                                                                              mean=0,
                                                                                                                              sd=norm_meas_error))),
           id=i,
           policy_points=policy_points[policy_points==i],
           country_points=c1,
           time_points=c2)
    
  }) %>% bind_rows
  
  
  # put data together
  
  combined_data <- bind_rows(cont_meas, disc_meas) %>% 
    mutate(model_id=ifelse(is.na(outcome_disc),9,1))
  
  outobj <- id_make(score_data=combined_data,person_id="country_points",item_id="policy_points",time_id="time_points")
  
  outobj@simul_data <- list(num_countries=params$num_countries,
                            num_policies=params$num_policies,
                            reg_discrim_sd=1,
                            ideal_pts_sd=params$ideal_pts_sd,
                            prior_func=rnorm,
                            true_person=ideal_pts,
                            true_reg_discrim=yes_points - no_points,
                            true_person_mean=ideal_pts_mean,
                            time_sd=params$time_sd,
                            drift=drift,
                            ar_adj=ar_adj)
  
  
  
  # estimate model
  
  est_obj <- id_estimate(outobj,vary_ideal_pts="random_walk",ncores=1,warmup=1500,nchains=1,niters=1000,
                         const_type = "items",max_treedepth=12,id_refresh=100,time_var=4,
                         restrict_ind_high = which((abs(yes_points-no_points))==max(abs(yes_points-no_points))),
                         restrict_ind_low=which((abs(yes_points-no_points))==min(abs(yes_points-no_points))),
                         fix_high=max(abs(yes_points-no_points)),
                         fix_low=min(abs(yes_points-no_points)),
                         discrim_reg_sd=1,person_sd=1,diff_reg_sd = 1,
                         restrict_sd_low = 0.01,restrict_sd_high=0.01)
  
  # create covid data
  
  dist_country <- tibble(country_points=country_points,
                         time_points=time_points) %>% 
    distinct
  
  # standardize ideal_pts by country
  
  ideal_pts_std <- t(apply(ideal_pts, 1, scale))
  
  dist_country <- mutate(dist_country,
                         latent_inf = sapply(1:length(dist_country$country_points),function(n) {
                           -10 - 2*ideal_pts_std[dist_country$country_points[n],dist_country$time_points[n]]
                         }),
                         obs_cases=rbinom(n=n(),size=10000000,prob = plogis(latent_inf)))
  
  # make policy data, estimate it
  
  policy_data <- bind_cols(spread(select(cont_meas,-id), key="policy_points",value="outcome_cont"),
                           select(spread(select(disc_meas,-id), key="policy_points",value="outcome_disc"),-country_points,-time_points))
  
  names(policy_data)[3:length(policy_data)] <- paste0("var_",1:10)
  
  policy_data <- left_join(policy_data,dist_country, by=c("country_points","time_points")) %>% 
    mutate(size=10000000)
  
  # get results
  
  est_cases_policies <- glm(cbind(obs_cases,size) ~ var_1 + var_2 + var_3 + var_4 + var_5 + var_6 + var_7 + var_8 + var_9 + var_10,
                            data=policy_data,family=binomial)
  
  # compare with country ideal points estimates
  
  all_varying <- est_obj@time_varying %>% 
    as_tibble %>% 
    mutate(iter=1:n()) %>% 
    gather(key = "param",value="ideal_pts",-iter) %>% 
    mutate(time_points=as.numeric(str_extract(param, "(?<=\\[)[0-9]+")),
           country_points=as.numeric(str_extract(param, "(?<=,)[0-9]+")))
  
  all_varying_sum <- group_by(all_varying,country_points) %>% 
    mutate(ideal_pts_scale=scale(ideal_pts)) %>% 
    group_by(country_points, time_points) %>% 
    summarize(med_est_ideal=median(ideal_pts),
              sd_est_ideal=sd(ideal_pts),
              med_est_ideal_scale=median(ideal_pts_scale),
              sd_est_ideal_scale=sd(ideal_pts_scale))
  
  policy_data <- left_join(policy_data, all_varying_sum)
  
  est_cases_ideal_pts <- glm(cbind(obs_cases,size) ~ med_est_ideal_scale,
                             data=policy_data,family=binomial)
  
  library(cmdstanr)
  library(brms)
  
  # est_cases_ideal_pts_meas <- brm(bf(obs_cases | trials(size) ~ me(med_est_ideal,sdx = sd_est_ideal),family=binomial),
  #                                 data=policy_data,  chains=1,max_treedepth=15,adapt_delta=0.99, backend="cmdstanr")
  
  c1 <- make_standata(bf(obs_cases | trials(size) ~ me(med_est_ideal_scale,sdx = sd_est_ideal_scale),family=binomial),
                      data=policy_data,  chains=1,max_treedepth=15,adapt_delta=0.99, backend="cmdstanr")
  
  me_model <- cmdstan_model("me_model2.stan")
  
  init_list <- list(
    list(zme_1 = policy_data$med_est_ideal)
  )
  
  fit_me_model <- me_model$sample(data=c1, chains=1,iter_warmup = 1000,iter_sampling = 1500,max_treedepth=12,
                                  init=init_list)
  fit_sum <- fit_me_model$summary()
  
  hist(fit_sum$median[grepl(x=fit_sum$variable, pattern="zme")])
  
  # use dynamic PCA
  
  dpca_data <- as.matrix(select(policy_data, var_1:var_10))
  
  dpca_fit <- dpca(dpca_data,Ndpc = 1)
  var.dpca <-  (1 - sum( (dpca_fit$Xhat - dpca_data)**2 ) / sum(dpca_data**2))*100
  
  # fit a model with these scores
  
  policy_data$pca_scores <- dpca_fit$scores[,1]
  
  policy_data <- group_by(policy_data, country_points) %>% 
    mutate(pca_scores=scale(pca_scores)) %>% 
    ungroup
  
  pca_glm_fit <- glm(cbind(obs_cases,size) ~ pca_scores,
                     data=policy_data,family=binomial)
  
  # return tibble with results
  
  sum_policies <- summary(est_cases_policies)$coefficients[-1,]
  sum_ideal <- summary(est_cases_ideal_pts)$coefficients[-1,]
  sum_ideal_me <- filter(fit_sum, grepl(x=variable, pattern="bsp"))
  sum_pca_fit <- summary(pca_glm_fit)$coefficients[-1,]
  
  print(paste0("finished iteration",i))
  
  tibble(iteration = i,
         policy_data=list(policy_data),
         coefs_pos_sig=sum(sum_policies[,"Estimate"]>0 & sum_policies[,"Pr(>|z|)"]<0.05),
         coefs_neg_sig=sum(sum_policies[,"Estimate"]>0 & sum_policies[,"Pr(>|z|)"]<0.05),
         idealpts_est=sum_ideal["Estimate"],
         idealpts_me_est=sum_ideal_me$median,
         coef_vec_policy=list(sum_policies[,"Estimate"]),
         sd_vec_policy=list(sum_policies[,"Std. Error"]),
         pca_est=sum_pca_fit["Estimate"],
         RMSE_true_idealpts=sqrt((-2 - sum_ideal_me$median)^2),
         RMSE_true_pca=sqrt((-2 - sum_pca_fit[1])^2),
         in_ci_idealpts=as.numeric((-2 < sum_ideal["Std. Error"]*1.96 + sum_ideal["Estimate"]) && (-2 > sum_ideal["Estimate"] - sum_ideal["Std. Error"]*1.96)),
         in_ci_idealpts_me=as.numeric((-2 < sum_ideal_me$q95) && (-2 > sum_ideal_me$q5)),
         in_ci_pca=as.numeric((-2 < sum_pca_fit["Std. Error"]*1.96 + sum_pca_fit["Estimate"]) && (-2 > sum_pca_fit["Estimate"] - sum_pca_fit["Std. Error"]*1.96)))
  
  
  
},mc.cores=parallel::detectCores()) 

  saveRDS(over_sims, "/scratch/rmk7/coronanet/over_sims.rds")



