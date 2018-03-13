#' ---
#' title: Lucky or Clever? -- Analysis File
#' author: Tobias Gerstenberg
#' date: March 13, 2018
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---

#+ General settings, echo = FALSE, results = 'hide' ------------------------------------------------------------------------------

knitr::opts_chunk$set(
  warning=FALSE,
  message=FALSE,
  fig.width=12,
  fig.height=8
  )


#+ PREPARATION -----------------------------------------------------------------------------
#' # PREPARATION
#' - Load packages
#' - Read in raw data
#' - Structure data
#' - Calculate demographics

#+ Packages ------------------------------------------------------------------------------------
#' ## Packages
library(lsr) #useful functions
library(psych) #useful functions
library(afex) #mixed ANOVAS
library(rms) #ordinal logistic regression
library(xtable) #exporting tables into latex
library(lmtest) #likelihood ratio test
library(scales) #rescaling
library(mclust) #clustering
library(Hmisc) #bootstrapped confidence intervals
library(knitr) #pretty tables for knitr output
library(tidyverse)

#+ Read data files -----------------------------------------------------------------------------
#' ## Read data files

rm(list=ls())
df.data = read.csv("../../data/data.csv",header=T,stringsAsFactors=F)

#+ Structure data  -----------------------------------------------------------------------------
#' ## Structure data

goalie_spinner_situations = c("wrong_20","wrong_80",
                      "correct_80","correct_20",
                      "wrong_40","wrong_60",
                      "correct_40","correct_60",
                      "wrong_20","wrong_40",
                      "correct_40","correct_20",
                      "wrong_60","wrong_80",
                      "correct_80","correct_60")

situation_labels = paste0(rep(c("wrong_","correct_"),each=4), seq(20,80,20))

situation_labels2 = paste0(
  rep(c("negative_","positive_"),each=4),
  rep(rep(c("suboptimal_","optimal_"),each=2),2),
  rep(c("nonpivotal","pivotal"),2))

agent_labels = c("bad_agent","average_agent","good_agent")

#GOALIE EXPERIMENT

df.goalie_original = df.data %>%
  filter(experiment == "goalie_original") %>%
  mutate(data_tmp = data %>% strsplit('\n') %>% unlist %>% strsplit(','),
         extra_tmp = extra %>% strsplit('\n') %>% unlist %>% strsplit(','))

df.goalie_original.wide = matrix(unlist(df.goalie_original$data_tmp),nrow=nrow(df.goalie_original),byrow=T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  mutate_all(funs(as.numeric)) %>%
  setNames(paste0(c("round_","ratingLeft_","ratingRight_","time_"),rep(1:8,each=4)))

df.goalie_original.long = df.goalie_original.wide %>%
  mutate(id = 1:nrow(.)) %>%
  wideToLong %>%
  select(-within,-time) %>%
  mutate(round = round + 1) %>%
  gather(key = situation, value = rating,c(ratingLeft,ratingRight)) %>%
  arrange(id,round,situation) %>%
  mutate(situation = rep(goalie_spinner_situations,length(unique(id))),
         situation = factor(situation,levels = situation_labels),
         outcome = str_extract_all(situation,c("wrong|correct"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("20|40|60|80"),simplify=T) %>% as.vector) %>%
  select(id,round,situation,outcome,probability,rating)


#SPINNER EXPERIMENT

df.spinner_original = df.data %>%
  filter(experiment == "spinner_original") %>%
  mutate(data_tmp = data %>% strsplit('\n') %>% unlist %>% strsplit(','),
         extra_tmp = extra %>% strsplit('\n') %>% unlist %>% strsplit(','))

df.spinner_original.wide = matrix(unlist(df.spinner_original$data_tmp),nrow=nrow(df.spinner_original),byrow=T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  mutate_all(funs(as.numeric)) %>%
  setNames(paste0(c("round_","ratingLeft_","ratingRight_","time_"),rep(1:8,each=4)))

df.spinner_original.long = df.spinner_original.wide %>%
  mutate(id = 1:nrow(.)) %>%
  wideToLong %>%
  select(-within,-time) %>%
  mutate(round = round + 1) %>%
  gather(key = situation, value = rating,c(ratingLeft,ratingRight)) %>%
  arrange(id,round,situation) %>%
  mutate(situation = rep(goalie_spinner_situations,length(unique(id))),
         situation = factor(situation,levels = paste0(rep(c("wrong_","correct_"),each=4), seq(20,80,20))),
         outcome = str_extract_all(situation,c("wrong|correct"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("20|40|60|80"),simplify=T) %>% as.vector) %>%
  select(id,round,situation,outcome,probability,rating)

# GOALIE AGENTS EXPERIMENT

df.goalie_skill = df.data %>%
  filter(experiment == "goalie_skill") %>%
  mutate(data = str_replace_all(data,"penalty_miss","wrong"),
         data = str_replace_all(data,"penalty_save","correct"),
         data = str_replace_all(data,"bad goalkeeper","bad_agent"),
         data = str_replace_all(data,"average goalkeeper","average_agent"),
         data = str_replace_all(data,"skilled goalkeeper","good_agent")) %>%
  mutate(data_tmp = data %>% strsplit('\n') %>% unlist %>% strsplit(','),
         extra_tmp = extra %>% strsplit('\n') %>% unlist %>% strsplit(','))

df.goalie_skill.wide = matrix(unlist(df.goalie_skill$data_tmp),nrow=nrow(df.goalie_skill),byrow=T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  setNames(c(paste(c("judgmentRound_","situationLeft_","ratingLeft_","situationRight_","ratingRight_","time_"),rep(1:8,each=6),sep=""),
             paste(c("inferenceRound_","agentTop_","ratingTop_","agentMiddle_","ratingMiddle_","agentBottom_","ratingBottom_"),rep(1:8,each=7),sep="")))

df.goalie_skill.long = df.goalie_skill.wide %>%
  mutate(id = 1:nrow(.)) %>%
  wideToLong() %>%
  select(-within) %>%
  mutate_at(vars(contains("rating"),judgmentRound),funs(as.numeric)) %>%
  mutate(judgmentRound = judgmentRound + 1) %>%
  arrange(id,judgmentRound)

# responsibility judgments
df.goalie_skill.long.responsibility = df.goalie_skill.long %>%
  select(id,judgmentRound,situationLeft,ratingLeft,situationRight,ratingRight) %>%
  gather(key = situation, value = rating,c(ratingLeft,ratingRight)) %>%
  select(-situationLeft,-situationRight) %>%
  rename(round = judgmentRound) %>%
  arrange(id,round,situation) %>%
  mutate(situation = rep(goalie_spinner_situations,length(unique(id))),
         situation = factor(situation,levels = situation_labels),
         outcome = str_extract_all(situation,c("wrong|correct"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("20|40|60|80"),simplify=T) %>% as.vector) %>%
  select(id,round,situation,outcome,probability,rating)

# agent inferences
df.goalie_skill.long.inference = df.goalie_skill.long %>%
  select(id,inferenceRound,contains("Top"),contains("Middle"),contains("Bottom")) %>%
  rename(agent_1 = agentTop, agent_2 = agentMiddle, agent_3 = agentBottom,
         rating_1 = ratingTop, rating_2 = ratingMiddle, rating_3 = ratingBottom,
         situation = inferenceRound) %>%
  wideToLong() %>%
  select(-within) %>%
  mutate(situation = factor(situation,levels = situation_labels),
         agent = factor(agent,levels = agent_labels),
         outcome = str_extract_all(situation,c("wrong|correct"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("20|40|60|80"),simplify=T) %>% as.vector) %>%
  arrange(id,situation,agent) %>%
  select(id,situation,outcome,probability,agent,rating)

# SPINNER AGENTS EXPERIMENT

df.spinner_skill = df.data %>%
  filter(experiment == "spinner_skill") %>%
  mutate(data = str_replace_all(data,"prediction_wrong","wrong"),
         data = str_replace_all(data,"prediction_correct","correct"),
         data = str_replace_all(data,"bad player","bad_agent"),
         data = str_replace_all(data,"average player","average_agent"),
         data = str_replace_all(data,"skilled player","good_agent")) %>%
  mutate(data_tmp = data %>% strsplit('\n') %>% unlist %>% strsplit(','),
         extra_tmp = extra %>% strsplit('\n') %>% unlist %>% strsplit(','))

df.spinner_skill.wide = matrix(unlist(df.spinner_skill$data_tmp),nrow=nrow(df.spinner_skill),byrow=T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  setNames(c(paste(c("judgmentRound_","situationLeft_","ratingLeft_","situationRight_","ratingRight_","time_"),rep(1:8,each=6),sep=""),
             paste(c("inferenceRound_","agentTop_","ratingTop_","agentMiddle_","ratingMiddle_","agentBottom_","ratingBottom_"),rep(1:8,each=7),sep="")))

df.spinner_skill.long = df.spinner_skill.wide %>%
  mutate(id = 1:nrow(.)) %>%
  wideToLong() %>%
  select(-within) %>%
  mutate_at(vars(contains("rating"),judgmentRound),funs(as.numeric)) %>%
  mutate(judgmentRound = judgmentRound + 1) %>%
  arrange(id,judgmentRound)

# responsibility judgments
df.spinner_skill.long.responsibility = df.spinner_skill.long %>%
  select(id,judgmentRound,situationLeft,ratingLeft,situationRight,ratingRight) %>%
  gather(key = situation, value = rating,c(ratingLeft,ratingRight)) %>%
  select(-situationLeft,-situationRight) %>%
  rename(round = judgmentRound) %>%
  arrange(id,round,situation) %>%
  mutate(situation = rep(goalie_spinner_situations,length(unique(id))),
         situation = factor(situation,levels = situation_labels),
         outcome = str_extract_all(situation,c("wrong|correct"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("20|40|60|80"),simplify=T) %>% as.vector) %>%
  select(id,round,situation,outcome,probability,rating)

# agent inferences
df.spinner_skill.long.inference = df.spinner_skill.long %>%
  select(id,inferenceRound,contains("Top"),contains("Middle"),contains("Bottom")) %>%
  rename(agent_1 = agentTop, agent_2 = agentMiddle, agent_3 = agentBottom,
         rating_1 = ratingTop, rating_2 = ratingMiddle, rating_3 = ratingBottom,
         situation = inferenceRound) %>%
  wideToLong() %>%
  select(-within) %>%
  mutate(situation = factor(situation,levels = situation_labels),
         agent = factor(agent,levels = agent_labels),
         outcome = str_extract_all(situation,c("wrong|correct"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("20|40|60|80"),simplify=T) %>% as.vector) %>%
  arrange(id,situation,agent) %>%
  select(id,situation,outcome,probability,agent,rating)

# GARDENER AGENTS EXPERIMENT

df.gardener_skill = df.data %>%
  filter(experiment == "gardener_skill") %>%
  mutate(data = str_replace_all(data,"bad gardener","bad_agent"),
         data = str_replace_all(data,"average gardener","average_agent"),
         data = str_replace_all(data,"skilled gardener","good_agent")) %>%
  mutate(data_tmp = data %>% strsplit('\n') %>% unlist %>% strsplit(','),
         extra_tmp = extra %>% strsplit('\n') %>% unlist %>% strsplit(','))

df.gardener_skill.wide = matrix(unlist(df.gardener_skill$data_tmp),nrow=nrow(df.gardener_skill),byrow=T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  select(-1) %>%
  setNames(c(
    paste(c("situation_","rating_"),rep(1:8,each=2),sep=""),
    paste(c("inferenceRound_","agentTop_","ratingTop_","agentMiddle_","ratingMiddle_","agentBottom_","ratingBottom_"),rep(1:8,each=7),sep="")))

df.gardener_skill.long = df.gardener_skill.wide %>%
  mutate(id = 1:nrow(.)) %>%
  wideToLong() %>%
  # select(-within) %>%
  mutate_at(vars(contains("rating")),funs(as.numeric)) %>%
  arrange(id)

# responsibility judgments
df.gardener_skill.long.responsibility = df.gardener_skill.long %>%
  select(id,situation,rating) %>%
  mutate(situation = factor(situation,levels = situation_labels2),
         outcome = str_extract_all(situation,c("negative|positive"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("suboptimal|optimal"),simplify=T) %>% as.vector,
         pivotality = str_extract_all(situation,c("nonpivotal|pivotal"),simplify=T) %>% as.vector) %>%
  select(id,situation,outcome,probability,pivotality,rating)

# agent inferences
df.gardener_skill.long.inference = df.gardener_skill.long %>%
  select(id,inferenceRound,contains("Top"),contains("Middle"),contains("Bottom")) %>%
  rename(situation = inferenceRound) %>%
  rename(agent_1 = agentTop, agent_2 = agentMiddle, agent_3 = agentBottom,
         rating_1 = ratingTop, rating_2 = ratingMiddle, rating_3 = ratingBottom) %>%
  wideToLong() %>%
  select(-within) %>%
  mutate(situation = factor(situation,levels = situation_labels2),
         agent = factor(agent,levels = agent_labels),
         outcome = str_extract_all(situation,c("negative|positive"),simplify=T) %>% as.vector,
         probability = str_extract_all(situation,c("suboptimal|optimal"),simplify=T) %>% as.vector,
         pivotality = str_extract_all(situation,c("nonpivotal|pivotal"),simplify=T) %>% as.vector) %>%
  arrange(id,situation,outcome,probability,pivotality,agent,rating) %>%
  select(id,situation,outcome,probability,pivotality,agent,rating)

# save data frames in lists

responsibility.list = list(goalie_original = df.goalie_original.long,
                       spinner_original = df.spinner_original.long,
                       goalie = df.goalie_skill.long.responsibility,
                       spinner = df.spinner_skill.long.responsibility,
                       # manager = df.manager_skill.long.responsibility,
                       gardener = df.gardener_skill.long.responsibility)
                       # gardener_noskill = df.gardener_noskill.long)

inference.list = list(goalie = df.goalie_skill.long.inference,
                   spinner = df.spinner_skill.long.inference,
                   # manager = df.manager_skill.long.inference,
                   gardener = df.gardener_skill.long.inference)

# remove unnecessary variables
rm(list = setdiff(ls(),c("df.data","responsibility.list","inference.list")))

#+ Demographics  -------------------------------------------------------------------------------
#' ## Demographics

demographics.list = list()

# Experiment 1

var.names = c("difficulty","gender","age","time",paste0("decision_",1:10))

demographics.list[["goalie_spinner_original"]] = df.data %>%
  filter(experiment %in% c("goalie_original","spinner_original")) %>%
  select(extra) %>%
  unlist %>%
  strsplit("\n") %>%
  unlist %>%
  strsplit(",") %>%
  unlist %>%
  matrix(ncol = length(var.names),byrow=T) %>%
  as.data.frame(stringsAsFactors = F) %>%
  setNames(var.names)

# Experiment 2

demographics.list[["goalie_spinner"]] = df.data %>%
  filter(experiment %in% c("goalie_skill","spinner_skill")) %>%
  select(extra) %>%
  unlist %>%
  strsplit("\n") %>%
  unlist %>%
  strsplit(",") %>%
  unlist %>%
  matrix(ncol = length(var.names),byrow=T) %>%
  as.data.frame(stringsAsFactors = F) %>%
  setNames(var.names)

var.names = c("index","difficulty","gender","age","time")

# Experiment 3
demographics.list[["gardener"]] = df.data %>%
  filter(experiment %in% c("gardener_skill")) %>%
  select(extra) %>%
  unlist %>%
  strsplit("\n") %>%
  unlist %>%
  strsplit(",") %>%
  unlist %>%
  matrix(ncol = length(var.names),byrow=T) %>%
  as.data.frame(stringsAsFactors = F) %>%
  setNames(var.names)

# descriptive information summary

df.descriptive_summary = as.data.frame(matrix(NA,ncol=6,nrow=0)) %>%
  setNames(c("experiment","age.mean","age.sd","n.female","time.mean","time.sd"))

for(i in 1:length(demographics.list)){
  df.tmp = demographics.list[[names(demographics.list)[i]]] %>%
    mutate_all(funs(as.numeric)) %>%
    summarise(experiment = names(demographics.list)[i],
              age.mean = mean(age),
              age.sd = sd(age),
              n.female = sum(gender == 1),
              time.mean = mean(time),
              time.sd = sd(time)
              ) %>%
    mutate_at(vars(-experiment),funs(round(.,2)))
  df.descriptive_summary = rbind(df.descriptive_summary,df.tmp)
}

kable(df.descriptive_summary, caption = "Demographics")

rm("var.names","demographics.list")

#+ Clustering in goalie and spinner condition  -------------------------------------------------
#' ## Clustering in goalie and spinner condition

cluster.list = list()
condition.names = c("goalie","spinner")

for (i in 1:length(condition.names)){
df.tmp = responsibility.list[[condition.names[i]]] %>%
  group_by(id,situation) %>%
  summarise(rating = mean(rating)) %>%
  spread(situation,rating)

# Gaussian mixture model
set.seed(2)
cluster.list[[condition.names[i]]] = data.frame(id = 1:nrow(df.tmp), cluster = Mclust(df.tmp[,-1],modelNames = "EII")$classification)
}

rm("condition.names")

#+ MODEL  ---------------------------------------------------------------------------------------
#' # MODEL
#' - Setting up the model
#' - Defining the agent decision functions
#' - Fitting beta in softmax and priors over the agent types
#' - Calculate difference between prior expected reward and posterior expected reward
#' - Run different regression models

#+ Agent decision functions  -------------------------------------------------------------------
#' ## Agent decision functions

# softmax decision function
softmax = function(beta,theta,reward){
  exp(beta*theta[,1]*reward)/rowSums(exp(beta*theta*reward))
}

# agent decision functions
agent.bad = function(beta,df,reward){
  df = df %>%
    select(outcome,outcome_counterfactual)
  softmax(beta,1-df,reward)
}

agent.average = function(beta,df,reward){
  df = df %>%
    select(probability,probability_counterfactual)
  softmax(beta,df,reward)
}

agent.good = function(beta,df,reward){
  df = df %>%
    select(outcome,outcome_counterfactual)
  softmax(beta,df,reward)
}

#+ Fit beta and priors to posteriors ----------------------------------------------------------------
#' ## Fit beta and priors to posteriors

df.posteriors = rbind(
  inference.list[["goalie"]] %>% group_by(probability,outcome,agent) %>%summarise(rating = mean(rating)) %>% mutate(experiment = "goalie", pivotality = "pivotal"),
  inference.list[["spinner"]] %>% group_by(probability,outcome,agent) %>%summarise(rating = mean(rating)) %>% mutate(experiment = "spinner", pivotality = "pivotal"),
  inference.list[["gardener"]] %>% group_by(probability,pivotality,outcome,agent) %>%summarise(rating = mean(rating)) %>% mutate(experiment = "gardener")
)

#TODO: change the probabilities around here
df.posteriors = df.posteriors %>%
  spread(agent,rating) %>%
  ungroup %>%
  mutate(probability = ifelse(experiment == "gardener", str_replace_all(probability,"suboptimal","30"),probability),
         probability = ifelse(experiment == "gardener", str_replace_all(probability,"optimal","70"),probability),
         probability = as.numeric(probability),
         outcome = str_replace_all(outcome,"wrong","negative"),
         outcome = str_replace_all(outcome,"correct","positive"),
         outcome = as.numeric(as.character(factor(outcome,levels=c("negative","positive"),labels = c("0","1")))),
         outcome_counterfactual = ifelse(pivotality == "pivotal",1-outcome,outcome),
         probability_counterfactual = 100-probability,
         experiment = factor(experiment,levels = c("goalie", "spinner", "gardener")))%>%
  mutate_at(vars(probability,probability_counterfactual),funs(./100)) %>%
  select(experiment,outcome,probability,everything()) %>%
  arrange(experiment,outcome,probability)

# fit the parameters
fit_prior = function(parameters){
  prior = matrix(c(rep(c(parameters[2],parameters[3],1-(parameters[2]+parameters[3])),8),
                   rep(c(parameters[4],parameters[5],1-(parameters[4]+parameters[5])),8),
                   rep(c(parameters[6],parameters[7],1-(parameters[6]+parameters[7])),8)),
                   ncol=3,byrow=T)

  likelihood = cbind(
    agent.bad(parameters[1],df.posteriors,reward),
    agent.average(parameters[1],df.posteriors,reward),
    agent.good(parameters[1],df.posteriors,reward)
  )
  posterior_fit = prior*likelihood/rowSums(prior*likelihood)
  posterior_data = select(df.posteriors,contains("agent"))/100
  return(sum((posterior_data-posterior_fit)^2))
}

# output the predicted posteriors, based on the fit priors and beta
predicted_posterior = function(parameters){
  prior = matrix(c(rep(c(parameters[2],parameters[3],1-(parameters[2]+parameters[3])),8),
                   rep(c(parameters[4],parameters[5],1-(parameters[4]+parameters[5])),8),
                   rep(c(parameters[6],parameters[7],1-(parameters[6]+parameters[7])),8)),
                   ncol=3,byrow=T)

  likelihood = cbind(
    agent.bad(parameters[1],df.posteriors,reward),
    agent.average(parameters[1],df.posteriors,reward),
    agent.good(parameters[1],df.posteriors,reward)
  )
  posterior_fit = prior*likelihood/rowSums(prior*likelihood)
  return(posterior_fit)
}

# reward
reward = 1

# optimize the function
fit = optim(par = runif(7,0,1),fit_prior,method = "L-BFGS-B",
                  lower = rep(0, 7), upper = c(5,rep(1,6)))

# fitted parameters
beta = fit$par[1]
prior.list = list()
prior.list[["goalie"]] = c(fit$par[2],fit$par[3],1-sum(fit$par[2],fit$par[3]))
prior.list[["spinner"]] = c(fit$par[4],fit$par[5],1-sum(fit$par[4],fit$par[5]))
prior.list[["gardener"]] = c(fit$par[6],fit$par[7],1-sum(fit$par[6],fit$par[7]))

# add predicted posteriors to dataframe
tmp = predicted_posterior(fit$par) %>%
  as.data.frame() %>%
  setNames(c("p.bad_agent","p.average_agent","p.good_agent"))

df.posteriors = df.posteriors %>%
  cbind(.,tmp)

#round beta
beta = beta %>% round(1)

#+ Expected reward prior ----------------------------------------------------------------------------
#' ## Expected reward prior

situations = list()
exp_r_prior = list()
exp_r_prior_scaled = list()

# SITUATIONS: GOALIE & SPINNER
situations[["goalie_spinner"]] = expand.grid(probability = seq(0.1,0.9,0.05), outcome = c(0,1)) %>%
  mutate(probability_counterfactual = 1-probability,
         outcome_counterfactual = 1-outcome,
         probability.overall = ifelse(outcome == 1, probability, probability_counterfactual),
         probability.overall = probability.overall/sum(probability.overall))

# SITUATIONS: GARDENER
situations[["gardener"]] = expand.grid(probability = seq(0.1,0.9,0.05), outcome = c(0,1),
                                       probability_counterfactual = seq(0.1,0.9,0.05), outcome_counterfactual = c(0,1)) %>%
  mutate(probability.overall = (1-abs(probability-outcome))*(1-abs(probability_counterfactual-outcome_counterfactual)),
         probability.overall = probability.overall/sum(probability.overall))

situation.names = c("goalie_spinner","gardener")

for(i in 1:length(situation.names)){
  #expected reward prior
  situations[[situation.names[i]]]$agent.bad = agent.bad(beta,situations[[situation.names[i]]],reward)
  situations[[situation.names[i]]]$agent.average = agent.average(beta,situations[[situation.names[i]]],reward)
  situations[[situation.names[i]]]$agent.good = agent.good(beta,situations[[situation.names[i]]],reward)

  exp_r_prior[[situation.names[i]]] = situations[[situation.names[i]]] %>%
    summarise(exp_r_prior.bad = sum((agent.bad*outcome + ((1-agent.bad)*outcome_counterfactual))*probability.overall),
              exp_r_prior.average = sum((agent.average*outcome + ((1-agent.average)*outcome_counterfactual))*probability.overall),
              exp_r_prior.good = sum((agent.good*outcome + ((1-agent.good)*outcome_counterfactual))*probability.overall))

  #rescale expected prior rewards
  exp_r_prior_scaled[[situation.names[i]]] = rescale(exp_r_prior[[situation.names[i]]] %>% as.numeric(),to=c(0,1),from = range(exp_r_prior[[situation.names[i]]]))
}

#+ Difference between expected posterior reward and expected prior reward ----------------------
#' ## Difference between expected posterior reward and expected prior reward

inference.names = c(rep("goalie_spinner",2),"gardener")
experiment.names = c("goalie","spinner","gardener")

exp_r_difference.list = list()

for(i in 1:length(inference.list)){
df.tmp = inference.list[[i]] %>%
  group_by(situation,agent) %>%
  summarise(rating = mean(rating)/100) %>%
  ungroup %>%
  spread(agent,rating) %>%
  mutate(exp_r_prior_sum = sum(prior.list[[names(inference.list[i])]]*exp_r_prior_scaled[[inference.names[i]]]), #prior expected reward
         exp_r_posterior = rowSums(matrix(c(bad_agent,average_agent,good_agent),ncol=3) *
                                     matrix(unlist(rep(exp_r_prior_scaled[[inference.names[i]]],8)),byrow=T,ncol=3)), #posterior expected reward
         exp_r_posterior.model = rowSums(df.posteriors %>% filter(experiment == experiment.names[i]) %>% select(contains("p.")) *
                                     matrix(unlist(rep(exp_r_prior_scaled[[inference.names[i]]],8)),byrow=T,ncol=3)), #posterior expected reward
         exp_r_difference = exp_r_posterior - exp_r_prior_sum,#expected reward difference
         exp_r_difference.model = exp_r_posterior.model - exp_r_prior_sum)

exp_r_difference.list[[names(inference.list[i])]] = select(df.tmp,situation,exp_r_difference,exp_r_difference.model)
}

rm("inference.names")

#+ Regression models  --------------------------------------------------------------------------
#' ## Regression models

df.regression.goalie = responsibility.list[["goalie"]] %>%
  group_by(situation,outcome,probability) %>%
  summarise(mean = mean(rating),
            low = smean.cl.boot(rating)[2],
            high = smean.cl.boot(rating)[3]
  ) %>%
  ungroup() %>%
  mutate(pivotality = 1) %>%
  select(situation,outcome,probability,pivotality,mean,low,high)

df.regression.spinner = responsibility.list[["spinner"]] %>%
  group_by(situation,outcome,probability) %>%
  summarise(mean = mean(rating),
            low = smean.cl.boot(rating)[2],
            high = smean.cl.boot(rating)[3]
  ) %>%
  ungroup() %>%
  mutate(pivotality = 1) %>%
  select(situation,outcome,probability,pivotality,mean,low,high)

df.regression.gardener = responsibility.list[["gardener"]] %>%
  group_by(situation,outcome,probability,pivotality) %>%
  summarise(mean = mean(rating),
            low = smean.cl.boot(rating)[2],
            high = smean.cl.boot(rating)[3]
  ) %>%
  ungroup() %>%
  select(situation,outcome,probability,pivotality,mean,low,high)


df.difference = rbind(exp_r_difference.list[["goalie"]],
                      exp_r_difference.list[["spinner"]],
                      exp_r_difference.list[["gardener"]]
)

df.regression = rbind(df.regression.goalie,
                      df.regression.spinner,
                      df.regression.gardener
) %>%
  mutate(outcome = str_replace_all(outcome,"wrong|negative","0"),
         outcome = str_replace_all(outcome,"correct|positive","1"),
         outcome = as.numeric(outcome),
         pivotality = str_replace_all(pivotality,"nonpivotal","0.5"), #can set different values here for pivotality
         pivotality = str_replace_all(pivotality,"pivotal","1"),
         pivotality = as.numeric(pivotality),
         mean = ifelse(outcome == 0,-mean,mean),
         pivotality = ifelse(outcome == 0,-pivotality,pivotality),
         difference = as.vector(df.difference$exp_r_difference),
         difference.model = as.vector(df.difference$exp_r_difference.model),
         probability = str_replace_all(probability,"suboptimal","30"),
         probability = str_replace_all(probability,"optimal","70"),
         probability = as.numeric(probability)
  ) %>%
  mutate(experiment = rep(c("goalie","spinner","gardener"),each=8)) %>%
  select(experiment,situation,outcome,difference,difference.model,pivotality,mean,low,high,probability)

# list for regression fits
fit.regression = list()

# difference and pivotality
fit.regression[["difference_pivotality"]] = lm(mean~scale(difference)+scale(pivotality),data=df.regression)

# round coefficients
fit.regression[["difference_pivotality"]]$coefficients = fit.regression[["difference_pivotality"]]$coefficients %>% round(0)

df.regression$p.difference_pivotality = fit.regression[["difference_pivotality"]]$coefficients[1] +
  fit.regression[["difference_pivotality"]]$coefficients[2] * scale(df.regression$difference)[,] +
  fit.regression[["difference_pivotality"]]$coefficients[3] * scale(df.regression$pivotality)[,]

# difference only
fit.regression[["difference"]] = lm(mean~difference,data=df.regression)
df.regression$p.difference = fit.regression[["difference"]]$fitted.values

# pivotality only
fit.regression[["pivotality"]] = lm(mean~pivotality,data=df.regression)
df.regression$p.pivotality = fit.regression[["pivotality"]]$fitted.values

# optimality model
df.regression = df.regression %>%
  mutate(optimal = 1,
         optimal = ifelse(str_detect(situation,"20|40|suboptimal"),0,optimal))

fit.regression[["optimality"]] = lm(mean~optimal*outcome,data=df.regression)
df.regression$p.optimality = fit.regression[["optimality"]]$fitted.values

#+ TABLES  ---------------------------------------------------------------------------------------
#' # TABLES
#' - Table with average agent inferences across all experiments
#' - Table with average responsibility judgments across all experiments

#+ Agent inferences longtable  -----------------------------------------------------------------------
#' ## Agent inferences longtable
df.table = df.posteriors %>%
  select(experiment,probability,outcome,pivotality,contains("agent")) %>%
  mutate_at(vars(contains("p."),probability), funs(. * 100)) %>%
  mutate_at(vars(contains("agent")), funs(. %>% round(2))) %>%
  mutate(probability = ifelse(probability == 45.5, 70, probability),
         probability = ifelse(probability == 25.5, 30, probability)) %>%
  select(everything(),-bad_agent,-average_agent,-good_agent,everything())
kable(df.table,caption = "Agent inferences")

#+ Responsibility judgments longtable  -----------------------------------------------------------------------
#' ## Responsibility judgments longtable
df.table = df.regression %>%
  rename(rating = mean) %>%
  select(experiment,probability,outcome,difference,pivotality,p.difference_pivotality,rating) %>%
  mutate_at(vars(difference,p.difference_pivotality,rating),funs(round(.,2)))
kable(df.table,caption = "Responsibility judgments")

#+ STATS  ---------------------------------------------------------------------------------------
#' # STATS

#+ Test for context effects in Experiments 1 and 2 --------------------------------------------------------------------
#' ## Test for context effects in Experiments 1 and 2

df.tmp = responsibility.list[["goalie_original"]]
# df.tmp = responsibility.list[["spinner_original"]]
# df.tmp = responsibility.list[["goalie"]]
# df.tmp = responsibility.list[["spinner"]]

df.tmp = df.tmp %>%
  group_by(id) %>%
  mutate(context = duplicated(situation))

aov_ez(id  = "id",
       dv = "rating",
       data = df.tmp,
       within = c("outcome","probability","context")
)

#+ ANOVAs for responsibility judgments (all experiments) ------------------------------------------------------------------
#' ## ANOVAs for responsibility judgments (all experiments)

anova_results = function(x){
  paste0('$F(',x$`num Df`,',',x$`den Df`,') = ', x$F %>% round(2), ', p = ', x$`Pr(>F)` %>% round(3),', \\eta^2 = ', x$pes %>% round(2),'$') %>% cat()
}

# Experiment 1
df.tmp = rbind(responsibility.list[["goalie_original"]] %>% mutate(condition = "goalie"),
               responsibility.list[["spinner_original"]] %>% mutate(condition = "spinner",
                                                   id = id + max(responsibility.list[["goalie_original"]][["id"]])))

# Experiment 2
# df.tmp = rbind(responsibility.list[["goalie"]] %>% mutate(condition = "goalie"),
#                responsibility.list[["spinner"]] %>% mutate(condition = "spinner",
#                                                            id = id + max(responsibility.list[["goalie"]][["id"]])))


fit = aov_ez(id  = "id",
       dv = "rating",
       data = filter(df.tmp,outcome == "wrong"), #blame judgments
       # data = filter(df.tmp,outcome == "correct"), #credit judgments
       anova_table = list(correction = "none", es = "pes"),
       within = "probability",
       between = "condition"
)
anova_results(fit$anova_table)

# # Experiment 3
df.tmp = responsibility.list[["gardener"]]

fit = aov_ez(id  = "id",
       dv = "rating",
       data = filter(df.tmp,outcome == "negative"), #blame judgments
       # data = filter(df.tmp,outcome == "positive"), #credit judgments
       anova_table = list(correction = "none", es = "pes"),
       within = c("probability","pivotality")
)

anova_results(fit$anova_table)

#+ Model comparisons  ---------------------------------------------------------------------------
#' ## Model comparisons

model.names = c("difference_pivotality","difference","pivotality","optimality")

# Likelihood ratio test for nested models
lrtest(fit.regression[[model.names[1]]],
       fit.regression[[model.names[2]]])

anova(fit.regression[[model.names[1]]])

# BIC and AIC scores

column.names = c("Model","r","rmse","BIC")
model.metrics = matrix(NA, nrow = length(fit.regression),ncol = length(column.names)) %>%
  as.data.frame() %>%
  setNames(column.names) %>%
  mutate(Model = names(fit.regression))

rmse = function(x1,x2){
  return(sqrt(mean((x1-x2)^2)))
}

for(i in 1:length(model.names)){
  model.metrics$BIC[i] = BIC(fit.regression[[model.names[i]]])
  model.metrics$r[i] = cor(abs(df.regression$mean),abs(df.regression[[paste0("p.",model.names[i])]]))
  model.metrics$rmse[i] = rmse(abs(df.regression$mean),abs(df.regression[[paste0("p.",model.names[i])]]))
}

kable(model.metrics,caption = 'Model comparison')

#+ Ordinal logistic regression on agent inferences --------------------------------------------
#' ## Ordinal logistic regression on agent inferences

# SPINNER AND GOALIE
df.tmp = inference.list[["goalie"]] %>%
  mutate(experiment = "goalie") %>%
  rbind(inference.list[["spinner"]] %>% mutate(experiment = "spinner")) %>%
  group_by(experiment,id,situation) %>%
  mutate(ranks = rank(rating,ties.method = "average"),
         mostlikely = which.max(rating),
         mostlikely = factor(mostlikely,levels = 1:3, labels = c("bad_agent","average_agent","good_agent"))) %>%
  select(id, experiment, situation, outcome, probability, mostlikely) %>%
  group_by(experiment,id,situation,mostlikely) %>%
  filter(row_number()==1) %>%
  mutate(probability = probability %>% as.numeric*0.01) %>%
  ungroup %>%
  mutate(experiment = experiment %>% factor(levels=c("goalie","spinner")) %>% as.numeric %>% -1) %>%
  mutate(outcome = outcome %>% factor(levels=c("wrong","correct")) %>% as.numeric %>% -1)

# ordinal logistic regression: check whether there is any effect of world (i.e. spinner vs. goalie)
ologit = lrm(mostlikely~experiment*probability*outcome, data = df.tmp)
anova(ologit)

# regression without experiment factor
ologit.red = lrm(mostlikely~outcome, data = df.tmp)
anova(ologit.red)

ologit.red = lrm(mostlikely~probability, data = df.tmp)
anova(ologit.red)

ologit.red = lrm(mostlikely~probability*outcome, data = df.tmp)
anova(ologit.red)

# effect of condition for unexpected positive outcomes
inference.list[["goalie"]] %>%
  mutate(experiment = "goalie") %>%
  rbind(inference.list[["spinner"]] %>%
          mutate(experiment = "spinner")) %>%
  group_by(experiment,id,situation) %>%
  filter(situation %in% c('correct_20','correct_40')) %>%
  group_by(experiment,agent) %>%
  summarise(rating = rating %>% mean() %>% round())

# GARDENER
experiment.name = "gardener"

df.tmp = inference.list[[experiment.name]] %>%
  group_by(id,situation) %>%
  mutate(ranks = rank(rating,ties.method = "average"),
         mostlikely = which.max(rating),
         mostlikely = factor(mostlikely,levels = 1:3, labels = c("bad_agent","average_agent","good_agent"))) %>%
  select(id, situation, outcome, probability, pivotality, mostlikely) %>%
  group_by(id,situation,mostlikely) %>%
  filter(row_number()==1) %>%
  ungroup %>%
  mutate(outcome = outcome %>% factor(levels=c("negative","positive")) %>% as.numeric %>% -1) %>%
  mutate(probability = probability %>% factor(levels=c("suboptimal","optimal")) %>% as.numeric %>% -1) %>%
  mutate(pivotality = pivotality %>% factor(levels=c("nonpivotal","pivotal")) %>% as.numeric %>% -1)

# ordinal logistic regression
ologit.red = lrm(mostlikely~outcome, data = df.tmp)
anova(ologit.red)

ologit.red = lrm(mostlikely~probability, data = df.tmp)
anova(ologit.red)

ologit.red = lrm(mostlikely~pivotality, data = df.tmp)
anova(ologit.red)

ologit = lrm(mostlikely~probability*outcome, data = df.tmp)
anova(ologit)

ologit = lrm(mostlikely~outcome*pivotality, data = df.tmp)
anova(ologit)

ologit = lrm(mostlikely~probability*outcome*pivotality, data = df.tmp)
anova(ologit)

#+ PLOTS  ---------------------------------------------------------------------------------------
#' # PLOTS

#+ Plotting functions --------------------------------------------------------------------------
#' ## Plotting functions

# responsiblity judgments without model predictions
plot.responsibility.no_model = function(x,bottom_space){
  ggplot(x,aes(x=probability,y=rating,fill=outcome))+
    stat_summary(fun.y = mean, geom = "bar",color="black") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width=0, size = 2)+
    facet_wrap(~outcome)+
    scale_y_continuous(breaks = seq(0,100,25), expand = c(0,0))+
    coord_cartesian(ylim=c(0,100))+
    labs(x = "situation", y = "responsibility rating")+
    scale_fill_manual(values=c("red3","green3"))+
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size=24),
          strip.text = element_text(size=24),
          axis.text.x = element_text(size = 20),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0,0.2,bottom_space,0.2),"cm")
    )
}

# responsibility judgments with model predictions
plot.responsibility.with_model = function(x,bottom_space){
  ggplot(x,aes(x=probability,y=value,group=index,fill=color_index))+
    stat_summary(fun.y = mean, geom = "bar",color="black",position = position_dodge(0.9)) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar",position = position_dodge(0.9),width=0, size = 2)+
    facet_wrap(~outcome)+
    scale_y_continuous(breaks = seq(0,100,25), expand = c(0,0))+
    coord_cartesian(ylim=c(0,100))+
    labs(x = "situation", y = "responsibility rating")+
    scale_fill_manual(values=c("red3","green3","black"))+
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size=24),
          strip.text = element_text(size=24),
          axis.text.x = element_text(size = 20),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0,0.2,bottom_space,0.2),"cm")
    )
}

plot.inferences = function(x,experiment.name, title.name){
  if (experiment.name %in% c("spinner","goalie")){
    agentlabels = c("unskilled  ","average  ","skilled  ")
  }else{
    agentlabels = c("bad  ","average  ","skilled  ")
  }

  ggplot(x,aes(x=probability,y=rating,group=rev(agent),fill=rev(agent)))+
    stat_summary(fun.y = mean, geom = "bar",color="black",position=position_fill(),width=0.75) +
    facet_wrap(~outcome,ncol=2)+
    labs(y = "agent inference", fill = "type of agent:  ", x = "situation")+
    scale_fill_manual(values = rev(c("peru", rgb(0.7,0.7,0.7), "gold")),labels = rev(agentlabels))+
    scale_alpha_manual(values = c(1,0.5),guide='none')+
    scale_y_continuous(breaks = seq(0,1,0.25), labels = paste0(seq(0,100,25),"%"))+
    coord_cartesian(xlim = c(0.5, 4.5), ylim = c(0,1.01), expand=F)+
    theme_bw()+
    theme(
      axis.title.x=element_text(margin = margin(t = 15,unit="pt")),
      legend.position="bottom",
      text = element_text(size=24),
      strip.text = element_text(size=24),
      axis.text.x = element_text(size = 20),
      axis.ticks.x = element_blank(),
      legend.key = element_rect(color="black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank()
    )+
    guides(fill = guide_legend(reverse = T, title.vjust = 1.5))
}

#+ Plots of responsibility judgments without model predictions  ------------------------------------------------------------
#' ## Plots of responsibility judgments without model predictions

experiment.name = "goalie_original"
# experiment.name = "spinner_original"

df.plot = responsibility.list[[experiment.name]] %>%
    mutate(probability = paste0(probability,'%'),
           outcome = factor(outcome,levels = c("wrong", "correct"), labels = c("negative outcome",
                                                                               "positive outcome")))

# plot results
plot.responsibility.no_model(df.plot,0.2)
# ggsave(paste("../../figures/plots/",experiment.name,"/",experiment.name,"_responsibility_bars.pdf",sep=""),width=10,height=6)

#+ Plots of responsibility judgments without model predictions (cluster)  ------------------------------------------------------------
#' ## Plots of responsibility judgments without model predictions (cluster)

experiment.name = "goalie"
# experiment.name = "spinner"
cluster.index = 1
# cluster.index = 2

df.plot = responsibility.list[[experiment.name]] %>%
  mutate(probability = paste0(probability,'%'),
         outcome = factor(outcome,levels = c("wrong", "correct"), labels = c("negative outcome",
                                                                             "positive outcome"))) %>%
  merge(.,cluster.list[[experiment.name]]) %>%
  filter(cluster == cluster.index)

# plot results
plot.responsibility.no_model(df.plot,2)
# ggsave(paste("../../figures/plots/",experiment.name,"/",experiment.name,"_responsibility_cluster",cluster.index,"_bars.pdf",sep=""),width=8,height=6)

#+ Responsibility plots with model predictions  ------------------------------------------------
#' ## Responsibility plots with model predictions

model.name = "difference_pivotality"
# model.name = "pivotality"
# model.name = "difference"

experiment.name = "goalie"
# experiment.name = "spinner"
# experiment.name = "gardener"

df.prediction = df.regression %>%
  filter(experiment == experiment.name)

if (experiment.name %in% c("goalie","spinner")){
  df.plot = responsibility.list[[experiment.name]] %>%
    select(id,situation,outcome,probability,rating) %>%
    merge(.,select(df.prediction,situation,p.difference,p.pivotality,p.difference_pivotality)) %>%
    mutate_at(vars(p.difference,p.pivotality,p.difference_pivotality),funs(abs)) %>%
    mutate(probability = paste0(probability,"%"),
           outcome = str_replace_all(outcome,"wrong","negative outcome"),
           outcome = str_replace_all(outcome,"correct","positive outcome")
    )
}else{
  df.plot = responsibility.list[[experiment.name]] %>%
    select(id,situation,outcome,probability,pivotality,rating) %>%
    rename(pivotality.index = pivotality) %>%
    merge(.,select(df.prediction,situation,p.difference,p.pivotality,p.difference_pivotality)) %>%
    mutate_at(vars(p.difference,p.pivotality,p.difference_pivotality),funs(abs)) %>%
    mutate(pivotality.index = str_replace_all(pivotality.index,"nonpivotal","not\npivotal"),
           probability = str_replace_all(probability,"suboptimal","30%\n"),
           probability = str_replace_all(probability,"optimal","70%\n"),
           probability = paste0(probability,pivotality.index),
           outcome = str_replace_all(outcome,"negative","negative outcome"),
           outcome = str_replace_all(outcome,"positive","positive outcome")
    )
}

df.plot = df.plot %>%
  select_("id","outcome","probability",paste0("p.",model.name),"rating") %>%
  gather_("index","value",c("rating",paste0("p.",model.name))) %>%
  mutate(color_index = 1,
         color_index = ifelse(outcome == "negative outcome",0,color_index),
         color_index = ifelse(index == paste0("p.",model.name),2,color_index),
         color_index = as.factor(color_index),
         index = factor(index,levels = c("rating",model.name),labels = c("rating",model.name))) %>%
  arrange(index,id,outcome,probability)

if (experiment.name %in% c("goalie","spinner")){
  bottom_space = 0.2
}else{
  bottom_space = 2
}

# plot results
plot.responsibility.with_model(df.plot,bottom_space)

# ggsave(paste("../../figures/plots/",experiment.name,"/",experiment.name,"_responsibility_model_bars.pdf",sep=""),width=10,height=6)

#+ Plots of agent inferences (data)  ------------------------------------------------------------------
#' ## Plots of agent inferences (data)

experiment.name = "goalie"
# experiment.name = "spinner"
# experiment.name = "gardener"

df.plot = inference.list[[experiment.name]]

if(experiment.name %in% c("goalie","spinner")){
  df.plot = df.plot %>%
    mutate(probability = paste0(probability,'%'),
           outcome = factor(outcome,levels = c("wrong", "correct"), labels = c("negative outcome",
                                                                               "positive outcome")))
}else{
  df.plot = df.plot %>%
    mutate(probability = paste0(probability,pivotality),
           probability = str_replace_all(probability,"suboptimal", "30%\n"),
           probability = str_replace_all(probability,"optimal", "70%\n"),
           probability = str_replace_all(probability,"nonpivotal", "not\npivotal"),
           outcome = factor(outcome,levels = c("negative", "positive"), labels = c("negative outcome",
                                                                                   "positive outcome")))
}

# plot results
plot.inferences(df.plot,experiment.name,element_blank())

# ggsave(paste("../../figures/plots/",experiment.name,"/",experiment.name,"_inferences_bars.pdf",sep=""),width=10,height=6)

#+ Plots of agent inferences (model)  ------------------------------------------------------------------
#' ## Plots of agent inferences (model)

experiment.name = "goalie"
# experiment.name = "spinner"
# experiment.name = "gardener"

df.plot = df.posteriors %>%
  filter(experiment == experiment.name) %>%
  gather(agent, rating, c(p.bad_agent,p.average_agent,p.good_agent))

if(experiment.name %in% c("goalie","spinner")){
  df.plot = df.plot %>%
    mutate(probability = paste0(100*probability,'%'),
           outcome = factor(outcome,levels = c(0, 1), labels = c("negative outcome",
                                                                 "positive outcome")),
           agent = factor(agent,levels = paste0("p.",c("bad","average","good"),"_agent")))
}else{
  df.plot = df.plot %>%
    mutate(probability = paste0(probability*100,"%"),
           pivotality = str_replace_all(pivotality,"nonpivotal", "not pivotal"),
           probability = paste0(probability,"\n",pivotality),
           outcome = factor(outcome,levels = c(0,1), labels = c("negative outcome",
                                                                "positive outcome")),
           agent = factor(agent,levels = paste0("p.",c("bad","average","good"),"_agent")))
}

# plot results
plot.inferences(df.plot,experiment.name,element_blank())

# ggsave(paste("../../figures/plots/",experiment.name,"/",experiment.name,"_inferences_bars.pdf",sep=""),width=8,height=6)

#+ Plots of agent inferences (cluster) ------------------------------------------------------------------
#' ## Plots of agent inferences (cluster)

experiment.name = "goalie"
# experiment.name = "spinner"
cluster.index = 1
# cluster.index = 2

df.plot = inference.list[[experiment.name]] %>%
  mutate(probability = paste0(probability,'%'),
         outcome = factor(outcome,levels = c("wrong", "correct"), labels = c("negative outcome",
                                                                             "positive outcome"))) %>%
  merge(.,cluster.list[[experiment.name]]) %>%
  filter(cluster == cluster.index)


# plot results
plot.inferences(df.plot,experiment.name,element_blank())

# ggsave(paste("../../figures/plots/",experiment.name,"/",experiment.name,"_inferences_cluster",cluster.index,"_bars.pdf",sep=""),width=8,height=6)

#+ Scatter plots (responsibility)  ------------------------------------------------------------------------------
#' ## Scatter plots (responsibility)

model.name = "difference_pivotality"
# model.name = "pivotality"
# model.name = "difference"
# model.name = "optimality"

df.plot = df.regression %>%
  mutate_at(vars(p.difference,p.pivotality,p.difference_pivotality,p.optimality,mean),funs(abs)) %>%
  mutate(experiment = factor(experiment,levels = c("goalie","spinner","gardener","manager")),
         outcome = factor(outcome,levels = c(0,1), labels = c("negative", "positive")))

ggplot(df.plot,aes_string(x=paste0("p.",model.name),y="mean",color="experiment"))+
  geom_abline(intercept = 0,slope = 1, linetype = 2)+
  geom_smooth(aes(group=1), method="lm",color="black")+
  geom_errorbar(aes(ymin=low,ymax=high),alpha = 1,width=0,size=1)+
  geom_point(aes(shape=outcome),size=4)+
  annotate(x = 10, y = 95, geom = 'text', label = paste("R^2 == ", cor(df.plot$mean,df.plot[[paste0("p.",model.name)]])^2 %>% round(2)), size = 8, parse = T)+
  theme_bw()+
  labs(y = "responsibility rating", x="model prediction")+
  scale_x_continuous(breaks = seq(0,100,25))+
  scale_y_continuous(breaks = seq(0,100,25))+
  coord_cartesian(xlim=c(0,100),ylim=c(0,100))+
  theme(text = element_text(size=26),
        legend.position=c(1,0),
        legend.justification=c(1.01,-0.01),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        axis.title.x = element_text(margin = margin(0.2,0,0,0,"cm")),
        axis.title.y = element_text(margin = margin(0,0.1,0,0,"cm"))
  )+
  guides(color = guide_legend(order = 1),
         shapes = guide_legend(order = 2))

# ggsave(paste0("../../figures/plots/misc/",model.name,"_scatter_errorbars.pdf"),width=8,height=6)

#+ Scatter plots (agent inferences)  ------------------------------------------------------------------------------
#' ## Scatter plots (agent inferences)

df.plot = df.posteriors %>%
  select(experiment,outcome,probability,pivotality,contains("agent")) %>%
  rename(e.agent_bad = bad_agent,
         e.agent_average = average_agent,
         e.agent_good = good_agent,
         p.agent_bad = p.bad_agent,
         p.agent_average = p.average_agent,
         p.agent_good = p.good_agent) %>%
  mutate(id = 1:nrow(.)) %>%
  wideToLong(within = "agent") %>%
  mutate(p.agent = p.agent * 100,
         outcome = factor(outcome,levels = c(0,1), labels = c("negative","positive")),
         agent = factor(agent,levels = c("bad","average","good"))) %>%
  filter(experiment %in% c("goalie","spinner","gardener"))

ggplot(df.plot,aes(x=p.agent,y=e.agent,color=experiment))+
  geom_smooth(aes(group=1), method="lm",color="black")+
  geom_point(aes(shape=agent),size=4)+
  theme_bw()+
  labs(y = "agent inference", x="model prediction")+
  scale_x_continuous(breaks = seq(0,100,10))+
  scale_y_continuous(breaks = seq(0,100,10))+
  coord_cartesian(xlim=c(0,75),ylim=c(0,75),expand=F)+
  theme(text = element_text(size=20),
        legend.position=c(1,0),
        legend.justification=c(1,0),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(0.5,0,0,0,"cm")),
        axis.title.y = element_text(margin = margin(0,0.5,0,0,"cm"))
  )+
  guides(color = guide_legend(order = 1),
         shapes = guide_legend(order = 2))

cor(df.plot$e.agent,df.plot$p.agent)

# ggsave(paste0("../../figures/plots/misc/agent_inferences_scatter.pdf"),width=8,height=6)

#+ CROSS VALIDATION  ---------------------------------------------------------------------------------------
#' # CROSS VALIDATION

#+ Data frame ----------------------------------------------------------------------------------
#' ## Data frame

df.cv = rbind(
  inference.list[["goalie"]] %>% group_by(probability,outcome,agent) %>%summarise(rating = mean(rating)) %>% mutate(experiment = "goalie", pivotality = "pivotal"),
  inference.list[["spinner"]] %>% group_by(probability,outcome,agent) %>%summarise(rating = mean(rating)) %>% mutate(experiment = "spinner", pivotality = "pivotal"),
  inference.list[["gardener"]] %>% group_by(probability,pivotality,outcome,agent) %>%summarise(rating = mean(rating)) %>% mutate(experiment = "gardener")
)

df.cv = df.cv %>%
  spread(agent,rating) %>%
  ungroup %>%
  mutate(probability = ifelse(experiment == "gardener", str_replace_all(probability,"suboptimal","30"),probability),
         probability = ifelse(experiment == "gardener", str_replace_all(probability,"optimal","70"),probability),
         outcome = str_replace_all(outcome,"wrong","negative"),
         outcome = str_replace_all(outcome,"correct","positive"),
         outcome = as.numeric(as.character(factor(outcome,levels=c("negative","positive"),labels = c("0","1")))),
         probability = as.numeric(probability),
         pivotality = str_replace_all(pivotality,"nonpivotal","0.5"),
         pivotality = str_replace_all(pivotality,"pivotal","1"),
         pivotality = as.numeric(pivotality),
         pivotality = ifelse(outcome == 0,-pivotality,pivotality),
         outcome_counterfactual = ifelse(pivotality == "pivotal",1-outcome,outcome),
         probability_counterfactual = 100-probability,
         experiment = factor(experiment,levels = c("goalie", "spinner","gardener")))%>%
  mutate_at(vars(probability,probability_counterfactual),funs(./100)) %>%
  select(experiment,outcome,probability,everything()) %>%
  arrange(experiment,outcome,probability)

df.regression.goalie = responsibility.list[["goalie"]] %>%
  group_by(situation,outcome,probability) %>%
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  mutate(pivotality = 1) %>%
  select(situation,outcome,probability,pivotality,rating)

df.regression.spinner = responsibility.list[["spinner"]] %>%
  group_by(situation,outcome,probability) %>%
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  mutate(pivotality = 1) %>%
  select(situation,outcome,probability,pivotality,rating)

df.regression.gardener = responsibility.list[["gardener"]] %>%
  group_by(situation,outcome,probability,pivotality) %>%
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  select(situation,outcome,probability,pivotality,rating)

df.cv2 = rbind(df.regression.goalie,
               df.regression.spinner,
               df.regression.gardener) %>%
  mutate(experiment = rep(c("goalie","spinner","gardener"),each=8),
         probability = ifelse(experiment == "gardener", str_replace_all(probability,"suboptimal","30"),probability),
         probability = ifelse(experiment == "gardener", str_replace_all(probability,"optimal","70"),probability),
         probability = as.numeric(probability),
         probability = probability/100,
         outcome = str_replace_all(outcome,"wrong|negative","0"),
         outcome = str_replace_all(outcome,"correct|positive","1"),
         outcome = as.numeric(outcome),
         pivotality = str_replace_all(pivotality,"nonpivotal","0.5"),
         pivotality = str_replace_all(pivotality,"pivotal","1"),
         pivotality = as.numeric(pivotality),
         rating = ifelse(outcome == 0,-rating,rating),
         pivotality = ifelse(outcome == 0,-pivotality,pivotality),
         experiment = factor(experiment,levels = c("goalie", "spinner", "gardener"))
  ) %>%
  select(experiment,outcome,pivotality,probability,rating)

df.cv = df.cv %>%
  merge(.,df.cv2) %>%
  mutate(situation.index = rep(1:8,3),
         optimality = ifelse(probability > 0.4, 1, 0)) %>%
  arrange(experiment)

rm(list = setdiff(ls(),c("df.cv","df.data","inference.list", "responsibility.list")))

#+ Functions  ----------------------------------------------------------------------------------
#' ## Functions

# softmax decision function
softmax = function(beta,theta,reward){
  exp(beta*theta[,1]*reward)/rowSums(exp(beta*theta*reward))
}

# agent decision functions
agent.bad = function(beta,df,reward){
  df = df %>%
    select(outcome,outcome_counterfactual)
  softmax(beta,1-df,reward)
}

agent.average = function(beta,df,reward){
  df = df %>%
    select(probability,probability_counterfactual)
  softmax(beta,df,reward)
}

agent.good = function(beta,df,reward){
  df = df %>%
    select(outcome,outcome_counterfactual)
  softmax(beta,df,reward)
}

fit_prior_cv = function(parameters){
  ngoalie = sum(df.tmp$experiment == "goalie")
  nspinner = sum(df.tmp$experiment == "spinner")
  ngardener = sum(df.tmp$experiment == "gardener")

  prior = matrix(c(rep(c(parameters[2],parameters[3],1-(parameters[2]+parameters[3])),ngoalie),
                   rep(c(parameters[4],parameters[5],1-(parameters[4]+parameters[5])),nspinner),
                   rep(c(parameters[6],parameters[7],1-(parameters[6]+parameters[7])),ngardener)),ncol=3,byrow=T)

  likelihood = cbind(
    agent.bad(parameters[1],df.tmp,reward),
    agent.average(parameters[1],df.tmp,reward),
    agent.good(parameters[1],df.tmp,reward)
  )
  posterior_fit = prior*likelihood/rowSums(prior*likelihood)
  posterior_data = select(df.tmp,contains("agent"))/100
  return(sum((posterior_data-posterior_fit)^2))
}

# output the predicted posteriors, based on the fit priors and beta
predicted_posterior = function(parameters){
  ngoalie = sum(df.tmp$experiment == "goalie")
  nspinner = sum(df.tmp$experiment == "spinner")
  ngardener = sum(df.tmp$experiment == "gardener")

  prior = matrix(c(rep(c(parameters[2],parameters[3],1-(parameters[2]+parameters[3])),ngoalie),
                   rep(c(parameters[4],parameters[5],1-(parameters[4]+parameters[5])),nspinner),
                   rep(c(parameters[6],parameters[7],1-(parameters[6]+parameters[7])),ngardener)),ncol=3,byrow=T)

  likelihood = cbind(
    agent.bad(parameters[1],df.tmp,reward),
    agent.average(parameters[1],df.tmp,reward),
    agent.good(parameters[1],df.tmp,reward)
  )
  posterior_fit = prior*likelihood/rowSums(prior*likelihood)
  return(posterior_fit)
}

expected_reward = function(){
  situations = list()
  exp_r_prior = list()
  exp_r_prior_scaled = list()

  # SITUATIONS: GOALIE & SPINNER
  situations[["goalie_spinner"]] = expand.grid(probability = seq(0.1,0.9,0.05), outcome = c(0,1)) %>%
    mutate(probability_counterfactual = 1-probability,
           outcome_counterfactual = 1-outcome,
           probability.overall = ifelse(outcome == 1, probability, probability_counterfactual),
           probability.overall = probability.overall/sum(probability.overall))

  # SITUATIONS: GARDENER
  situations[["gardener"]] = expand.grid(probability = seq(0.1,0.9,0.05), outcome = c(0,1),
                                         probability_counterfactual = seq(0.1,0.9,0.05), outcome_counterfactual = c(0,1)) %>%
    mutate(probability.overall = (1-abs(probability-outcome))*(1-abs(probability_counterfactual-outcome_counterfactual)),
           probability.overall = probability.overall/sum(probability.overall))

  situation.names = c("goalie_spinner","gardener")

  for(i in 1:length(situation.names)){
    #expected reward prior
    situations[[situation.names[i]]]$agent.bad = agent.bad(beta,situations[[situation.names[i]]],reward)
    situations[[situation.names[i]]]$agent.average = agent.average(beta,situations[[situation.names[i]]],reward)
    situations[[situation.names[i]]]$agent.good = agent.good(beta,situations[[situation.names[i]]],reward)

    exp_r_prior[[situation.names[i]]] = situations[[situation.names[i]]] %>%
      summarise(exp_r_prior.bad = sum((agent.bad*outcome + ((1-agent.bad)*outcome_counterfactual))*probability.overall),
                exp_r_prior.average = sum((agent.average*outcome + ((1-agent.average)*outcome_counterfactual))*probability.overall),
                exp_r_prior.good = sum((agent.good*outcome + ((1-agent.good)*outcome_counterfactual))*probability.overall))

    #rescale expected prior rewards
    exp_r_prior_scaled[[situation.names[i]]] = rescale(exp_r_prior[[situation.names[i]]] %>% as.numeric(),to=c(0,1),from = range(exp_r_prior[[situation.names[i]]]))
  }
  return(exp_r_prior_scaled)
}

difference_in_expected_reward = function(){
  inference.names = c(rep("goalie_spinner",2),"gardener")
  experiment.names = c("goalie","spinner","gardener")
  exp_r_difference.list = list()
  for(i in 1:length(inference.list)){
    df.tmp = inference.list[[i]] %>%
      group_by(situation,agent) %>%
      summarise(rating = mean(rating)/100) %>%
      ungroup %>%
      spread(agent,rating) %>%
      mutate(exp_r_prior_sum = sum(prior.list[[names(inference.list[i])]]*exp_r_prior_scaled[[inference.names[i]]]), #prior expected reward
             exp_r_posterior = rowSums(matrix(c(bad_agent,average_agent,good_agent),ncol=3) *
                                         matrix(unlist(rep(exp_r_prior_scaled[[inference.names[i]]],8)),byrow=T,ncol=3)), #posterior expected reward
             exp_r_difference = exp_r_posterior - exp_r_prior_sum#expected reward difference
             )

    exp_r_difference.list[[names(inference.list[i])]] = select(df.tmp,situation,exp_r_difference)
  }
  return(exp_r_difference.list)
}

#+ Run (Models with difference in expectation) -----------------------------------------------------------------------------------------
#' ## Run (Models with difference in expectation)

reward = 1
# nsims = 100
nsims = 2

set.seed(5)
seeds = sample(nsims,nsims)

experiment.names = c("goalie", "spinner", "gardener")

cv.results = list()

# Models which include difference in expectations

for (i in 1:nsims){

  set.seed(seeds[i])
  cases = sample(1:nrow(df.cv))[1:(nrow(df.cv)/2)] #split half cross-validation

  df.training = df.cv %>%
    filter(row_number() %in% cases) %>%
    arrange(experiment)

  df.test = df.cv %>%
    filter(!row_number() %in% cases) %>%
    arrange(experiment)

  df.tmp = df.training
  fit.prior = optim(par = runif(7,0,1),fit_prior_cv,method = "L-BFGS-B",
                    lower = rep(0, 7), upper = c(100,rep(1,6)))

  beta = fit.prior$par[1]
  prior.list = list()
  prior.list[["goalie"]] = c(fit.prior$par[2],fit.prior$par[3],1-sum(fit.prior$par[2],fit.prior$par[3]))
  prior.list[["spinner"]] = c(fit.prior$par[4],fit.prior$par[5],1-sum(fit.prior$par[4],fit.prior$par[5]))
  prior.list[["gardener"]] = c(fit.prior$par[6],fit.prior$par[7],1-sum(fit.prior$par[6],fit.prior$par[7]))

  exp_r_prior_scaled = expected_reward()
  difference = difference_in_expected_reward()

  differences = data.frame()

  for (j in 1:length(difference)){
    tmp = difference[[j]]
    differences = rbind(differences,tmp)
  }

  differences = differences %>%
    mutate(experiment = rep(experiment.names,each=8),
           # situation.index = rep(1:8,4))
           situation.index = rep(1:8,3))

  df.tmp = df.tmp %>%
    merge(.,differences %>% select(experiment,situation.index,exp_r_difference))

  # CHOOSE WHICH MODEL TO RUN HERE
  # model = "rating~pivotality+exp_r_difference"
  model = "rating~exp_r_difference"
  fit.regression = lm(formula = paste0(model),data=df.tmp)

  model.fit = df.test %>%
    merge(.,differences %>% select(experiment,situation.index,exp_r_difference)) %>%
    mutate(prediction = predict(fit.regression,.)) %>%
    mutate_at(vars(rating,prediction),funs(abs)) %>%
    summarise(r = cor(rating,prediction),
              rmse = sqrt(mean((rating-prediction)^2))
              )

  cv.results[['parameters']][[i]] = as.data.frame(matrix(fit.prior$par,nrow=1)) %>%
  setNames(c('beta','goalie_bad','goalie_average','spinner_bad','spinner_average','gardener_bad','gardener_average'))
  cv.results[['regression']][[i]] = as.data.frame(matrix(fit.regression$coefficients,nrow=1))
  cv.results[['model']][[i]] = model.fit
  cv.results[['training']][[i]] = cases
}

#+ Run (Models without difference in expectation) -----------------------------------------------------------------------------------------
#' ## Run (Models without difference in expectation)

reward = 1
# nsims = 100
nsims = 2

set.seed(5)
seeds = sample(nsims,nsims)
cv.results = list()

# Models which include difference in expectations
for (i in 1:nsims){

  set.seed(seeds[i])
  cases = sample(1:nrow(df.cv))[1:(nrow(df.cv)/2)] #split half cross-validation

  df.training = df.cv %>%
    filter(row_number() %in% cases) %>%
    arrange(experiment)

  df.test = df.cv %>%
    filter(!row_number() %in% cases) %>%
    arrange(experiment)

  # model = "rating~pivotality"
  model = "rating~optimality*outcome"
  fit.regression = lm(formula = paste0(model),data=df.training)

  model.fit = df.test %>%
    mutate(prediction = predict(fit.regression,.)) %>%
    mutate_at(vars(rating,prediction),funs(abs)) %>%
    summarise(r = cor(rating,prediction),
              rmse = sqrt(mean((rating-prediction)^2))
    )
  cv.results[['regression']][[i]] = as.data.frame(matrix(fit.regression$coefficients,nrow=1))
  cv.results[['model']][[i]] = model.fit
  cv.results[['training']][[i]] = cases
}

#+ Summarize the results (run for each model separately) -----------------------------------------------------------------------
#' ## Summarize the results (run for each model separately)

df.parameters = data.frame()

for(i in 1:nsims){
  df.parameters = rbind(df.parameters,cbind(cv.results$regression[[i]],cv.results$model[[i]]))
}

df.parameters %>%
  mutate(z = fisherz(r)) %>%
  summarize(r_mean = fisherz2r(mean(z)),
            r_quantile_low = fisherz2r(quantile(z,probs = c(0.05))),
            r_quantile_high =fisherz2r(quantile(z,probs = c(0.95))),
            rmse_mean = mean(rmse),
            rmse_quantile_low = quantile(rmse,probs = c(0.05)),
            rmse_quantile_high = quantile(rmse,probs = c(0.95))) %>%
  round(2) %>% 
  kable(caption = "Cross-validation results")
