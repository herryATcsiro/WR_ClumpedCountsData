library(brms)
library(rstan)
library(cmdstanr)
library(tidyverse)
options(cmdstanr_write_stan_file_dir = "models")
options(mc.cores = parallel::detectCores())
#set_cmdstan_path("~/.cmdstan/cmdstan-2.36.0") # adjust to your location
#load data
ds1=read_csv("ClumpedCounts1000.csv")%>%filter(Smple<=50) #adjust to your random sample filename
# setup family and brms formula
fam="gaussian"
f1=bf(Acum~ 0+ CumC)
#prior
p1=c(prior(normal(0, 1), class=b),
          prior(normal(50,1000), class = sigma)
)

#create code for each grid in series
lst<-unique(ds1$name)
out<-lapply(lst, function(f){
  df=ds1%>%filter(name == f)
  fl=make.names(paste0("ExampleN50_",f)) #make filenames, adjust accordingly
  cat(paste0("xxxxxxx ",fl," XXXXXXXXXXXXXXXXXX"),"\n")
  #run for each grid simulation
  fit=brm(f1, data=df,
          family=fam,
          prior = p1, 
          seed=666,
          refresh=1,
          cores=12, # this is for hpc, adjust accordingly including chains and threads
          chains=3, # adjust accordingly
          threads=threading(3), # adjust accordingly
          backend='cmdstanr',    # use cmdstanr backend
          init=1,
          file_refit="always",
          #file_refit="on_change",
          file=fl,
          #control=list(adapt_delta=0.95, max_treedepth = 12), #adjust based on fit quality
          warmup=500,
          iter=1500, #adjust according to fit quality
          output_dir="models/.", #ensure directory exists
          refresh=1, #adjust for what you want to see during execution
          save_pars = save_pars(all=T) #turn off if you don't care about details
  )
}
)
