#### SETUP PARAMETERS ####
times = seq(0,50 , by = 0.1)
times_short = seq(0,50 , by = 1)
parms = c(N=1000,       # population size
          beta=1.5,     # transmission rate
          gamma = 0.5,  # recovery rate
          V=0.02,       # vaccination rate
          d= 0)         # % reduction in transmission due to NPIs
start = c (S=995,       # initial susceptible inidividuals 
           I=5,         # initial infected individuals
           R=0,         # initial recovered individuals
           C=0)         # initial number of infections

# pick one value for v and d as an example
pick_v = 0.01
pick_d = 0.3

# choose two sets of parameters to use as examples
m1_beta = 1
m1_gamma = 0.2
m2_beta = 1.15
m2_gamma = 0.4

m1_beta/m1_gamma #R0 for model 1
m2_beta/m2_gamma #R0 for model 2
