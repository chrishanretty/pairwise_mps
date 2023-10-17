### ##########################################################
### Load the libraries
### ##########################################################

library(rstan)
library(tidyverse)
library(rio)

here::i_am("R/002_model.R")

set.seed(1582)

### ##########################################################
### Data in 
### ##########################################################

dat <- readRDS(file = here::here("working", "tidied_data.rds")) |>
    filter(!is.na(dv))

canon <- read.csv(here::here("data", "canonical_representation.csv")) |>
    dplyr::select(DisplayName, Party, PCON22CD, Person.ID,
                  SocialistCampaignGroup, TribuneGroup,
                  OneNation, TRG_Patron, ERG_Subscriber) |>
    filter(!grepl("^N", PCON22CD))

### Restrict to the MPs who are in the data
canon <- canon |>
    filter(DisplayName %in% unique(c(dat$MP_A, dat$MP_B)))
           
dat$MP_A.num <- match(dat$MP_A, canon$DisplayName)
dat$MP_B.num <- match(dat$MP_B, canon$DisplayName)

### Get in region and lagged vote shares
aux <- rio::import(here::here("data", "BES-2019-General-Election-results-file-v1.1.xlsx"))

aux <- aux |>
    dplyr::select(ONSConstID, Region, Con19, Lab19)

### Make sure that the auxiliary data frame is ordered like the canon data frame
aux <- aux[match(canon$PCON22CD, aux$ONSConstID),]

### Make one tiny change to the Conservative vote share (where missing)
aux <- aux |>
    mutate(Con19 = coalesce(Con19, 0.0))


### Make some changes to parties
canon <- canon |>
    mutate(Party = case_when(DisplayName == "Jeremy CORBYN (Islington North)" ~ "Labour",
                             DisplayName == "Jonathan EDWARDS (Carmarthen East and Dinefwr)" ~ "Plaid Cymru",
                             DisplayName == "Margaret FERRIER (Rutherglen and Hamilton West)" ~ "Scottish National Party",
                             DisplayName == "Rob ROBERTS (Delyn)" ~ "Conservative",
                             DisplayName == "Claudia WEBBE (Leicester East)" ~ "Labour",
                             DisplayName == "Lindsay HOYLE (Chorley)" ~ "Labour",
                             Party == "Alba" ~ "Scottish National Party",
                             Party == "Labour/Co-operative" ~ "Labour",
                             TRUE ~ Party))


data_list <- list(N = nrow(dat),
                  Y = dat$dv,
                  K = 5,
                  N_MPs = nrow(canon),
                  N_resps = length(unique(dat$resp_id)),
                  idx_resp = dat$resp_id,
                  idx_a = dat$MP_A.num,
                  idx_b = dat$MP_B.num,
                  N_parties = n_distinct(canon$Party),
                  N_regions = n_distinct(aux$Region),
                  mp_party = as.numeric(factor(canon$Party)),
                  mp_is_lab = as.numeric(canon$Party == "Labour"),
                  mp_is_libdem = as.numeric(canon$Party == "Liberal Democrat"),
                  mp_is_plaid = as.numeric(canon$Party == "Plaid Cymru"),
                  mp_is_snp = as.numeric(canon$Party == "Scottish National Party"),
                  mp_is_green = as.numeric(canon$Party == "Green"),
                  mp_region = as.numeric(factor(aux$Region)),
                  con19 = as.vector(scale(aux$Con19 / 100, scale = FALSE)),
                  lab19 = as.vector(scale(aux$Lab19 / 100, scale = FALSE)),
                  prior_only = 0)


### Code here taken from https://betanalpha.github.io/assets/case_studies/ordinal_regression.html
stan_code <- "
data {
  int<lower=1> N;             // Number of observations
  int<lower=1> K;             // Number of ordinal categories
  int<lower=1,upper=K> Y[N];  // response variable
  int<lower=1> N_MPs;
  int<lower=1> N_parties; 
  int<lower=1, upper=N_MPs> idx_a[N]; // link responses to MPs
  int<lower=1, upper=N_MPs> idx_b[N]; // link responses to MPs
  int<lower=1, upper=N_parties> mp_party[N_MPs];
  // series of dummy variables (Con = ref. cat.)
  int<lower=0, upper=1> mp_is_lab[N_MPs];
  int<lower=0, upper=1> mp_is_libdem[N_MPs];
  int<lower=0, upper=1> mp_is_snp[N_MPs];
  int<lower=0, upper=1> mp_is_plaid[N_MPs];
  int<lower=0, upper=1> mp_is_green[N_MPs];
}

parameters {
  vector[N_MPs] alpha;  // (residual) latent trait
  vector[N_parties - 1] party_fx;
  real<lower=0> sigma_alpha; // SD on residual latent trait
  real c0; // initial threshold
  real<lower=0>skip1; 
  real<lower=0>skip2;
}
transformed parameters {
  vector[N_MPs] theta;
  ordered[K - 1] c;
  c[1] = c0;
  c[2] = c0 + skip1;
  c[3] = c[2] + skip2;
  c[4] = c[3] + skip1;
  for (l in 1:N_MPs) {
     theta[l] = alpha[l] +
       party_fx[1] * mp_is_lab[l] +
       party_fx[2] * mp_is_snp[l] +
       party_fx[3] * mp_is_libdem[l] +
       party_fx[4] * mp_is_plaid[l] +
       party_fx[5] * mp_is_green[l];
  }
}

model {
  // set up the linear predictor
  // has to be at the top of the block
  vector[N] mu = rep_vector(0.0, N);

  // Prior model
  alpha ~ normal(0, sigma_alpha);
  c0 ~ normal(-1, 1);
  skip1 ~ std_normal();
  skip2 ~ std_normal();
  party_fx ~ normal(0, 2.5);
  sigma_alpha ~ std_normal(); // half-normal b/c of constraints

  // Observational model
  for (n in 1:N) {
    mu[n] = theta[idx_a[n]] - theta[idx_b[n]];
  }

  // outcome
  Y ~ ordered_logistic(mu, c);
}

generated quantities {
  int<lower=1, upper=K> Y_ppc[N];
  // create a counter for the number of correct predictions
  int correct_pred = 0;
  real pcp;
  
  for (n in 1:N) {
    real mu = theta[idx_a[n]] - theta[idx_b[n]];
    Y_ppc[n] = ordered_logistic_rng(mu, c);
    // increment the counter holding correct preds
    correct_pred += (Y_ppc[n] == Y[n]);
  }

  // calculate percentage correctly predicted
  // multiply by 1.0 to promote from int to real
  pcp = (correct_pred * 1.0) / (N * 1.0);
}
"

writeLines(stan_code, con = here::here("working", "partyonly.stan"))

fit <- stan(file = here::here("working", "partyonly.stan"),
            data = data_list,
            seed = 123,
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95,
                           max_treedepth = 14),
            refresh = 250 # print update every 250 iters
            )

saveRDS(fit, file = here::here("working", "partyonly_fit.rds"))

### Create the data frame with MPs, their details, and their values
s <- summary(fit)$summary |>
                as.data.frame() |>
                rownames_to_column("par")

saveRDS(s, file = here::here("working", "partyonly_smry.rds"))

theta <- s |>
    filter(grepl("theta", par)) |>
    mutate(mp_idx = as.numeric(gsub("[^0-9]", "", par)))


theta <- cbind(theta,
               canon[theta$mp_idx,])

theta <- cbind(theta,
               aux[theta$mp_idx,])

saveRDS(theta, file = here::here("working", "partyonly_thetas.rds"))

### Work on an efficient save for Shiny

theta <- rstan::extract(fit, "theta")[[1]] |>
    as.data.frame() |>
    rownames_to_column("iter") |>
    pivot_longer(cols = starts_with("V")) |>
    mutate(idx = as.numeric(sub("V", "", name)))

### Add on DisplayName and Party
theta <- cbind(theta,
               canon[theta$idx, c("DisplayName", "Party")])

### Remove the iter, name and index variables
theta <- theta |>
    dplyr::select(value, DisplayName, Party)

### Separate DisplayName into Name and Constituency
theta <- theta |>
    separate_wider_delim(DisplayName,
                         names = c("Name", "Constituency"),
                         delim = " (",
                         too_many = "merge") |>
    mutate(Constituency = sub("\\)$", "", Constituency))

### Convert character variables to factor
theta <- theta |>
    mutate_if(is.character, factor)

saveRDS(theta, file = here::here("working", "partyonly_theta_iters.rds"))

### Thin it a bit for Shiny
theta <- theta |>
    group_by(Name, Constituency, Party) |>
    sample_n(500) |>
    ungroup()

saveRDS(theta, file = here::here("shiny_pairwise_mps", "partyonly_theta_iters.rds"))

### Save cut-off averages
cutoffs <- rstan::extract(fit, "c")[[1]]
cutoffs <- colMeans(cutoffs)
saveRDS(cutoffs, file = here::here("working", "mean_cutoff.rds"))
