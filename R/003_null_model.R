### ##########################################################
### Load the libraries
### ##########################################################

library(rstan)
library(tidyverse)
library(rio)

here::i_am("R/003_null_model.R")

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
}

parameters {
  // real alpha;  // 
  real c0;
  real<lower=0>skip1;
  real<lower=0>skip2;
}
transformed parameters {
  ordered[K - 1] c;
  c[1] = c0;
  c[2] = c0 + skip1;
  c[3] = c[2] + skip2;
  c[4] = c[3] + skip1;
}

model {
  // set up the linear predictor
  // Prior model
//  alpha ~ normal(0, 10);
//  c0 ~ normal(-1, 2.5);
//  skip1 ~ std_normal();
//  skip2 ~ std_normal();

  // Observational model
  Y ~ ordered_probit(rep_vector(0.0, N), c);
}

generated quantities {
  int<lower=1, upper=K> Y_ppc[N];
  int<lower=0, upper=1> correct_pred[N];
  real pcp;
  for (n in 1:N) {
    Y_ppc[n] = ordered_probit_rng(0, c);
    correct_pred[n] = (Y_ppc[n] == Y[n]);
  }
  pcp = mean(to_vector(correct_pred));
}
"

writeLines(stan_code, con = here::here("working", "null.stan"))

fit <- stan(file = here::here("working", "null.stan"),
            data = data_list,
            seed = 123,
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95),
            refresh = 250 # print update every 250 iters
            )

saveRDS(fit, file = here::here("working", "null_fit.rds"))

s <- summary(fit)$summary
pcp <- s |>
    as.data.frame() |>
    rownames_to_column("par") |>
    filter(par == "pcp")


yrep <- s |>
    as.data.frame() |>
    rownames_to_column("par") |>
    filter(grepl("Y_ppc", par))

alpha <- s |>
    as.data.frame() |>
    rownames_to_column("par") |>
    filter(par == "alpha")

cp <- s |>
    as.data.frame() |>
    rownames_to_column("par") |>
    filter(grepl("^c\\[",par))

## other_pars <- other_pars |>
##     mutate(par = dplyr::recode(par,
##                                "party_fx[1]" = "Cons",
##                                "party_fx[2]" = "Green",
##                                "party_fx[3]" = "Lab",
##                                "party_fx[4]" = "LDem",
##                                "party_fx[5]" = "Plaid",
##                                "party_fx[6]" = "SNP",
##                                "region_fx[1]" = "E Midlands",
##                                "region_fx[2]" = "E of England",
##                                "region_fx[3]" = "London",
##                                "region_fx[4]" = "North East",
##                                "region_fx[5]" = "North West",
##                                "region_fx[6]" = "Scotland",
##                                "region_fx[7]" = "South East",
##                                "region_fx[8]" = "South West",
##                                "region_fx[9]" = "Wales",
##                                "region_fx[10]" = "W Midlands",
##                                "region_fx[11]" = "Yorks. and Humber"))
                               
## ggplot(other_pars, aes(x = par, y = mean, ymin = `2.5%`, ymax = `97.5%`)) +
##     geom_pointrange() +
##     coord_flip()

## pairs(fit, pars = c("beta_con19", "beta_lab19", "sigma_region", "sigma_party"))
## pairs(fit, pars = c("party_fx", "region_fx"))
## s <- summary(fit)$summary

## s <- s |>
##     as.data.frame() |>
##     rownames_to_column("par")

## s |>
##     filter(!grepl("alpha", par)) |>
##     arrange(n_eff) |>
##     head()


## ## ### Plot the quantities
## plot.df <- s |>
##     filter(grepl("theta", par)) |>
##     mutate(mp_idx = as.numeric(gsub("[^0-9]", "", par)))

## ### Add on the information
## canon <- canon |>
##     mutate(mp_idx = 1:n())

## plot.df <- left_join(plot.df, canon)

## plot.df$Name <- sub(" \\(.*", "", plot.df$DisplayName)

## ggplot(plot.df, aes(x = mean,
##                     fill = Party)) +
##     facet_wrap(~Party ) + 
##     geom_histogram()
                    
## ggplot(plot.df |> sample_n(100) |>
##        arrange(mean) |>
##        mutate(Name = fct_inorder(Name)), aes(x = Name,
##                     y = mean,
##                     ymin = `2.5%`,
##                     ymax = `97.5%`,
##                     colour = Party)) +
##     geom_pointrange() +
##     coord_flip() +
##     theme_bw()

## plot.df |>
##     group_by(Party) |>
##     summarize(meanval = mean(mean),
##               sdval = sd(mean))


## ### JND calculation
## threshold_3 <- rstan::extract(fit, "c")[[1]][,3]

## ### Degree of overlap
## a <- plot.df |> filter(Party == "Conservative") |> pull(mean)
## b <- plot.df |> filter(Party == "Labour") |> pull(mean)

## lower <- min(c(a, b)) - 1 
## upper <- max(c(a, b)) + 1

## # generate kernel densities
## da <- density(a, from=lower, to=upper)
## db <- density(b, from=lower, to=upper)
## d <- data.frame(x=da$x, a=da$y, b=db$y)

## # calculate intersection densities
## d$w <- pmin(d$a, d$b)

## # integrate areas under curves
## library(sfsmisc)
## total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
## intersection <- integrate.xy(d$x, d$w)

## # compute overlap coefficient
## overlap <- 2 * intersection / total

## ## ### 
## ### Degree of overlap
## a <- plot.df |> filter(Party == "Scottish National Party") |> pull(mean)
## b <- plot.df |> filter(Party == "Labour") |> pull(mean)

## lower <- min(c(a, b)) - 1 
## upper <- max(c(a, b)) + 1

## # generate kernel densities
## da <- density(a, from=lower, to=upper)
## db <- density(b, from=lower, to=upper)
## d <- data.frame(x=da$x, a=da$y, b=db$y)

## # calculate intersection densities
## d$w <- pmin(d$a, d$b)

## # integrate areas under curves
## library(sfsmisc)
## total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
## intersection <- integrate.xy(d$x, d$w)

## # compute overlap coefficient
## overlap <- 2 * intersection / total

## ### Get the rank information

## thetas <- rstan::extract(fit, "theta")[[1]]

## ###
## ranks <- apply(thetas, 1, rank)
## meanranks <- apply(ranks, 1, function(x) {
##     data.frame(best_guess = round(mean(x)),
##                lo_as = quantile(x, 0.025),
##                hi_as = quantile(x, 1 - 0.025))
## })

## meanranks <- do.call("rbind", meanranks)

## canon2 <- cbind(canon, meanranks)

## canon2 |>
##     arrange(desc(best_guess)) |>
##     head()

## canon2 |>
##     arrange(best_guess) |>
##     head()
