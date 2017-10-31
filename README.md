This R package contains functions used in "Name of the HPV Modeling Paper", 2018. To obtain it on your local R installation, install the `devtools` package and then run `devtools::install_github(caleb-easterly/sfceHPV)`. 

## Steady-State Prevalence
To estimate the steady-state prevalence from a model with the default parameters, no vaccination, and the empirical mixing structure, run the following code:
```
parms <- all_parameters()
prev <- estimate_steady_state(parms = parms, mix = "emp", vacc_strategy = parms$phi0)
```

The resulting variable `prev` is a matrix containing age- and gender-specific prevalence. To compare two vaccination strategies, run `estimate_steady_state()` with the two strategies. 

## Cases of HPV
To estimate cases of HPV over 50 years, comparing two vaccination strategies (`JAstratF_base` and `JAstratF_comp`), run the following code:
```
parms <- all_parameters()
cases_prevnt <- cases_prevented(parms,
  base_vacc = parms$JAstratF_base,
  addnl_vacc = parms$JAstratF_comp)
```

The resulting variable, `cases_prevnt`, is a data frame containing the number of cases per person over 50 years for each age, gender, vaccination strategy, and mixing structure. 

## Program Structure

The three essential top-level functions are `all_parameters()`, `cases_prevented()` and `estimate_steady_state()`, which together call most of the other functions in the package. Here, we describe the structure of these functions.
The symbol `->` indicates that the parent function calls the child function. The symbol `<-` indicates a return to the parent function, optionally followed by what is returned, named informally and incompletely to describe roughly what they are. Functions written in C++ are marked with a`*`.  

### `all_parameters()`

```
all_parameters()
    -> age_specific_parameters()
        -> calculate_age_distribution() # estimates the proportion of population in each age group
            -> death_rate() # convert life tables to death rates
                <- vector of death rates
            -> transfer_rate() # calculate aging rate from death rates
                <- aging rates
            <- list(age_proportions, aging rate, death rate)
        -> calculate_age_distribution() # estimate population proportion in each *year of age*
            ** same as above **
            <- list(age_proportions, aging rate, death rate)
        -> estimate_spar() # estimate the sex partner acquisition rate for each age group
            -> estimate_spar_prepare_data() # load spar data from private natsal dataframe
                -> spar_all() # divide data for each age into high, medium, low sexual activity
                <- spar data, divided into high, medium, low SA
            -> estimate_spar_poisson_reg() # do regression on data to estimate spar
            <- adjusted SPARs
        -> pt_choice_all_choose_variance_model() # use regression to estimate age mixing
            -> gam_param() # estimate shape and scale of Gamma distributions
                <- shape, scale
            -> laplace_mle() # estimate mean, variance of laplace distribution
                <- best estimates
            -> laplace_MOM() # convert mean, variance to location + scale
                <- location, scale
            <- age mixing matrices
        <- list of all age-specific parameters
    -> data_matrix_longform() # bin partnership data into age groups
        <- data matrices with group-specific partnership data
    -> calc_ap_eps() # estimate epsilon with maximum likelihood fit to data
        -> optim(eps_mle_fast()) # calculate epsilons with MLE
            -> eval_rho_ap() # calculate full A-P mixing matrix for given values of epsilon
                -> sub_epsilon() # substitute values of epsilon into list of parameters
                    <- new parameter list
                -> lambda_all()* # function calculates force of infection, returns A-P matrices as side effect
                    -> balance_spar() # balances the acquisition rates so all partnerships are 'realized'; called once for A-P, once for empirical
                        <- balanced partnership rates
                    <- A-P mixing matrices (among many other things)
                <- A-P mixing matrices
            -> calculate_ap_age_mixing_matrix() # calculate A-P *age* matrix from full A-P matrix
                <- A-P age matrix
            -> mixing_matrix_loglikelihood() # calculate log likelihood of data under age mixing matrix
                <- (positive) log likelihood of data
            <- best fit epsilons
        
        <- best fit age assortativity parameters
    -> define_journal_article_vaccination_strategies()
        -> def_vacc_strat() # define vaccination matrices based on a few probabilities; called several times
            <- a vaccination matrix
        <- vaccination strategies
    <- all defined parameters
```

### `estimate_steady_state()`

```
estimate_steady_state()
    -> lsodar( hpv_model_function()* ) # solves system of ODEs in hpv_model_function()
        -> lambda_all()* # calculate force of infection (lambda)
            -> balance_spar() # balances the acquisition rates so all partnerships are 'realized'; called once for A-P, once for empirical
                <- balanced partnership rates
            <- age-, gender-, and sexual-activity-specific force of infection
        <- matrix of prevalence at steady-state
    -> combine_prevalence() # calculate age- and gender-specific prevalence from model output; averages over SA groups
        <- age- and gender-specific prevalence
```

### `cases_prevented()`

```
cases_prevented()
    -> caseprev_base_vec() # run model to steady state
        -> lsodar( hpv_model_function() )
            ...etc...
            <-
        <- initial conditions for empirical mixing
    -> caseprev_base_vec() # run model to steady state
        -> lsodar( hpv_model_function() )
            ...etc...
            <-
        <- initial conditions for A-P mixing
    -> cases_cumulative()
        -> lsoda ( hpv_model_function() )
            ...etc...
            <- 
        <- empirical mixing; cases over 50 years under base vaccination
    -> cases_cumulative()
        -> lsoda ( hpv_model_function() )
            ...etc...
            <- 
        <- A-P mixing; cases over 50 years under base vaccination
    -> cases_cumulative()
        -> lsoda ( hpv_model_function() )
            ...etc...
            <- 
        <- empirical mixing; cases over 50 years under *comparison* vaccination plan
    -> cases_cumulative()
        -> lsoda ( hpv_model_function() )
            ...etc...
            <- 
        <- A-P mixing; cases over 50 years under *comparison* vaccination plan
```




## Contact
Developer and Maintainer: Caleb Easterly (easte080@umn.edu)

Please contact me with issues, questions, and suggestions (or open an issue on Github). Feel free to open pull requests and contribute! 
