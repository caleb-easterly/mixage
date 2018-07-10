This R package contains functions used in "Revisiting Assumptions about Age Preferences in Mathematical Models of Sexually Transmitted Infection" (Easterly, et al., 2018) to estimate age mixing in heterosexual populations. To obtain it on your local R installation, install the `devtools` package and then run `devtools::install_github("caleb-easterly/mixage")`. 

## Age mixing: Natsal-3 data
To use our estimates of age mixing with your own age groups, define a vector of the minimum age in each age group (age mixing is only supported for ages 12 and up):
```
your_age_groups <- c(12, 20, 30, 40, 50, 60)
```
Then, run `define_age_group_matrix()` with your age groups, the maximum age in your population (the max age must be 74 or below), and (optionally), a vector of length 99 with the age distribution from ages 1 to 99.
```
mixage <- define_age_group_matrix(your_age_groups, max_age = 74)
```
If an age distribution is not provided, the [U.K. 2010-2012 life tables](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables) are used to define an age distribution. The function returns a list of matrices 
```
> str(mixage)
List of 2
 $ MOME: num [1:6, 1:6] 0.9613 0.3045 0.0622 0.0242 0.0117 ...
 $ FOME: num [1:6, 1:6] 0.62054 0.10251 0.03308 0.01485 0.00777 ...
```

where `MOME` is the male age mixing matrix and `FOME` is the female age mixing matrix. The $i$th row of `FOME` is the partner age distribution for females in age group $i$, and `FOME[i, j]` is the probability that a female of age group `i` chooses a male partner in age group `j`. 

## Calculate age mixing with your data

The other main functionality of the package is to estimate age mixing using your data. To use this function, your data must be in a dataframe with columns `chsage`, `ptage`, `sex`, and (optionally) `weights`, where each row is a partnership where the individual reporting the partnership (in a survey) has age `chsage` and the reported age of their partner is `ptage`. The variable `sex` should be coded as `Male` and `Female`. A sample (toy) dataset is provided, which can be accessed with `data('mixage_sample_data')`. As an example, we can run the following:

```
start_ages <- seq(12, 60, by = 5)
data('mixage_sample_data')
estimates <- estimate_age_mixing(mixage_sample_data,
    start_ages = start_ages, 
    distribution = "normal")
```

For more details about the arguments, see `?estimate_age_mixing` once the package is installed. 

## Best age mixing for your data

Finally, we can evaluate age mixing models using the Akaike information criterion for any given dataset, using `best_age_mixing()`. This function calculates the best model (with the lowest AIC), then returns it. 

```
start_ages <- seq(12, 60, by = 5)
data('mixage_sample_data')
best <- best_age_mixing(mixage_sample_data,
    start_ages = start_ages)
```

The result is:
```
> best$all_AIC
  distribution     link      AIC
1       normal identity 4988.392
2        gamma      log 6274.510
3        gamma identity 5532.356
```

This is reassuring, as the data were sampled from a normal distribution (see the [data creation file](R/create_sample_data.R))

## Contact
Developer and Maintainer: Caleb Easterly (easte080@umn.edu)

Contributors: [Fernando Alarid-Escudero](https://github.com/feralaes), Szu-Yu (Zoe) Kao

Please contact me with issues, questions, and suggestions (or open an issue on Github).
