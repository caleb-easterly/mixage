This R package contains functions used in "Revisiting Assumptions about Age Preferences in
#' Mathematical Models of Sexually Transmitted Infection" (Easterly, et al., 2018) to estimate age mixing. To obtain it on your local R installation, install the `devtools` package and then run `devtools::install_github(caleb-easterly/mixage)`. 

## Age mixing: Natsal-3 data
To use our estimates of age mixing with your own age groups, define a vector of the minimum age in each age group (age mixing is only supported for ages 12 and up):
```
> your_age_groups <- c(12, 20, 30, 40, 50, 60)
```
Then, run `define_age_group_matrix()` with your age groups, the maximum age in your population (the max age must be 74 or below), and (optionally), a vector of length 99 with the age distribution from ages 1 to 99.
```
> mixage <- define_age_group_matrix(your_age_groups, max_age = 74)
```
If an age distribution is not provided, the U.S. 2011 life tables are used to define an age distribution. The function returns a list of matrices 
```
> str(mixage)
List of 2
 $ MOME: num [1:6, 1:6] 0.9613 0.3045 0.0622 0.0242 0.0117 ...
 $ FOME: num [1:6, 1:6] 0.62054 0.10251 0.03308 0.01485 0.00777 ...
```

where `MOME` is the male age mixing matrix and `FOME` is the female age mixing matrix. The $i$th row of `FOME` is the partner age distribution for females in age group $i$, and `FOME[i, j]` is the probability that a female of age group `i` chooses a male partner in age group `j`. 

## Calculate age mixing with your data

The other main functionality of the package is to estimate age mixing using your data. To use this function, your data must be in a dataframe with columns `chsage`, `ptage`, `sex`, and (optionally) `weights`, where each row is a partnership where the individual reporting the partnership (in a survey) has age `chsage` and the reported age of their partner is `ptage`. The variable `sex` should be coded as `M` and `F`. Assuming we have data stored in the variable `choice_data`, we can run the following:
```
estimates <- estimate_age_mixing(choice_data,
    max_age = 74,
    start_ages = start_ages, 
    distribution = "normal",
    variance_model = "const",
    age_distribution = NULL)
```

For details about all of the arguments mean, see `?estimate_age_mixing` once the package is installed. 


## Contact
Developer and Maintainer: Caleb Easterly (easte080@umn.edu)

Please contact me with issues, questions, and suggestions (or open an issue on Github). Feel free to open pull requests and contribute! 
