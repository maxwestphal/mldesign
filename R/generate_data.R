#' Generate synthetic data for testing purposes
#'
#' @param n_obs (numeric) \cr number of observations
#'
#' @return (data.frame)
#' @export
#'
#' @examples
#' generate_data(200)
#' @importFrom stats rbinom
#' @importFrom stats rnorm
generate_data <- function(n_obs = 20000){

  n_country <- 4
  n_clinic_per_country <- 5
  n_clinic <- n_country*n_clinic_per_country
  n_year <- 5
  prev <- 0.25

  clinics <- letters[1:n_clinic]
  clinic <- sample(clinics, n_obs, replace=TRUE)

  countries <- LETTERS[1:n_country]
  clinic_to_country <- rep(1:n_country, times=n_clinic_per_country)
  country <- sapply(clinic, function(x) countries[clinic_to_country[which(clinics == x)]]  )

  data <- data.frame(
    obs_id = sample(1:n_obs, replace=FALSE),
    clinic = clinic %>% as.factor(),
    country = country %>% as.factor(),
    year = sample(2000 + (1:n_year), n_obs, replace=TRUE),
    response = stats::rbinom(n_obs, 1, prev),
    age = stats::rnorm(n_obs, 60, 5) %>% round(2)
  )


  return(data)


}
