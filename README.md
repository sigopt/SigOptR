# SigOpt R API
The [SigOpt](https://sigopt.com) public R client.

Learn more about SigOpt and R from our [docs](https://sigopt.com/docs/overview/r).

# Installation

```
# install.packages("devtools")
library(devtools)
install_github("sigopt/SigOptR")
library(SigOptR)
```

# Authentication

Set the environment variable `SIGOPT_API_TOKEN` to the value of the API Token found on the [API tokens page](https://sigopt.com/tokens) (you may have to sign in).

```
Sys.setenv(SIGOPT_API_TOKEN=sigopt_api_token)
```

# Run Some Code
Run SigOpt's [Optimization Loop](https://sigopt.com/docs/overview/optimization) for a small function:
```
experiment <- create_experiment(list(
  name="Franke Optimization",
  parameters=list(
    list(name="x", type="double", bounds=list(min=0.0, max=1.0)),
    list(name="y", type="double", bounds=list(min=0.0, max=1.0))
  )
))
print(paste(
  "Created experiment: https://sigopt.com/experiment/",
  experiment$id,
  sep=""
))

# Evaluate your model with the suggested parameter assignments
# Franke function - http://www.sfu.ca/~ssurjano/franke2d.html
evaluate_model <- function(assignments) {
  return(franke(assignments$x, assignments$y))
}

# Run the Optimization Loop between 10x - 20x the number of parameters
for(i in 1:20) {
  suggestion <- create_suggestion(experiment$id)
  value <- evaluate_model(suggestion$assignments)
  create_observation(experiment$id, list(
    suggestion=suggestion$id,
    value=value
  ))
}
```

(optional variant) Report an Observation with standard deviation
```
create_observation(experiment$id, list(suggestion=suggestion$id, value=value, value_stddev=value_standard_deviation))
```

# Endpoints

`SigOptR` supports a subset of API endpoints:
 - `create_experiment`
 - `create_suggestion`
 - `create_observation`

These should be sufficient to run SigOpt's [Optimization Loop](https://sigopt.com/docs/overview/optimization).

## General Information

Repository: 2016-2023
