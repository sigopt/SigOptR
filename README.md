# SigOptR
SigOpt's public R client

# Installation

```
# install.packages("devtools")
library(devtools)
install_github("sigopt/SigOptR")
library(SigOptR)
```

# Usage

Create an Experiment
```
experiment <- create_experiment(list(
  name="R test experiment", 
  parameters=list(
    list(name="x1", type="double", bounds=list(min=0, max=100)), 
    list(name="x2", type="double", bounds=list(min=0, max=100))
  )
))
```

Receive a Suggestion
```
suggestion <- create_suggestion(experiment$id)
```

Evaluate Your Metric (implement this)
```
values <- evaluate_metric(suggestion)
```

Report an Observation 
```
create_observation(experiment$id, list(suggestion=suggestion$id, value=value))
```

(optional variant) Report an Observation with standard deviation
```
create_observation(experiment$id, list(suggestion=suggestion$id, value=value, value_stddev=value_standard_deviation))
```
