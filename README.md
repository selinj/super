[![Build Status](https://travis-ci.org/selinj/super.svg?branch=master)](https://travis-ci.org/selinj/super)

# super
:tada: An R package implementing supervised scores for cluster analysis results

`super` implements of several supervised metrics and scores for
cluster analysis (i.e. they compare the cluster assignments with ground
truth labels). These scores are agnostic to the clustering method and
to the structure of the data: each function takes in a contingency table,
or a vector of class labels and a vector of cluster assignments, and
returns a score. A handy wrapper function, `super::super`, returns a tidy one-row
dataframe with all implemented stats for a cluster analysis result.

### Scores

- Purity
- Homogeneity
- Completeness
- V measure

### Installation

With `devtools`:
```
install.packages("devtools")  
devtools::install_github("selinj/super")
```
