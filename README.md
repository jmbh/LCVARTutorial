## Tutorial: Estimating LCVAR Models

This repository contains files for a fully reproducible tutorial on estimating Latent Class Vector Autoregressive (LCVAR) Models with the R-package [ClusterVAR](https://cran.r-project.org/web/packages/ClusterVAR/index.html). We reanalyze open data by [Grommisch et al. (2020)](https://psycnet.apa.org/record/2019-51325-001), who helpfully shared their materials and data [here on OSF](https://osf.io/r7jw6/).

The tutorial is in the following paper:

XXX


This repository contains the following files/folders:

- `Preprocess_Grommisch2020.R` takes the data of Grommisch et al. in the state in which the author shared it on OSF and transforms it into a clean data frame containing only the relevant variables, which is saved as `Data_Grommisch2020_clean.RDS` into the folder `Files`
- `LCVAR_Tutorial.R` contains the code for the entire tutorial
- `Helpers.R` contains helper functions for the tutorial, mostly for plotting figures
- `/Data` is the folder containing the empirical data
- `/Files` is the folder where we save files such as the preprocessed data or LCVAR model objects 
- `/Figures` is the folder in which we plot PDF figures



