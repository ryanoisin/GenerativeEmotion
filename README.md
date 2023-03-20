### README

This project contains the computational model developed and evaluated in "Towards a Generative Model for Emotion Dynamics" (preprint: <https://psyarxiv.com/x52ns>). This project can be used to simulate data from the model and investigate the behaviour implied by the model

### Folders

-   `/R` contains functions to specify the computational model (`model_matrices.R`), and generate data from the model (`datagen.R`).
-   `/man` contains manual files for these functions

### Scripts

-   `Showcase.Rmd` illustrates how to simulate data from the model


### R

-   `model_matrices.R` functions to specify all model components
-   `datagen.R` function to simulate data from the generative model, using the output of `model_matrices()`
-   `helper_functions.R` contains functions are used by `model_matrices()` to derive additional model-implied relationships outputted by that function.