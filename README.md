# Fish_FlowIntermittency

This code was produced for the publication Gilbert, E.I., Turner, T.F., Moses, M.E. and Webster, A.J., "Drying regime characteristics predict riverine fish occurrence and density". All data used in this publication is publically available. The river drying data can be obtained from the website https://reyes.gsanalysis.com/. The fish data may be obtained through a Freedom of Information Act request to the U.S. Bureau of Reclamation Office in Albuquerque, NM. Overall, this analysis predicted annual autumn fish occurrence and densities as a function of three river drying regime dimensions (magnitude, duration, rate of change) using Bayesian hurdle models, which account for a high frequency of zeros in the data. The analysis also uses a functional linear regression to identify when (5-day time step) during the irrigation season do these river drying regime dimensions influence the autumn fish abundance

This scripts in this repository include:
01_ data wrangling of the fish and river drying data
02_ Bayesian hurdle models (n=9) and leave-one-out cross validation for model comparison
03_ linear functional regression analysis (one script that loops through all species x river reach x river drying dimension and one obtains the results just for species with significant effects)
04_ plotting manuscript figures (river drying regime overtime, species life-history traits, hurdle model predictions, and significant linear functional regressions)

