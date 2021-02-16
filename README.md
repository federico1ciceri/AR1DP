# An AR1-DP model for the gender bias in the USA
How gender stereotypes changed for adjectives and occupations?

## Table of contents
* [Dataset and model](#Dataset/model)
* [Folder structure](#Folders)
* [Before Running](#Instructions)
* [A sketch of the code](#Code)
* [Some results](#Results)

## Dataset/model
The focus of this project is on the study of the temporal dynamics of gender stereotypes in adjectives and occupations in the 20th and 21st centuries in the United States.
The available data consists in:
![Dataset](https://github.com/federico1ciceri/AR1DP/blob/main/images/Dataset.png)
Here a brief representation of the model:
![Model](https://github.com/federico1ciceri/AR1DP/blob/main/images/Model.png)


More details on the report included in this github repository.

## Folders
1. R_script includes:
   - **Analisi output codice**:
     - **adj_jitter_names.Rdata**, **occu_jitter_names.Rdata** _data_
     - **estclusters_adjectives.Rdata**, **estclusters_occupation.Rdata** _cluster estimates from AR1DP_
     - **Code Analysis Occu.R** _R script with posterior analysis on cluster estimates for occupations: cluster visualization, traceplots and validation analysis_
   - **K means**:
     - **adj_jitter_names.Rdata**, **occu_jitter_names.Rdata** _data_
     - **k_means_adj_2_5.R**, **k_means_occu_2_5.R** _R script performing k-means while varying k, visualizing boxplots and co-clustering matrices_
   - **PAM**
     - **adj_jitter_names.Rdata**, **occu_jitter_names.Rdata** _data_
     - **pam_adj.R**, **pam_occu.R** _R script performing PAM clustering, visualizing boxplots and co-clustering matrices_
     
2. **report.pdf** _the full report of the project_


## Instructions
directories
R packages: library(fields)

## Code
Due to copyright restrictions, we omit the full code for the MCMC.
Sketch of the C++ code (teasing the main.cpp):

`for (int i = 0; i< nwarm + nsample; ++i) { `<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`update_s(y, s_initial, mu_initial, sigma_initial, w_initial, L, N, &rng);`<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`update_musigma(musigma_initial, mu_initial, sigma_initial, s_initial, y, sigma_alpha, sigma_beta, mumean, lambda0, L, &rng);`<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`update_allocated_variables(sigma_matrix,sigma_initial, s_initial, T, N);`<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`update_allocated_variables(mu_matrix,mu_initial, s_initial, T, N);`<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`M_initial = update_M(s_initial, N*T, prioralphaforM, priorbetaforM, M_initial, &rng);`<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`psi_initial = update_psi(psi_initial, epsilon_initial, s_initial, M_initial, L, N, T, R, psi_sigma,`<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`botlimforpsi, uplimforpsi, &psi_accept_count, &psi_metro_count, &rng);`<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`epsilon_initial = update_epsilon_gibbs(psi_initial, epsilon_initial, s_initial, M_initial, L, N, T, R, psi_sigma, &rng);`<br />
	`}`
	

```
for (int i = 0; i< nwarm + nsample; ++i) {
	update_s(y, s_initial, mu_initial, sigma_initial, w_initial, L, N, &rng);
	update_musigma(musigma_initial, mu_initial, sigma_initial, s_initial, y, sigma_alpha, sigma_beta, mumean, lambda0, L, &rng);
	update_allocated_variables(sigma_matrix,sigma_initial, s_initial, T, N);
	update_allocated_variables(mu_matrix,mu_initial, s_initial, T, N);
	M_initial = update_M(s_initial, N*T, prioralphaforM, priorbetaforM, M_initial, &rng);
	psi_initial = update_psi(psi_initial, epsilon_initial, s_initial, M_initial, L, N, T, R, psi_sigma, botlimforpsi, uplimforpsi, &psi_accept_count, &psi_metro_count, &rng);
	epsilon_initial = update_epsilon_gibbs(psi_initial, epsilon_initial, s_initial, M_initial, L, N, T, R, psi_sigma, &rng);
}	
```
	
## Results

