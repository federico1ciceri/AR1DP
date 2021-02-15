# AR1DP
How gender stereotypes changed for adjectives and occupations?

## Table of contents
* [Dataset and model](#Dataset/model)
* [A sketch of the code](#Code)
* [Results](#Results)

## Dataset/model
The focus of this project is on the study of the temporal dynamics of gender stereotypes in adjectives and occupations in the 20th and 21st centuries in the United States.
The available data consists in:
![Dataset](https://github.com/federico1ciceri/AR1DP/blob/main/images/Dataset.png)
Here a brief representation of the model:
![Model](https://github.com/federico1ciceri/AR1DP/blob/main/images/Model.png)


More details on the report included in this github repository.
	
## Code
Sketch of the C++ code (showing the main.cpp):

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
	
	
## Results
To run this project, install it locally using npm:

```
$ cd ../lorem
$ npm install
$ npm start
```
