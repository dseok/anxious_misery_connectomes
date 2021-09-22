# anxious_misery_connectomes
Code repository for "Differential impact of transdiagnostic, dimensional psychopathology on multiple representations of the functional connectome".

Files in /scripts are designed to be deployed on a high performance computing (HPC) cluster. These files perform the multivariate modeling approaches (edgewise regression and BBS).

Files in /paramsets are pre-set parameter sets that instruct the files in /scripts to perform particular analyses and compute models with different parameters.

Files in /outputs are the results of multivariate analyses conducted using the files in /scripts.

Files in /logs are logfiles for the analyses conducted through the files in /scripts.


Files in /data include the raw imaging data (after preprocessing using fmriprep and parcellation to the Power264 atlas), the raw symptom data, the symptom cluster scores, a list of subjects (divided into training and testing partitions), and the results from seed-based correlation analyses (SCA). 


Files in /analysis analyze the outputs of SCA, edgewise regression and BBS models. It includes scripts to perform some hyperparameter tuning (specifically, nfeatures), calculate and control for p-values, plot scatterplots of raw data and plot the performance of BBS with and without healhty comparators. 
