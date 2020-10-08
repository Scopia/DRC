
## DRC

### Introduction
The main code is provided in **main.R**, and the other R files contain functions that are called by the main.  
**GaussDataGeneration.R** contains function for data generation of Gaussian mixture distribution.  
**dataPartition.R** contains function for partitioning data set.  
**kernel_func.R** contains function of Gaussian kernel function.  
**modelTrain.R** contains function for trainning DRC.  
**predictDRC.R** contains function for predicting new data using DRC. 
**measureCal.R** contains function for calculate every measures of DRC on test sets.  
**visulization.R** contains function for visulizing the generation data by t-SNE.  
imbalanced datasets from UCI repository to carry out an experiment are provided in **data.zip**.  

### Generation mechanism of data
For "GaussDataGeneration", the generation of covariance matrix $ \Sigma$ is random. We first draw all the coefficients in the covariance matrix as iid samples, e.g. from a normal or a constant distribution. Then we substitute $\Sigma = UDV^{T} $ by $\Tilde{\Sigma} = VDV^{T}$.  
For "DataGeneration", we first generate $q$ independent random variables $Z = (Z_{1},\cdots,Z_{q})$ with arbitrary single or mixed distributions and get our target variable through transformation $X = ZC^{T}$, where $C$ is singular value decomposition of covariance matrix $\Sigma$ of $X$. The generation of $\Sigma$ is random and specific methods can be found in "On the Generation of Random Multivariate Data".
