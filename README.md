# neastbenchmark
R package containing benchmark data for disease clusters

The package can be installed in R via the command

```
devtools::install_github("jfrench/neastbenchmark")
```
assuming the **devtools** package is installed.

This package contains benchmark data for evaluating the power of disease clustering methods.  The data are based on the female at-risk population in 245 counties (and equivalents) based on the 1990 United States census.  The data consist of regions from the Northeastern United States, including regions in Connecticut, Delaware, the District of Columbia, Maine, Maryland, Massachusetts, New Hampshire, New Jersey, New York, Pennsylvania, Rhode Island, and Vermont.

The observational data were originally used to identify clusters of breast cancer mortality in Kulldorff et al. (1997). The original circular cluster benchmark data (`rural*`, `mixed*`, and `urban*` data sets) were produced by Song and Kulldorff (2003). The irregularly-shaped cluster benchmark data (`a`-`k`) were produced by Duczmal et al. (2006). The remaining irregularly clustered benchmark data (`irural*`, `imixed*`, `iurban*`) were produced by French et al. (2022).

Martin Kulldorff, Eric J. Feuer, Barry A. Miller, Laurence S. Freedman; Breast Cancer Clusters in the Northeast United States: A Geographic Analysis, American Journal of Epidemiology, Volume 146, Issue 2, 15 July 1997, Pages 161–170, https://doi.org/10.1093/oxfordjournals.aje.a009247.

Song, C., & Kulldorff, M. (2003). Power evaluation of disease clustering tests. International Journal of Health Geographics, 2, 9. https://doi.org/10.1186/1476-072X-2-9.

Duczmal, L., Kulldorff, M., & Huang, L. (2006). Evaluation of Spatial Scan Statistics for Irregularly Shaped Clusters. Journal of Computational and Graphical Statistics, 15(2), 428-442. Retrieved from http://www.jstor.org/stable/27594187.

Joshua P. French, Mohammad Meysami, Lauren M.
Hall, Nicholas E. Weaver, Minh C. Nguyen & Lee Panter
(2022) A comparison of spatial scan methods for cluster
detection, Journal of Statistical Computation and
Simulation, 92:16, 3343-3372. https://doi.org/10.1080/00949655.2022.2065676.
