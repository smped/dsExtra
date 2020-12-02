# dsExtra

This is a simple R package with a handful of functions, designed for the 
specific drug screen workflows as commonly performed in the Dame Roma Mitchell
Cancer Research Laboratories.
All functions are highly specific, so are not likely to be particularly useful 
to the wider community.

To install, I highly recommend using the BiocManager package which can be 
installed as follows:

```
biocInstalled <- "BiocManager" %in% rownames(installed.packages())
if (!biocInstalled) install.packages("BiocManager")
```

An advantage of the `BiocManager` package is that it allows for easy package
installation from [CRAN](https://cran.r-project.org/), 
[Bioconductor](https://bioconductor.org/) or [github](https://github.com/).
Once installed, simply use the following code to install this package:

```
BiocManager::install("steveped/dsExtra")
```
