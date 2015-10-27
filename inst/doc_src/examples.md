# Distributed Statistical Modeling

author: BN, DLR, SMG, MB, PWL, Stanford University

## Introduction

We demonstrate the possibility of fitting statistical models
stratified by sites in a manner that brings computation to the data
that may be distributed across sites or more generally, partitioned in
some manner. (For simplicity, we will call these partitions, sites.)
The infrastructure consists of a single master process that issues
queries to worker processes running at each of the sites. A query is
merely a function call, more specifically a request to each site to
evaluate a pre-defined function $f(\beta)$ on the data at that site,
for a given value of parameters $\beta$.  The master process uses
these queries to aggregate and execute an optimization algorithm
resulting in a model fit, the results of which should be
_indistinguishable from_ those that might be obtained if all the data
had been in a single place.  Of course, this assumes a lossless
serialization format, like Google Protocol Buffers for example, but we
make do with JSON for now.  Also, in comparisons below, we don't use
exactly the same iterations as the survival package and so minor
differences will be seen.

The advantages are many, chief among them the fact that no raw data
needs to be shared between sites. The modeling entity, however, can
make an unlimited number of queries of the sites, where each query is
a request to compute a model-specific function for a specified value
of parameters. This may pose a security concern that we ignore for
now. However, it leads to some further interesting questions regarding
what may be learned such computation.

We focus specifically on Cox regression models in this exercise.

> [Setup](#setup)

> [A simple example](#simple-example)

>> [A Cox fit on the aggregated data](#simple-example-agg)

>> [The model definition](#simple-model)

>> [The data for each site](#simple-data)

>> [Setting up the sites](#simple-site-setup)

>> [Reproducing original aggregated analysis in a distributed fashion](#reproduce)

> [A larger example](#larger-example)

>> [The aggregated fit] (#larger-example-full)

>> [The distributed fit] (#larger-example-distributed)

> [Bone Marrow Transplant Example](#bmt)

>> [The aggregated fit] (#bmt-full)

>> [The distributed fit] (#bmt-distributed)

> [Byar and Greene Prostate Cancer Data Example (4 strata)](#prostate)


<a id="setup"></a>

### Setup

It must be noted that for users to be able to `knit` this document, or
to run these examples in an R session, an `opencpu` server must be
running with appropriate settings. On MacOS and Unix, this is done by
designating an empty directory as workspace and adding the following
lines to the `${HOME}/.Rprofile`.


```r
library(distcomp)
distcompSetup(workspace = "full_path_to_workspace_directory",
              ssl_verifyhost = 0L, ssl_verifypeer = 0L)
```

On windows, the same should be done in the `RHOME\etc\Rprofile.site`
file.

__Note__: On Yosemite (MacOS 10.10.4 and below), we have found that
references to `localhost` sometimes fail in the opencpu URL; rather
the explicit IP address `127.0.0.1` is needed.

In what follows, we assume that such initialization profile has been
done. Furthermore, we assume that the `opencpu` server has been
started in the _same session_ as the one where this markdown document
is being knitted via `library(opencpu)`. This is merely a
convenience that allows us to refer to the `opencpu` server URL
programmatically via `opencpu$url()`; otherwise, alternative means
would have to be found to refer to the `opencpu` URL in a permanent
manner. For example, if `opencpu` is started in another terminal using
`library(opencpu)`, one could note down the URL that it displays after
starting, and define


```r
opencpu <- list(url = function() "your opencpu URL here")
```

to ensure that the expression `opencpu$url()` returns the
appropriate URL.

To summarize: assuming that an `opencpu` server has been started via
`library(opencpu)` with a proper R profile and the expression
`opencpu$url()` has been set up appropriately, one can proceed to knit
this document in that R session.

<a id="simple-example"></a>

## A simple example

We take a simple example from the `survival` package, the `ovarian`
dataset.


```r
library(knitr)
library(survival)
data(ovarian)
str(ovarian)
```

```
## 'data.frame':	26 obs. of  6 variables:
##  $ futime  : num  59 115 156 421 431 448 464 475 477 563 ...
##  $ fustat  : num  1 1 1 0 1 0 1 1 0 1 ...
##  $ age     : num  72.3 74.5 66.5 53.4 50.3 ...
##  $ resid.ds: num  2 2 2 2 2 1 2 2 2 1 ...
##  $ rx      : num  1 1 1 2 1 1 2 2 1 2 ...
##  $ ecog.ps : num  1 1 2 1 1 2 2 2 1 2 ...
```

```r
kable(ovarian)
```



| futime| fustat|     age| resid.ds| rx| ecog.ps|
|------:|------:|-------:|--------:|--:|-------:|
|     59|      1| 72.3315|        2|  1|       1|
|    115|      1| 74.4932|        2|  1|       1|
|    156|      1| 66.4658|        2|  1|       2|
|    421|      0| 53.3644|        2|  2|       1|
|    431|      1| 50.3397|        2|  1|       1|
|    448|      0| 56.4301|        1|  1|       2|
|    464|      1| 56.9370|        2|  2|       2|
|    475|      1| 59.8548|        2|  2|       2|
|    477|      0| 64.1753|        2|  1|       1|
|    563|      1| 55.1781|        1|  2|       2|
|    638|      1| 56.7562|        1|  1|       2|
|    744|      0| 50.1096|        1|  2|       1|
|    769|      0| 59.6301|        2|  2|       2|
|    770|      0| 57.0521|        2|  2|       1|
|    803|      0| 39.2712|        1|  1|       1|
|    855|      0| 43.1233|        1|  1|       2|
|   1040|      0| 38.8932|        2|  1|       2|
|   1106|      0| 44.6000|        1|  1|       1|
|   1129|      0| 53.9068|        1|  2|       1|
|   1206|      0| 44.2055|        2|  2|       1|
|   1227|      0| 59.5890|        1|  2|       2|
|    268|      1| 74.5041|        2|  1|       2|
|    329|      1| 43.1370|        2|  1|       1|
|    353|      1| 63.2192|        1|  2|       2|
|    365|      1| 64.4247|        2|  2|       1|
|    377|      0| 58.3096|        1|  2|       1|

<a id="simple-example-agg"></a>

### A Cox fit on the aggregated data

A simple Cox model fit estimates the effect of age on survival,
stratified by drug.


```r
cp <- coxph(Surv(futime, fustat) ~ age + strata(rx), data = ovarian, ties = "breslow")
print(cp)
```

```
## Call:
## coxph(formula = Surv(futime, fustat) ~ age + strata(rx), data = ovarian, 
##     ties = "breslow")
## 
## 
##       coef exp(coef) se(coef)   z      p
## age 0.1374    1.1472   0.0474 2.9 0.0038
## 
## Likelihood ratio test=12.7  on 1 df, p=0.000368
## n= 26, number of events= 12
```

The above shows the intial and final log likelihood values at 0 and
the estimated coefficient respectively and the actual estimated
coefficient in the last line.

For our setting, we can pretend that this data set is actually from
two sites, one containing the control or placebo group (`rx = 1`)and
the other containing the drug group (`rx = 2`).

<a id="simple-model"></a>

### The model definition

We first need to define the computation. The available computations
can be listed:

```r
print(availableComputations())
```

```
## $StratifiedCoxModel
## $StratifiedCoxModel$desc
## [1] "Stratified Cox Model"
## 
## $StratifiedCoxModel$definitionApp
## [1] "defineNewCoxModel"
## 
## $StratifiedCoxModel$setupWorkerApp
## [1] "setupCoxWorker"
## 
## $StratifiedCoxModel$setupMasterApp
## [1] "setupCoxMaster"
## 
## $StratifiedCoxModel$makeDefinition
## function () 
## {
##     data.frame(id = getComputationInfo("id"), compType = getComputationInfo("compType"), 
##         projectName = getComputationInfo("projectName"), projectDesc = getComputationInfo("projectDesc"), 
##         formula = getComputationInfo("formula"), stringsAsFactors = FALSE)
## }
## <environment: 0x7fa882d9e8a8>
## 
## $StratifiedCoxModel$makeMaster
## function (defn, debug = FALSE) 
## CoxMaster$new(defnId = defn$id, formula = defn$formula, debug = debug)
## <environment: 0x7fa882d9e8a8>
## 
## $StratifiedCoxModel$makeWorker
## function (defn, data) 
## CoxWorker$new(data = data, formula = defn$formula)
## <environment: 0x7fa882d9e8a8>
## 
## 
## $RankKSVD
## $RankKSVD$desc
## [1] "Rank K SVD"
## 
## $RankKSVD$definitionApp
## [1] "defineNewSVDModel"
## 
## $RankKSVD$setupWorkerApp
## [1] "setupSVDWorker"
## 
## $RankKSVD$setupMasterApp
## [1] "setupSVDMaster"
## 
## $RankKSVD$makeDefinition
## function () 
## {
##     data.frame(id = getComputationInfo("id"), compType = getComputationInfo("compType"), 
##         projectName = getComputationInfo("projectName"), projectDesc = getComputationInfo("projectDesc"), 
##         rank = getComputationInfo("rank"), ncol = getComputationInfo("ncol"), 
##         stringsAsFactors = FALSE)
## }
## <environment: 0x7fa882d9e8a8>
## 
## $RankKSVD$makeMaster
## function (defn, debug = FALSE) 
## SVDMaster$new(defnId = defn$id, k = defn$rank, debug = debug)
## <environment: 0x7fa882d9e8a8>
## 
## $RankKSVD$makeWorker
## function (defn, data) 
## SVDWorker$new(x = data)
## <environment: 0x7fa882d9e8a8>
```

So, we can define the ovarian data computation as follows.


```r
ovarianDef <- data.frame(compType = names(availableComputations())[1],
                         formula = "Surv(futime, fustat) ~ age",
                         id = "Ovarian", stringsAsFactors=FALSE)
```

<a id="simple-data"></a>

### The data for each site

We split the `ovarian` data into two sites as indicated earlier.


```r
siteData <- with(ovarian, split(x = ovarian, f = rx))
```

<a id="simple-site-setup"></a>

### Setting up the sites

We can now set up each site with its own data. A site is merely a list
of two items, a (unique) `name` and an `opencpu` URL.


```r
nSites <- length(siteData)
sites <- lapply(seq.int(nSites), function(i) list(name = paste0("site", i),
                                                  url = opencpu$url()))
```

By default, on each site, data for a computation is stored under the
name `data.rds` and the definition itself is stored under the name
`defn.rds`.  If the sites are physically separate, then everything
proceeds smoothly. However, here, in our case, we are using the same
`opencpu` server for simulating both sites. We therefore have to save
the files under different names, just for this experiment, say
`site1.rds` and `site2.rds` for this example. This is all taken care
of by the code which checks to see if the `opencpu` URLs refer to
local hosts or not. (In fact, as this code is executing, one can
examine the contents of the workspace to see what is happening)
We now `Map` the upload function to each site so that the computation
becomes well-defined.


```r
ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) ovarianDef),
          siteData)

stopifnot(all(as.logical(ok)))
```

<a id="reproduce"></a>

### Reproducing original aggregated analysis in a distributed fashion

We are now ready to reproduce the original aggregated analysis. We
first create a master object using the same definition.


```r
master <- CoxMaster$new(defnId = ovarianDef$id, formula = ovarianDef$formula)
```
We then add the worker sites specifying a name and a URL for each.
names.


```r
for (i in seq.int(nSites)) {
    master$addSite(name = sites[[i]]$name, url = sites[[i]]$url)
}
```
And we now maximize the partial likelihood, by calling the `run`
method of the master.


```r
result <- master$run()
```

We then print the summary.


```r
master$summary()
```

```
##        coef exp(coef)  se(coef)       z           p
## 1 0.1373399  1.147218 0.0473947 2.89779 0.003758017
```
As we can see, the results we get from the distributed analysis are
the same as we got for the original aggregated analysis. We print them
separately here for comparison.

<a id="larger-example"></a>

## A larger example

We turn to a larger the example from Therneau and Grambsch using the
`pbc` data where the stratifying variable is `ascites`.

<a id="larger-example-full"></a>

### The aggregated fit


```r
data(pbc)
pbcCox <- coxph(Surv(time, status==2) ~ age + edema + log(bili) +
                  log(protime) + log(albumin) + strata(ascites), data = pbc,
                ties = "breslow")
print(pbcCox)
```

```
## Call:
## coxph(formula = Surv(time, status == 2) ~ age + edema + log(bili) + 
##     log(protime) + log(albumin) + strata(ascites), data = pbc, 
##     ties = "breslow")
## 
## 
##                  coef exp(coef) se(coef)     z       p
## age           0.03135   1.03185  0.00907  3.45 0.00055
## edema         0.59935   1.82093  0.32127  1.87 0.06210
## log(bili)     0.86626   2.37800  0.10066  8.61 < 2e-16
## log(protime)  3.03406  20.78146  1.03884  2.92 0.00349
## log(albumin) -2.96618   0.05150  0.78177 -3.79 0.00015
## 
## Likelihood ratio test=146  on 5 df, p=0
## n= 312, number of events= 125 
##    (106 observations deleted due to missingness)
```

<a id="larger-example-distributed"></a>

### The distributed fit

We split the data using `ascites` and proceed the usual way as shown above


```r
pbcDef <- data.frame(compType = names(availableComputations())[1],
                     formula = paste("Surv(time, status==2) ~ age + edema +",
                       "log(bili) + log(protime) + log(albumin)"),
                     id = "pbc", stringsAsFactors = FALSE)
siteData <- with(pbc, split(x = pbc, f = ascites))
nSites <- length(siteData)
sites <- lapply(seq.int(nSites), function(i) list(name = paste0("site", i),
                                                  url = opencpu$url()))
ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) pbcDef),
          siteData)
stopifnot(all(as.logical(ok)))
master <- CoxMaster$new(defnId = pbcDef$id, formula = pbcDef$formula)
for (site in sites) {
    master$addSite(site$name, site$url)
}
```


```r
result <- master$run()
```

We then print the summary.


```r
kable(master$summary())
```



|       coef|  exp(coef)|  se(coef)|         z|         p|
|----------:|----------:|---------:|---------:|---------:|
|  0.0310247|  1.0315110| 0.0090692|  3.420887| 0.0006242|
|  0.6019922|  1.8257525| 0.3205949|  1.877735| 0.0604174|
|  0.8682667|  2.3827773| 0.1006068|  8.630302| 0.0000000|
|  3.0276949| 20.6495786| 1.0393738|  2.912999| 0.0035798|
| -2.9765945|  0.0509661| 0.7809580| -3.811465| 0.0001381|

The results should be comparable to the aggregated fit above.

<a id="bmt"></a>

## Bone Marrow Transplant Example

This uses the `bmt` data from Klein and Moschberger. Some variable
renaming, first.


```r
if (!require("KMsurv")) {
  stop("Please install the KMsurv package before proceeding")
}
```


```r
##
## BMT data
##

library(KMsurv)
data(bmt)
bmt$tnodis <- bmt$t2 ## time to disease relapse/death
bmt$inodis <- bmt$d3 ## disease relapse/death indicator
bmt$tplate <- bmt$tp ## time to platelet recovery
bmt$iplate <- bmt$dp ## platelet recovery
bmt$agep <- bmt$z1 ## age of patient in years
bmt$aged <- bmt$z2 ## age of donor in years
bmt$fab <- bmt$z8 ## fab grade 4 or 5 + AML
bmt$imtx <- bmt$z10 ## MTX used
bmt <- bmt[order(bmt$tnodis), ] ## order by time to disease relapse/death
bmt <- cbind(1:nrow(bmt)[1], bmt)
names(bmt)[1] <- "id"

##
#####
##
bmt$agep.c <- bmt$agep - 28
bmt$aged.c <- bmt$aged - 28

bmt$imtx <- factor(bmt$imtx)
```

<a id="bmt-full"></a>

### The aggregated fit


```r
bmt.cph <- coxph(formula = Surv(tnodis, inodis) ~ fab + agep.c * aged.c +
                 factor(group) + strata(imtx), data = bmt, ties = "breslow")

print(bmt.cph)
```

```
## Call:
## coxph(formula = Surv(tnodis, inodis) ~ fab + agep.c * aged.c + 
##     factor(group) + strata(imtx), data = bmt, ties = "breslow")
## 
## 
##                    coef exp(coef) se(coef)     z      p
## fab             0.90780   2.47886  0.27899  3.25 0.0011
## agep.c          0.00551   1.00553  0.01997  0.28 0.7826
## aged.c         -0.00164   0.99836  0.01816 -0.09 0.9282
## factor(group)2 -1.03389   0.35562  0.36472 -2.83 0.0046
## factor(group)3 -0.33909   0.71242  0.36784 -0.92 0.3566
## agep.c:aged.c   0.00284   1.00285  0.00095  3.00 0.0027
## 
## Likelihood ratio test=31  on 6 df, p=2.5e-05
## n= 137, number of events= 83
```

<a id="bmt-distributed"></a>

### The distributed fit

We'll use `imtx` for splitting data into sites.


```r
bmtDef <- data.frame(compType = names(availableComputations())[1],
                     formula = paste("Surv(tnodis, inodis) ~ fab +",
                       "agep.c * aged.c + factor(group)"),
                     id = "bmt", stringsAsFactors = FALSE)
siteData <- with(bmt, split(x = bmt, f = imtx))
nSites <- length(siteData)
sites <- lapply(seq.int(nSites), function(i) list(name = paste0("site", i),
                                                  url = opencpu$url()))
ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) bmtDef),
          siteData)

stopifnot(all(as.logical(ok)))
master <- CoxMaster$new(defnId = bmtDef$id, formula = bmtDef$formula)
for (site in sites) {
    master$addSite(site$name, site$url)
}
```


```r
result <- master$run()
```

We then print the summary.


```r
kable(master$summary())
```



|       coef| exp(coef)|  se(coef)|          z|         p|
|----------:|---------:|---------:|----------:|---------:|
|  0.9083860| 2.4803160| 0.2789473|  3.2564784| 0.0011280|
|  0.0054343| 1.0054490| 0.0199716|  0.2720993| 0.7855457|
| -0.0017046| 0.9982969| 0.0181680| -0.0938216| 0.9252508|
| -1.0338625| 0.3556307| 0.3648730| -2.8334862| 0.0046043|
| -0.3375983| 0.7134818| 0.3680206| -0.9173355| 0.3589669|
|  0.0028547| 1.0028588| 0.0009480|  3.0111928| 0.0026022|

<a id="prostate"></a>

## Byar and Greene Prostate Cancer Data Example

This example is the largest of them all and also has four strata
rather than 2.


```r
prostate <- readRDS("prostate.RDS")
```

<a id="prostate-full"></a>

### The aggregated fit


```r
pcph <- coxph(Surv(dtime, status) ~ stage + strata(rx) + age + wt + pf + hx +
                  sbp + dbp + ekg + hg + sz + sg + ap + bm, data = prostate)
print(pcph)
```

```
## Call:
## coxph(formula = Surv(dtime, status) ~ stage + strata(rx) + age + 
##     wt + pf + hx + sbp + dbp + ekg + hg + sz + sg + ap + bm, 
##     data = prostate)
## 
## 
##                          coef exp(coef)  se(coef)     z       p
## stageIV             -0.177590  0.837286  0.175923 -1.01 0.31275
## age                  0.024208  1.024504  0.009157  2.64 0.00820
## wt                  -0.010256  0.989797  0.004755 -2.16 0.03102
## pfBedridden(<50%)   -1.372753  0.253408  0.851150 -1.61 0.10678
## pfBedridden(>50%)   -1.174937  0.308838  0.850354 -1.38 0.16706
## pfnormal            -1.588673  0.204196  0.825542 -1.92 0.05430
## hx                   0.510419  1.665989  0.120371  4.24 2.2e-05
## sbp                 -0.033086  0.967455  0.029279 -1.13 0.25846
## dbp                  0.049434  1.050676  0.047790  1.03 0.30096
## ekgblock/conduction -0.145903  0.864242  0.382387 -0.38 0.70279
## ekgheart strain      0.393173  1.481675  0.282538  1.39 0.16405
## ekgnormal           -0.057959  0.943689  0.281828 -0.21 0.83706
## ekgold MI            0.007442  1.007470  0.301073  0.02 0.98028
## ekgrecent MI         0.818946  2.268109  1.055545  0.78 0.43784
## ekgrhythmic disturb  0.280313  1.323544  0.307714  0.91 0.36232
## hg                  -0.068929  0.933393  0.031996 -2.15 0.03122
## sz                   0.017804  1.017963  0.004608  3.86 0.00011
## sg                   0.100201  1.105393  0.041607  2.41 0.01603
## ap                  -0.001384  0.998617  0.000997 -1.39 0.16510
## bm                   0.319640  1.376633  0.181333  1.76 0.07795
## 
## Likelihood ratio test=99.4  on 20 df, p=1.59e-12
## n= 475, number of events= 338 
##    (27 observations deleted due to missingness)
```
<a id="prostate-distributed"></a>

### The distributed fit

The distributed fit for this particular example doesn't work in the
current implementation. This is because the $X$ matrix for each site
is singular. The math holds, obviously, but the current implementation
is based on re-using as much of the `survival` package as possible. We
have to work harder to implement the distributed computation in the
situation where $X$ is singular at at least one site. This affects the
computation of the variance (or equivalently, the information
matrix). Some work needs to be done to work around this and figure out
how best to reuse what's already in the `survival` package.

However, in order to demonstrate that the distributed fit really
works, we show below an alternative implementation that yields the
same result as the aggregated one.


<a id="site-object"></a>

#### An object representing the sites

Here's a reference object for each site. It has several fields: a
`data` field containing the data, a `formula` field (as used in the
well-known `survival` R package) describing the model being fit. These
two are the only ones needed for initializing a site. Other fields
that are generated based on these two fields are `modelDataFrame`
containing the actual model data used for fitting the model and a
`modelMatrix`.


```r
site <- setRefClass("siteObject",
                    fields = list(
                        data = "data.frame",
                        formula = "formula",
                        modelDataFrame = "data.frame",
                        coxControl = "list",
                        modelMatrix = "matrix"),
                    methods = list(
                        initialize = function(formula, data) {
                            'Initialize the object with a formula and dataset'
                            formula <<- formula
                            data <<- data
                            temp <- coxph.control()
                            temp$iter.max <- 0
                            coxControl <<- temp
                            stopifnot(kosher())
                        },
                        kosher = function() {
                            'Check that the class data passes sanity checks'
                            modelDataFrame <<- model.frame(formula, data = data)
                            lhs <- modelDataFrame[, 1]
                            ordering <- order(lhs[, 1])
                            modelDataFrame <<- modelDataFrame[ordering, ]
                            data <<- data[ordering, ]
                            modelMatrix <<- model.matrix(formula, data = modelDataFrame)
                            TRUE
                        },
                        dimP = function() {
                            'Return the number of covariates'
                            ncol(modelMatrix) - 1
                        })
                    )
site$accessors(c("data", "formula", "modelDataFrame", "modelMatrix"))
```

The `initialize` method above mostly sets the fields, generates values
for other fields, and does a mild sanity check. It will not proceed
further if the function `kosher` returns false.  For now the `kosher`
function merely orders the data frame by follow-up time, but in a
production system a number of other checks might be performed, such as
ensuring all named variables are available at the site.

The method `dimP` merely returns the number of columns of the model
matrix.

<a id="site-loglik"></a>

### The (partial) log likelihood function for each site

For our example, the (partial) log likelihood (named `localLogLik`) is simple
and can be computed directly. Assuming failure times $t_i$ and
event indicators $\delta_i$, it is
precisely:
$$
l(\beta) = \sum_{i=1}^n\delta_i\biggl[z_i\beta -\log\bigl(\sum_{j\in R(t_i)} \exp(z_j\beta)\bigr)\biggr]
$$
where $\beta$ is the vector of parameters, $z_i$ is row $i$ of the
model matrix (covariates for subject $i$) and $R(t_i)$ is the risk set
at time $t_i$.

The first derivative with respect to $\beta$ is:
$$
l'(\beta) = Z^T\delta - \sum_{i=1}^n\delta_i\frac{\sum_{j\in R(t_i)} \exp(z_j\beta)z_j^T}{\sum_{j\in R(t_i)} \exp(z_j\beta)}.
$$


```r
localLogLik <- function(beta) {
    beta <- c(0, beta) ## model matrix has intercept in model
    z <- modelMatrix
    delta <- modelDataFrame[, 1][, 2] ## event indicators
    zBeta <- z %*% beta
    sum.exp.zBeta <- rev(cumsum(rev(exp(zBeta))))
    ld <- delta %*% (z  - apply(diag(as.numeric(zBeta)) %*% z, 2, function(x) rev(cumsum(rev(x)))) / sum.exp.zBeta)
    result <- sum(delta * (zBeta - log(sum.exp.zBeta))) # assuming Breslow
    attr(result, "gradient") <- ld
    result
}
site$methods(logLik = localLogLik)
```

#### The alternative distributed fit.

We are now ready to do the alternative distributed fit. We split the
`prostate` data into two sites as indicated earlier.


```r
siteData <- with(prostate, split(x = prostate, f = rx))
sites <- lapply(siteData,
                function(x) {
                    site$new(data = x,
                             formula = Surv(dtime, status) ~ stage + age + wt + pf + hx + sbp + dbp + ekg + hg + sz + sg + ap + bm)
                })
```
Ok, now we can reproduce the original aggregated analysis by writing a
full likelihood routine.


```r
logLik <- function(beta, sites) {
    sum(sapply(sites, function(x) x$logLik(beta)))
}
```

All that remains is to maximize this log likelihood.


```r
mleResults <- nlm(f=function(x) -logLik(x, sites),
                  p = rep(0, sites[[1]]$dimP()),
                  gradtol = 1e-10, iterlim = 1000)
```

```
## Warning in nlm(f = function(x) -logLik(x, sites), p = rep(0, sites[[1]]
## $dimP()), : NA/Inf replaced by maximum positive value
```

```
## Warning in nlm(f = function(x) -logLik(x, sites), p = rep(0, sites[[1]]
## $dimP()), : NA/Inf replaced by maximum positive value
```

```
## Warning in nlm(f = function(x) -logLik(x, sites), p = rep(0, sites[[1]]
## $dimP()), : NA/Inf replaced by maximum positive value
```

We print the coefficient estimates side-by-side for comparison.


```r
d <- data.frame(distCoef = mleResults$estimate, aggCoef = pcph$coefficients)
rownames(d) <- names(pcph$coefficients)
kable(d)
```



|                    |   distCoef|    aggCoef|
|:-------------------|----------:|----------:|
|stageIV             | -0.1740491| -0.1775897|
|age                 |  0.0245191|  0.0242084|
|wt                  | -0.0105003| -0.0102559|
|pfBedridden(<50%)   | -1.3893181| -1.3727528|
|pfBedridden(>50%)   | -1.1506821| -1.1749372|
|pfnormal            | -1.5842335| -1.5886734|
|hx                  |  0.5206160|  0.5104190|
|sbp                 | -0.0329601| -0.0330859|
|dbp                 |  0.0526530|  0.0494337|
|ekgblock/conduction | -0.1505854| -0.1459025|
|ekgheart strain     |  0.3980165|  0.3931730|
|ekgnormal           | -0.0557321| -0.0579591|
|ekgold MI           |  0.0064296|  0.0074423|
|ekgrecent MI        |  0.8560625|  0.8189465|
|ekgrhythmic disturb |  0.2816923|  0.2803126|
|hg                  | -0.0707047| -0.0689290|
|sz                  |  0.0180493|  0.0178037|
|sg                  |  0.1016070|  0.1002007|
|ap                  | -0.0013375| -0.0013841|
|bm                  |  0.3180923|  0.3196404|

## Session Information

```r
sessionInfo()
```

```
## R version 3.2.2 (2015-08-14)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.5 (Yosemite)
## 
## locale:
## [1] C
## 
## attached base packages:
## [1] grDevices utils     datasets  stats     graphics  methods   base     
## 
## other attached packages:
## [1] rmarkdown_0.8.1 KMsurv_0.1-5    opencpu_1.5.1   distcomp_0.25.4
## [5] survival_2.38-3 knitr_1.11      devtools_1.9.1 
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.1      magrittr_1.5     splines_3.2.2    xtable_1.7-4    
##  [5] R6_2.1.1         brew_1.0-6       highr_0.5.1      stringr_1.0.0   
##  [9] httr_1.0.0       tools_3.2.2      parallel_3.2.2   htmltools_0.2.6 
## [13] openssl_0.4      digest_0.6.8     shiny_0.12.2     formatR_1.2.1   
## [17] codetools_0.2-14 curl_0.9.3       memoise_0.2.1    evaluate_0.8    
## [21] mime_0.4         stringi_0.5-5    compiler_3.2.2   jsonlite_0.9.17 
## [25] markdown_0.7.7   httpuv_1.3.3
```

