distcomp
========

Install this package the usual way in R or via:

```{r}
library(devtools)
install_github("hrpcisd/distcomp")
```

Then, you will find a document that describes several examples
installed under the R library tree. For example:

```{r}
list.files(system.file("doc", package = "distcomp"))
list.files(system.file("doc_src", package = "distcomp"))
```

The examples described in the Journal of Statistical Software paper
are available as follows:

```{r}
list.files(system.file("ex", package = "distcomp"))
```

Use of this package requires some configuration. In particular, to run
the examples on a local machine where a single `opencpu` server will
be emulating several sites, a suitable R profile needs to be set
up. That profile will be something along the lines of

```{r, eval=FALSE}
library(distcomp)
distcompSetup(workspace = "full_path_to_workspace_directory",
              ssl_verifyhost = 0L, ssl_verifypeer = 0L)
```
where the workspace is a directory that the `opencpu` server can
serialize objects to. On Unix or Mac, the above can be inserted into
an `.Rprofile` file, but on Windows, we find that the `Rprofile.site`
file needs to contain the above lines.

The effect of this is that _every R_ process (including the `opencpu`
process) has access to the `distcomp` library and the workspace.


## OSX Yosemite Issues (10.10.4 and below)

In some earlier versions of Yosemite (MacOS 10.10.2 for example) there
were issues with the `opencpu` package. We suspected it had more to do
with Yosemite than `opencpu` or `httpuv`---see the number of DNS
resolver issues people are having with the move to `discoveryd` via a
google search.

Furthermore, even if `opencpu` came up, which it did sometimes after
many tries, you were better off using the ip address `127.0.0.1` in
the url in place of `localhost` due to those DNS problems.

We no longer see this problem in recent versions of Yosemite
(10.10.5+).
