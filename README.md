distcomp
========

Install this package in R via:

```{r}
library(devtools)
install_github("hrpcisd/distcomp")
```

Then, you will find a document that describes several examples
installed under the R library tree. For example:

```{r}
list.files(system.file("doc", package="distcomp"))
list.files(system.file("doc_src", package="distcomp"))
```

The examples described in the paper are available as follows:

```{r}
list.files(system.file("ex", package="distcomp"))
```

Use of this package requires some configuration. In particular, to run
the examples on a local machine where a single `opencpu` server will
be emulating several sites, a suitable R profile needs to be set
up. That profile will be something along the lines of

```{r, eval=FALSE}
library(distcomp)
distcompSetup(workspace="full_path_to_workspace_directory",
              ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
```

where the workspace is a directory that the `opencpu` server can
serialize objects to. On Unix or Mac, the above can be inserted into
an `.Rprofile` file, but on Windows, we find that the `Rprofile.site`
file needs to contain the above lines.

The effect of this is that _every R_ process (including the `opencpu`
process) has access to the library and the workspace.



