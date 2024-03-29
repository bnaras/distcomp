% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/querycount.R
\docType{class}
\name{QueryCountMaster}
\alias{QueryCountMaster}
\title{Create a master object to control worker objects generated by \code{\link[=QueryCountWorker]{QueryCountWorker()}}}
\description{
\code{QueryCountMaster} objects instantiate and run a distributed query count computation
}
\seealso{
\code{\link[=QueryCountWorker]{QueryCountWorker()}} which goes hand-in-hand with this object
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-QueryCountMaster-new}{\code{QueryCountMaster$new()}}
\item \href{#method-QueryCountMaster-kosher}{\code{QueryCountMaster$kosher()}}
\item \href{#method-QueryCountMaster-queryCount}{\code{QueryCountMaster$queryCount()}}
\item \href{#method-QueryCountMaster-getSites}{\code{QueryCountMaster$getSites()}}
\item \href{#method-QueryCountMaster-addSite}{\code{QueryCountMaster$addSite()}}
\item \href{#method-QueryCountMaster-run}{\code{QueryCountMaster$run()}}
\item \href{#method-QueryCountMaster-clone}{\code{QueryCountMaster$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-QueryCountMaster-new"></a>}}
\if{latex}{\out{\hypertarget{method-QueryCountMaster-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{QueryCountMaster} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{QueryCountMaster$new(defn, debug = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{defn}}{the computation definition}

\item{\code{debug}}{a flag for debugging, default \code{FALSE}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a new \code{QueryCountMaster} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-QueryCountMaster-kosher"></a>}}
\if{latex}{\out{\hypertarget{method-QueryCountMaster-kosher}{}}}
\subsection{Method \code{kosher()}}{
Check if inputs and state of object are sane. For future use
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{QueryCountMaster$kosher()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{TRUE} or \code{FALSE}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-QueryCountMaster-queryCount"></a>}}
\if{latex}{\out{\hypertarget{method-QueryCountMaster-queryCount}{}}}
\subsection{Method \code{queryCount()}}{
Run the distributed query count and return the result
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{QueryCountMaster$queryCount()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
the count
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-QueryCountMaster-getSites"></a>}}
\if{latex}{\out{\hypertarget{method-QueryCountMaster-getSites}{}}}
\subsection{Method \code{getSites()}}{
Retrieve the value of the private \code{sites} field
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{QueryCountMaster$getSites()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-QueryCountMaster-addSite"></a>}}
\if{latex}{\out{\hypertarget{method-QueryCountMaster-addSite}{}}}
\subsection{Method \code{addSite()}}{
Add a url or worker object for a site for participating in the distributed computation. The worker object can be used to avoid complications in debugging remote calls during prototyping.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{QueryCountMaster$addSite(name, url = NULL, worker = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{of the site}

\item{\code{url}}{web url of the site; exactly one of \code{url} or \code{worker} should be specified}

\item{\code{worker}}{worker object for the site; exactly one of \code{url} or \code{worker} should be specified}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-QueryCountMaster-run"></a>}}
\if{latex}{\out{\hypertarget{method-QueryCountMaster-run}{}}}
\subsection{Method \code{run()}}{
Run the distributed query count
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{QueryCountMaster$run()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
the count
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-QueryCountMaster-clone"></a>}}
\if{latex}{\out{\hypertarget{method-QueryCountMaster-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{QueryCountMaster$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
