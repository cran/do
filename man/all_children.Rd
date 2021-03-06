% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/children_node.R
\name{all_children}
\alias{all_children}
\title{Extract all children nodes}
\usage{
all_children(x, res = "do not change", i = 1)
}
\arguments{
\item{x}{one or more documents, nodes, or node sets.}

\item{res}{omit. do not make any change.}

\item{i}{must be 1}
}
\value{
nodeset
}
\description{
Extract all children nodes
}
\examples{
txt='<div class="activityBody postBody thing">
        <p>
            <a href="/forum/conversation" class="mqPostRef">(22)</a>
            where?
        </p>
      <p>
        stays 
        <b>disappears</b>
        <a>disappears</a>
        <span>disappears</span>
        stays
      </p>
    </div>'
    library(xml2)
    html=read_html(txt)
    
    all_children(html)
}
