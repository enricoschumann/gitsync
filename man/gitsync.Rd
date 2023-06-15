\name{gitsync}
\alias{create_bundles}
\alias{gitsync-package}
\alias{gitsync}
\title{
  Sync Directories and Subdirectories with Git
}
\description{
  Sync directories and subdirectories with git.
}
\usage{
create_bundles(root.dir, bundle.dir,
               exclude.re = NULL,
               dir.list = NULL,
               path.separator = "FORWARDSLASH", ...)
}
\arguments{
  \item{root.dir}{
    string
}
  \item{bundle.dir}{
    string
}
  \item{exclude.re}{
    character. 
}
  \item{dir.list}{
    character. 
  }
  \item{path.separator}{
    a strong to be used as path separator. Do not use \\ or /.
  }
  \item{\dots}{
    other arguments
}
}
\details{
 Experimental.
}
\value{
  called for side effect.
}
\author{
  Enrico Schumann
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
1+1
}
