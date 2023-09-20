\name{gitsync}
\alias{create_bundles}
\alias{git_paths}
\alias{gitsync}
\alias{gitsync-package}
\alias{pull_from_bundles}
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

pull_from_bundles(root.dir, bundle.dir,
                  path.separator = "FORWARDSLASH",
                  if.not.exists = "skip",
                  ...)

git_paths(path = ".", sub.tilde = TRUE,
          max.char = NA, ...)
}
\arguments{
  \item{root.dir}{
      string: a path
}
  \item{bundle.dir}{
      string: the directory path where the bundles are
      stored
}
  \item{exclude.re}{
      character: regular expressions
}
  \item{dir.list}{
      character: if not \code{NULL}, only the directories
      specified in \code{dir.list} are bundled
  }
  \item{path.separator}{
    a string to be used as path separator. Do not use \\ or /.
  }
  \item{\dots}{
    other arguments
}
}
\details{
 Experimental.

 \code{git_paths} identifies paths of git repositories
 under \code{path}, by (currently) simply checking whether
 a subdirectory \code{.git} exists.

}
\value{
  \code{create_bundles} and \code{pull_from_bundles} are
  called for their side effect
}
\author{
  Enrico Schumann
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
\donttest{
    git_paths("~/Documents")
}
}
