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
               exclude.pattern = NULL,
               include.only.pattern = NULL,
               path.separator = "FORWARDSLASH",
               git.paths = NULL,
               ...,
               max.char = NA)

pull_from_bundles(root.dir, bundle.dir,
                  path.separator = "FORWARDSLASH",
                  if.not.exists = "skip",
                  bundle.pattern = NULL,
                  ignore.case = FALSE,
                  ...)

git_paths(root.dir = ".", sub.tilde = TRUE,
          max.char = NA,
          git.paths = NULL,
          cache.dir = NULL, ...)
}
\arguments{
  \item{root.dir}{
    string: a path
  }
  \item{bundle.dir}{%

    string: the directory path where the bundles are to be
    stored

  }
  \item{exclude.pattern}{
    character: regular expression; can be of length > 1
  }
  \item{include.only.pattern}{
    character: regular expression; can be of length > 1
  }
  \item{bundle.pattern}{
    character: regular expression for selecting bundles
  }
  \item{ignore.case}{%
    logical: used for \code{bundle.pattern} when
    selecting bundles
  }
  \item{path.separator}{
    a string to be used as path separator. Do not use \\ or /.
  }
  \item{git.paths}{%
    character: one or more paths relative to
    \code{root.dir}. if not \code{NULL}, only the directories
      specified in \code{git.paths} are bundled
  }
  \item{if.not.exists}{
    string: what to do if a git directory does not exist
    yet; can be \code{"skip"} (default) or \code{"clone"}
  }
  \item{sub.tilde}{
    if \code{TRUE}, replace \code{~} by
    \code{normalizePath(path.expand("~"), winslash = "/")}
  }
  \item{max.char}{%
    integer: if not \code{NA}, skip paths longer than
    \code{max.char}
  }
  \item{cache.dir}{%
    a path. Not yet implemented.
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
