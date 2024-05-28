# gitsync

Find git repositories within a directory structure and
clone/synchronize those repositories to another
directory or machine, using git's bundle tools.

[ [More] ](http://enricoschumann.net/R/packages/gitsync/)

## Installing the package

The latest released version is available from
http://enricoschumann.net. In an R session, just type:

    install.packages('gitsync', type = 'source',
                     repos = c('http://enricoschumann.net/R', getOption('repos')))


## Usage

    ## create directory Machine1, with two repositories
    root <- file.path(tempdir(), "gitsync_tests")
    dir.create(file.path(root, "Machine1", "D1"), recursive = TRUE)
    dir.create(file.path(root, "Machine1", "D", "D2"), recursive = TRUE)

    setwd(file.path(root, "Machine1", "D1"))
    writeLines("test D1 on machine 1", "test.txt")
    system2("git", "init")
    system2("git", c("add", "."))
    system2("git", c("commit", "-m", "'Initial commit'"))

    setwd(file.path(root, "Machine1", "D", "D2"))
    writeLines("test D2 in Machine1", "test.txt")
    system2("git", "init")
    system2("git", c("add", "."))
    system2("git", c("commit", "-m", "'Initial commit'"))

    ## ==> directory structure on Machine1 should look like this
    ## Machine1/
    ## |-- D
    ## |   `-- D2
    ## |       `-- test.txt
    ## `-- D1
    ##     `-- test.txt

    ## create bundles ....
    gitsync::create_bundles(file.path(root, "Machine1"),
                            bundle.dir = root)
    ## ... and recreate directory structure from bundles
    pull_from_bundles(file.path(root, "Machine2"),
                      bundle.dir = root)
    pull_from_bundles(file.path(root, "Machine2"),
                      bundle.dir = root,
                      if.not.exists = "clone")
    ## ==> directory structure on Machine2 should look like this
    ## Machine2/
    ## |-- D
    ## |   `-- D2
    ## |       `-- test.txt
    ## `-- D1
    ##     `-- test.txt

