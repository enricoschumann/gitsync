# gitsync

...............................................
...............................................
..................................................
......................................................
...................................................
.....................................................
....................................................
.................................................
..........................................
...............................................
.............................................

[ [More] ](http://enricoschumann.net/R/packages/gitsync/)

## Installing the package

The latest released version is available from
http://enricoschumann.net. In an R session, just type:

    install.packages('gitsync', type = 'source',
                     repos = c('http://enricoschumann.net/R', getOption('repos')))


For the latest development version, check out the Git repository and
build it. In a shell (e.g. sh or bash):

    ## FIRST-TIME INSTALLATION
    #### cd to parent directory and ...
    $ git clone https://github.com/enricoschumann/gitsync.git

    #### build and install the package
    $ Rscript gitsync/Scripts/tznames.R
    $ R CMD build gitsync
    $ R CMD INSTALL gitsync_0.1-0.tar.gz  ## adjust version number

    #### optionally check
    $ R CMD check gitsync_0.1-0.tar.gz    ## adjust version number



    ## UPDATING
    #### later: cd to parent directory and ...
    $ R CMD build gitsync
    $ R CMD INSTALL gitsync_0.1-0.tar.gz  ## adjust version number
