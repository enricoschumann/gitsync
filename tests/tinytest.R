if (requireNamespace("tinytest", quietly = TRUE))
    tinytest.results <- tinytest::test_package("gitsync",
                                               color = interactive(),
                                               verbose = 1)
