root <- file.path(tempdir(), "gitsync_tests")
dir.create(file.path(root, "source"), recursive = TRUE)
dir.create(file.path(root, "source", "A"), recursive = TRUE)

setwd(file.path(root, "source", "A"))
writeLines("test", "test.txt")
system2("git", "init")
system2("git", c("add", "."))
system2("git", c("commit", "-m", "'Initial commit'"))

setwd(file.path(root, "source"))
## gitsync:::git_bundle_create(repos = "A",
                            

dir.create(file.path(root, "target"), recursive = TRUE)
