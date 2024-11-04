## create Machine1, with two repositories
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
## Machine1/
## Machine1/
## |-- D
## |   `-- D2
## |       `-- test.txt
## `-- D1
##     `-- test.txt


gitsync::git_paths(file.path(root, "Machine1"))
gitsync::create_bundles(file.path(root, "Machine1"),
                        bundle.dir = root)
gitsync::pull_from_bundles(file.path(root, "Machine2"),
                           bundle.dir = root)
gitsync::pull_from_bundles(file.path(root, "Machine2"),
                           bundle.dir = root,
                           if.not.exists = "clone")
