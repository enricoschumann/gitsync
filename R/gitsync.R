pull_from_bundles <-
function(root.dir,
         bundle.dir,
         path.separator = "FORWARDSLASH",
         if.not.exists = "skip",
         bundle.pattern = NULL,
         ignore.case = FALSE,
         ...) {

    bundles <- dir(bundle.dir, include.dirs = FALSE,
                   pattern = bundle.pattern,
                   ignore.case = ignore.case)
    isdir <- file.info(file.path(bundle.dir, bundles))$isdir
    bundles <- bundles[!isdir]
    for (bundle in bundles) {

        b.path <- sub("[.]bundle$", "", bundle)
        b.path <- gsub(path.separator,
                       .Platform$file.sep,
                       b.path)

        message(b.path, " ...", appendLF = FALSE)
        if (!dir.exists(file.path(root.dir, b.path))) {
            if (if.not.exists == "clone") {

                message("dir does not exist -- [clone]")
                parent.dir <- file.path(root.dir, dirname(b.path))
                if (!dir.exists(parent.dir))
                    dir.create(parent.dir,
                               recursive = TRUE,
                               showWarnings = FALSE)
                msg <- try(git_bundle_clone(
                    file.path(bundle.dir, bundle),
                    dir.name = basename(b.path),
                    parent.dir = parent.dir,
                    stdout = TRUE,
                    stderr = TRUE),
                    silent = TRUE)
            } else {
                message("dir does not exist -- [skip]")
                next
            }

        } else {

            if (!dir.exists(file.path(root.dir, b.path, ".git"))) {
                message(paste0("\n   ===> dir exists, ",
                               "but no .git repository -- [skip]"))
                next
            }

            d <- normalizePath(file.path(root.dir, b.path))

            msg <- git_bundle_pull(
                shQuote(normalizePath(file.path(bundle.dir, bundle))),
                target = file.path(root.dir, b.path),
                ## branch = br.name,
                stdout = TRUE,
                stderr = TRUE)
            if (length(msg) == 3 && msg[3] == "Already up to date.") {
                message(cli::col_green(" [OK]"), appendLF = TRUE)
            } else {
                message("")
                cat(paste("    ", msg), sep = "\n")
                message("\n\n")
            }
        }
    }

    invisible(bundles)
}

create_bundles <-
function(root.dir,
         bundle.dir,
         exclude.pattern = NULL,
         include.only.pattern = NULL,
         path.separator = "FORWARDSLASH",
         git.paths = NULL,
         cache.dir = NULL,
         ...,
         max.char = NA) {

    git <- git_paths(root.dir = root.dir,
                     max.char = max.char,
                     git.paths = git.paths,
                     cache.dir = cache.dir)

    for (x in include.only.pattern)
        git <- git[ grepl(x, git)]
    for (x in exclude.pattern)
        git <- git[!grepl(x, git)]

    for (i in seq_along(git)) {
        h <- git2r::repository_head(file.path(root.dir, git[i]))
        if (is.null(h)) {
            message("no repository ", git[i])
            next
        }

        d.file <- gsub(.Platform$file.sep,
                       path.separator, git[i])
        d.file <- paste0(d.file, ".bundle")

        git_bundle_create(file.path(root.dir, git[i]),
                          output.filenames = d.file,
                          output.dir = bundle.dir,
                          dated.bundle = FALSE)
    }
    invisible(git)
}

git_paths <-
function(root.dir = ".",
         sub.tilde = TRUE,
         max.char = NA,
         git.paths = NULL,
         cache.dir = NULL,
         ...) {

    write.cache <- FALSE
    if (!is.null(cache.dir) && is.null(git.paths)) {
        if (!dir.exists(cache.dir)) {
            dcreated <- dir.create(cache.dir, recursive = TRUE)
            if (dcreated) {
                message("created directory ", cache.dir)
                write.cache <- TRUE
            } else {
                message("could not create cache directory ", cache.dir)
            }
        } else {
            ## read file
            cached <- dir(path = cache.dir,
                          pattern = paste0("^[0-9_T]+.*__git_paths__",
                                           gsub("/", "_", root.dir)))
            if (length(cached)) {
                max.age <- 5
                cached <- max(cached)
                if (as.numeric(Sys.Date() - as.Date(cached), unit = "days") >= max.age) {
                    write.cache <- TRUE
                } else {
                    git.paths <- readLines(file.path(cache.dir, cached))
                    if (!length(git.paths)) {
                        message("no paths in cache [run without or clear cache if that seems wrong]")
                        return(character())
                    }
                }

            } else {
                write.cache <- TRUE
            }
        }
    }
    if (is.null(git.paths)) {
        f <- list.files(path = root.dir, pattern = "^[.]git$",
                        include.dirs = TRUE,
                        recursive = TRUE, all.files = TRUE)
        f <- dirname(f)
        if (write.cache) {
            fn <- paste0(format(Sys.time(), "%Y-%m-%dT%H%M%S"),
                         "__git_paths__",
                         gsub("/", "_", root.dir))
            writeLines(f, file.path(cache.dir, fn))
        }
    } else
        f <- git.paths
    if (is.finite(max.char))
        f <- f[nchar(file.path(root.dir, f)) <= max.char]
    f <- f[file.info(file.path(root.dir, f))$isdir]
    ## if (is.null(git.paths)) {
    ##     f <- dirname(f)
    ## }
    f <- sort(unique(f))
    if (sub.tilde)
        f <- sub(normalizePath(path.expand("~"), winslash = "/"),
                 "~", f, fixed = TRUE)
    class(f) <- c("git_list", class(f))
    f
}



git_info <-
function(path, ...,
         branch.type = "local") {

    stopifnot(requireNamespace("git2r"))
    paths <- path
    ## clean <- rep(NA, length(paths))
    ## remotes <- rep(NA_character_, length(paths))
    ## remote_urls <- rep(NA_character_, length(paths))
    ## branches <- rep(NA_character_, length(paths))

    ans <- vector("list", length(paths))
    names(ans) <- paths



    for (path in paths) {
        p <- path == paths
        br <- sort(names(git2r::branches(path, branch.type)))
        ans[[path]][["branches"]] <- br

        st <- git2r::status(path)
        ans[[path]][["status"]] <- st

        clean <- !(length(st$staged) || length(st$unstaged) || length(st$untracked))
        ans[[path]][["is.clean"]] <- clean

        rem <- git2r::remotes(path)
        urls <- git2r::remote_url(path, remote = rem)
        names(urls) <- rem
        ans[[path]][["remotes"]] <- urls
    }

    ans
}

git_bundle_create <-
function(repos, output.filenames,
         output.dir,
         dated.bundle = TRUE,
         overwrite = TRUE,
         ref = c("--branches", "--tags")) {

    if (!dir.exists(output.dir)) {
        ans <- askYesNo(paste0("Create directory", output.dir, "?"))
        if (is.na(ans) || !ans)
            return(invisible(NULL))
        else
            dir.create(path.expand(output.dir))
    }
    current.dir <- getwd()
    on.exit(setwd(current.dir))
    for (i in seq_along(repos)) {
        message(repos[i], "\n",
                " =>", appendLF = FALSE)
        if (!dir.exists(repos[i]))
            message("    repository does not exist => skip")
        else
            message("    ", output.filenames[i], "\n")

        setwd(repos[i])
        bundle <- paste0(
            strftime(Sys.time(), "%Y%m%d_%H%M%S__"),
            "temp.bundle")
        system2("git", c("bundle", "create", bundle, ref))
        message("")

        out.file <- output.filenames[i]
        if (!endsWith(out.file, ".bundle"))
            out.file <- paste0(out.file, ".bundle")

        copied <- file.copy(bundle, file.path(output.dir, out.file),
                            overwrite = overwrite)

        if (dated.bundle) {
            out.file <- paste0(
                strftime(Sys.time(), "%Y%m%d_%H%M%S__"),
                out.file)
            file.copy(bundle, file.path(output.dir, out.file),
                      overwrite = overwrite)
        }
        removed <- file.remove(bundle)
        if (!removed)
            warning("could not remove bundle ", bundle)

    }
    invisible(NULL)
}

git_bundle_pull <-
function(bundle, target, branch = NULL, verify = TRUE, ...) {

    if (!dir.exists(target))
        stop("'target' does not exist. Maybe clone?")

    current.dir <- getwd()
    on.exit(setwd(current.dir))

    setwd(target)
    if (verify)
        v <- system2("git", c("bundle", "verify", bundle), ...)

    heads <- system2("git", c("bundle", "list-heads", bundle), ...)
    heads <- heads[grepl("refs/heads", heads)]

    heads <- strsplit(heads, " refs/heads/", fixed = TRUE)
    heads <- unlist(lapply(heads, `[[`, 2))

    current <- system2("git", "status",
                       stdout = TRUE, stderr = TRUE)
    current <- sub("On branch ", "", current[[1]], fixed = TRUE)

    if (current %in% heads) {
        system2("git", c("pull", bundle, current))
        heads <- setdiff(heads, current)
    }

    for (branch in heads) {
        res <- system2("git", c("checkout", branch),
                       stdout = TRUE, stderr = TRUE)
        if (grepl("error: .* did not match any file", res[1L])) {
            res <- system2("git", c("checkout", "-b", branch),
                           stdout = TRUE, stderr = TRUE)
        }

        res <- system2("git", c("pull", bundle, branch), ...)
    }

    system2("git", c("checkout", current))
    invisible(NULL)
}

git_bundle_clone <-
function(bundle, dir.name, parent.dir, ...,
         branch.name = c("main")) {

    if (dir.exists(file.path(parent.dir, dir.name)))
        stop("directory ", sQuote(dir.name), " already exists. Maybe pull?")

    current.dir <- getwd()
    on.exit(setwd(current.dir))

    refs <- system2("git", c("bundle", "list-heads",
                             shQuote(normalizePath(bundle))),
                    stderr = TRUE, stdout = TRUE)
    refs <- refs[grepl("refs/heads", refs)]
    refs <- sub("^[0-9a-f]+ refs/heads/", "", refs)

    b <- branch.name[1L]
    if (is.null(b) || !b %in% refs) {
        b <- refs[1L]
    }

    setwd(parent.dir)
    ## system2("git",
    ##         c("clone",
    ##           shQuote(normalizePath(bundle)), dir.name, "-b", b), ...)
    system2("git",
            c("clone",
              shQuote(normalizePath(bundle)), dir.name))
    message(cli::col_green("[done]"))
}
