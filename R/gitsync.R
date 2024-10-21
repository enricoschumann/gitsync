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

        message(b.path, " ... ", appendLF = FALSE)
        if (!dir.exists(file.path(root.dir, b.path))) {
            if (if.not.exists == "clone") {

                message("dir does not exist -- [clone]")
                parent.dir <- file.path(root.dir, dirname(b.path))
                if (!dir.exists(parent.dir))
                    dir.create(parent.dir, recursive = TRUE)
                msg <- try(git_bundle_clone(file.path(bundle.dir, bundle),
                                            dir.name = basename(b.path),
                                            parent.dir = parent.dir,
                                            stdout = TRUE, stderr = TRUE),
                           silent = TRUE)
                cat(paste("    ", msg), sep = "\n")
                message("\n")
            } else {
                message("dir does not exist -- [skip]")
                next
            }
        }

        if ( dir.exists(file.path(root.dir, b.path)) &&
            !dir.exists(file.path(root.dir, b.path, ".git"))) {
            message("\n   ===> dir exists, but no .git repository -- [skip]")
            next
        }

        d <- normalizePath(file.path(root.dir, b.path))
        bn <- names(git2r::branches(d))
        br.name <- NA
        if (any(c("main", "origin/main") %in% bn)) {
            br.name <- "main"
        } else if (any(c("master", "origin/master") %in% bn)) {
            br.name <- "master"
        } else if (length(bn) == 1L) {
            br.name <- gsub(".*/([^/]+)$", "\\1", bn)
        } else {
            warning("could not determine main branch name: no checkout done")
        }
        if (!is.na(br.name)) {
            msg <- git_bundle_pull(
                shQuote(normalizePath(file.path(bundle.dir, bundle))),
                target = file.path(root.dir, b.path),
                branch = br.name,
                stdout = TRUE,
                stderr = TRUE)
            if (length(msg) == 3 && msg[3] == "Already up to date.") {
                message(crayon::green("   [OK]"), appendLF = TRUE)
            } else {
                message("")
                cat(paste("    ", msg), sep = "\n")
                message("\n\n")
            }
        }
    }
}

create_bundles <-
function(root.dir,
         bundle.dir,
         exclude.re = NULL,
         include.only.re = NULL,
         path.separator = "FORWARDSLASH",
         ...,
         max.char = 259) {

    git <- git_paths(root.dir, max.char = max.char)
    for (x in include.only.re) {
        git <- git[grepl(x, git)]
    }
    if (is.null(include.only.re))
        for (x in exclude.re)
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
}

git_paths <-
function(path = ".",
         sub.tilde = TRUE,
         max.char = NA, ...) {

    f <- list.files(path = path, pattern = "^[.]git$",
                    include.dirs = TRUE,
                    recursive = TRUE, all.files = TRUE)
    if (is.finite(max.char))
        f <- f[nchar(f) <= max.char]
    f <- f[file.info(file.path(path, f))$isdir]
    f <- sort(unique(dirname(f)))
    if (sub.tilde)
        f <- sub(normalizePath(path.expand("~"), winslash = "/"),
                 "~", f, fixed = TRUE)
    class(f) <- c("git_list", class(f))
    f
}

fetch_git_info <-
function(path, ...) {

    paths <- path
    clean <- rep(NA, length(paths))
    remotes <- rep(NA_character_, length(paths))
    remote_urls <- rep(NA_character_, length(paths))
    branches <- rep(NA_character_, length(paths))

    for (path in paths) {
        p <- path == paths
        br <- sort(names(git2r::branches(path, "local")))

        branches[p] <- paste(br, collapse = ";")



        st <- git2r::status(path)
        clean[p] <- !(length(st$staged) || length(st$unstaged) || length(st$untracked))

        rem <- git2r::remotes(path)
        remotes[p] <- paste(rem, collapse = ";")
        remote_urls[p] <- paste(git2r::remote_url(path, remote = rem), collapse = ";")
    }
    data.frame(path = paths,
               machine = Sys.info()[["nodename"]],
               ## user = Sys.info()["user"],
               clean = clean,
               remotes = remotes,
               remote_urls = remote_urls,
               branches = branches,
               check.names = FALSE,
               stringsAsFactors = FALSE)
}

git_bundle_create <-
function(repos, output.filenames,
         output.dir,
         dated.bundle = TRUE,
         overwrite = TRUE,
         ref = c("--branches", "--tags")) {

    if (!dir.exists(output.dir)) {
        ans <- askYesNo("Create directory?")
        if (is.na(ans) || !ans)
            return(invisible(NULL))
        else
            dir.create(path.expand(output.dir))
    }
    current.dir <- getwd()
    on.exit(setwd(current.dir))
    for (i in seq_along(repos)) {
        message(repos[i], "\n",
                " =>")
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
function(bundle, target, branch = "master", ...) {

    if (!dir.exists(target))
        stop("'target' does not exist. Maybe clone?")

    current.dir <- getwd()
    on.exit(setwd(current.dir))

    setwd(target)
    system2("git", c("pull", bundle, branch), ...)
}

git_bundle_clone <-
function(bundle, dir.name, parent.dir, ...) {

    if (dir.exists(file.path(parent.dir, dir.name)))
        stop("directory ", sQuote(dir.name), " already exists. Maybe pull?")

    current.dir <- getwd()
    on.exit(setwd(current.dir))

    setwd(parent.dir)
    system2("git", c("clone", shQuote(normalizePath(bundle)), dir.name), ...)
}
