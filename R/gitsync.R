pull_from_bundles <-
function(root.dir,
         bundle.dir,
         path.separator = "FORWARDSLASH",
         ...) {

    bundles <- dir(bundle.dir)
    for (bundle in bundles) {

        b.path <- sub("[.]bundle$", "", bundle)
        b.path <- gsub(path.separator,
                       .Platform$file.sep,
                       b.path)

        message(b.path, appendLF = FALSE)

        if (!dir.exists(file.path(root.dir, b.path))) {

            message("\n   ===> dir does not exist -- [clone]")
            tmp <- sub(paste0(".*", slash), "", bundle)
            dir.name <- sub(r"(.bundle$)", "", tmp)
            parent.dir <- file.path(root.dir, dirname(gsub(slash, "/", bundle)))
            if (!dir.exists(parent.dir))
                dir.create(parent.dir, recursive = TRUE)
            msg <- try(git_bundle_clone(file.path("~",
                                                  "Sharepoint",
                                                  "backup_git", bundle),
                                        dir.name = dir.name,
                                        parent.dir = parent.dir,
                                        branch = "master",
                                        stdout = TRUE, stderr = TRUE), silent = TRUE)
            if (inherits(msg, "try-error")) {
                msg <- git_bundle_clone(file.path("~",
                                                  "Sharepoint",
                                                  "backup_git", bundle),
                                        dir.name = dir.name,
                                        parent.dir = parent.dir,
                                        branch = "main",
                                        stdout = TRUE, stderr = TRUE)
            }
            cat(paste("    ", msg), sep = "\n")
            message("\n")
        }
        if (!dir.exists(file.path(root.dir, b.path, ".git"))) {
            message("\n   ===> dir exists, but no .git repository -- [skip]")
            next
        }

        d <- normalizePath(file.path(root.dir, b.path))
        bn <- names(git2r::branches(d))
        if ("master" %in% bn) {
            br.name <- "master"
        } else
            br.name <- "main"
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

create_bundles <-
function(root.dir,
         bundle.dir,
         exclude.re = NULL,
         dir.list = NULL,
         path.separator = "FORWARDSLASH",
         ...,
         max.char) {

    pwd <- getwd()
    on.exit(setwd(pwd))

    git <- git_paths(root.dir, max.char = 259)
    for (x in exclude.re) {
        git <- git[!grepl(x, git)]
    }

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
         max.char = NA,...) {

    f <- dir(path = path,
             include.dirs = TRUE,
             recursive = TRUE,
             all.files = TRUE)
    if (is.finite(max.char))
        f <- f[nchar(f) <= max.char]
    f <- f[basename(f) == ".git"]
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
         overwrite = TRUE) {

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
        system2("git",
                c("bundle", "create", bundle,
                  "--branches", "--tags"))
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
function(bundle, dir.name, parent.dir,
         branch = "master", ...) {

    if (dir.exists(file.path(parent.dir, dir.name)))
        stop("directory ", sQuote(dir.name), " already exists. Maybe pull?")

    current.dir <- getwd()
    on.exit(setwd(current.dir))

    setwd(parent.dir)
    system2("git", c("clone", "-b", branch, bundle, dir.name), ...)
}
