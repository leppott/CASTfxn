# Package Usage
# Erik.Leppo@tetratech.com
# 2026-02-10
# Copilot code

# Run from the package root: source("tools/usage_report.R")
# Output: a usage data frame and a printed table of used/unused packages.

suppressWarnings({
  if (!requireNamespace("desc", quietly = TRUE)) {
    stop("Please install 'desc' package: install.packages('desc')")
  }
})

library(utils)

pkg_root <- "."
r_dir <- file.path(pkg_root, "R")
ns_file <- file.path(pkg_root, "NAMESPACE")
desc_file <- file.path(pkg_root, "DESCRIPTION")

stopifnot(dir.exists(r_dir))
stopifnot(file.exists(desc_file))

# ---- 1) Read DESCRIPTION: Imports, Depends, Suggests ----
get_desc_pkgs <- function(desc) {
  # Return a named list of character vectors of package names per field
  fields <- c("Imports", "Depends", "Suggests")
  out <- lapply(fields, function(f) {
    txt <- desc$get(f)
    if (length(txt) == 0 || is.na(txt)) return(character())
    # Split by commas, strip version constraints
    pkgs <- unlist(strsplit(txt, ","))
    pkgs <- gsub("\\(.*?\\)", "", pkgs)       # remove (>= x.y.z)
    pkgs <- trimws(pkgs)
    pkgs <- pkgs[pkgs != ""]
    # Drop base pkgs that sometimes appear in Depends
    pkgs[!pkgs %in% c("R")]
  })
  names(out) <- fields
  out
}

desc <- desc::desc(file = desc_file)
declared <- get_desc_pkgs(desc)
declared$All <- unique(c(declared$Imports, declared$Depends))  # runtime-relevant
declared$AllPlusSuggests <- unique(c(declared$Imports, declared$Depends, declared$Suggests))

# ---- 2) Parse NAMESPACE to map importFrom/import ----
parse_namespace_imports <- function(ns_path) {
  imp_map <- list()      # pkg -> set of functions imported via importFrom
  imported_pkgs <- character()  # packages imported via 'import()' (all)
  
  if (!file.exists(ns_path)) {
    return(list(importFrom = imp_map, importAll = imported_pkgs))
  }
  
  lines <- readLines(ns_path, warn = FALSE)
  # importFrom(pkg, fun1, fun2, ...)
  imp_from_lines <- grep("^\\s*importFrom\\(", lines, value = TRUE)
  for (ln in imp_from_lines) {
    inside <- sub("^\\s*importFrom\\((.*)\\)\\s*$", "\\1", ln)
    # split by comma, first token is pkg, rest are funs
    tokens <- trimws(strsplit(inside, ",")[[1]])
    if (length(tokens) >= 2) {
      pkg <- gsub("^['\"]|['\"]$", "", tokens[1])
      funs <- gsub("^['\"]|['\"]$", "", tokens[-1])
      if (is.null(imp_map[[pkg]])) imp_map[[pkg]] <- character()
      imp_map[[pkg]] <- unique(c(imp_map[[pkg]], funs))
    }
  }
  # import(pkg)
  imp_all_lines <- grep("^\\s*import\\(", lines, value = TRUE)
  if (length(imp_all_lines)) {
    for (ln in imp_all_lines) {
      inside <- sub("^\\s*import\\((.*)\\)\\s*$", "\\1", ln)
      pkg <- trimws(gsub("^['\"]|['\"]$", "", inside))
      imported_pkgs <- unique(c(imported_pkgs, pkg))
    }
  }
  list(importFrom = imp_map, importAll = imported_pkgs)
}

ns_info <- parse_namespace_imports(ns_file)

# ---- 3) Read all R files and collect text ----
r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
r_txt <- vapply(r_files, function(fp) paste(readLines(fp, warn = FALSE), collapse = "\n"), character(1))

# ---- 4) Count qualified calls: pkg::fun and pkg:::fun ----
# Return a data.frame with columns: pkg, fun, n_calls, accessor ('::' or ':::')
count_qualified_calls <- function(texts) {
  # captures: package, accessor, function
  # e.g., foo::bar(, or foo:::baz( ; allow names with . and numbers and backticks around fun occasionally
  pattern <- "(?<![A-Za-z0-9_.])([A-Za-z][A-Za-z0-9._]*)\\s*(::|:::)\\s*`?([A-Za-z][A-Za-z0-9._]*)`?"
  m <- regexec(pattern, texts, perl = TRUE)
  # Apply per string to get multiple matches
  out <- data.frame(pkg = character(), fun = character(), accessor = character(), stringsAsFactors = FALSE)
  for (i in seq_along(texts)) {
    tx <- texts[[i]]
    hits <- gregexpr(pattern, tx, perl = TRUE)
    if (length(hits) && hits[[1]][1] != -1) {
      starts <- as.integer(hits[[1]])
      lens   <- attr(hits[[1]], "match.length")
      for (j in seq_along(starts)) {
        s <- starts[j]
        e <- s + lens[j] - 1
        seg <- substr(tx, s, e)
        subm <- regexec(pattern, seg, perl = TRUE)
        gr <- regmatches(seg, subm)[[1]]
        if (length(gr) >= 4) {
          out <- rbind(out, data.frame(pkg = gr[2], accessor = gr[3], fun = gr[4], stringsAsFactors = FALSE))
        }
      }
    }
  }
  if (nrow(out)) {
    aggregate(list(n_calls = rep(1, nrow(out))), by = out[, c("pkg", "fun", "accessor")], FUN = sum)
  } else {
    data.frame(pkg = character(), fun = character(), accessor = character(), n_calls = integer())
  }
}

qual_calls <- count_qualified_calls(r_txt)

# ---- 5) Count unqualified calls for importFrom functions ----
# Heuristic: if NAMESPACE says importFrom(pkg, f), count bare occurrences of f(...) not part of ::,
# not in assignment of function definition "f <- function("
count_unqualified_importFrom <- function(texts, ns_import_map) {
  rows <- list()
  for (pkg in names(ns_import_map)) {
    for (f in ns_import_map[[pkg]]) {
      # regex for bare function call like  f(  where f is a whole token
      # Avoid :: and ::: patterns, avoid function definition patterns.
      # We'll count occurrences of "\\bf\\s*\\(" that are NOT preceded by ":" within 2 chars, and not followed by "<-\\s*function\\s*\\("
      pat_call <- paste0("(?<!:|: )\\b", f, "\\s*\\(")
      pat_def  <- paste0("\\b", f, "\\s*<-\\s*function\\s*\\(")
      n <- 0L
      for (tx in texts) {
        n_call <- length(gregexpr(pat_call, tx, perl = TRUE)[[1]])
        if (n_call == 1 && gregexpr(pat_call, tx, perl = TRUE)[[1]][1] == -1) n_call <- 0
        n_def <- length(gregexpr(pat_def, tx, perl = TRUE)[[1]])
        if (n_def == 1 && gregexpr(pat_def, tx, perl = TRUE)[[1]][1] == -1) n_def <- 0
        n <- n + max(0L, n_call - n_def)
      }
      if (n > 0) {
        rows[[length(rows) + 1]] <- data.frame(pkg = pkg, fun = f, accessor = "importFrom(bare)", n_calls = n, stringsAsFactors = FALSE)
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame(pkg = character(), fun = character(), accessor = character(), n_calls = integer())
}

unqual_calls <- count_unqualified_importFrom(r_txt, ns_info$importFrom)


# ---- 6) Combine usage counts per package ----
usage_raw <- rbind(qual_calls, unqual_calls)
pkg_usage_counts <- if (nrow(usage_raw)) aggregate(list(total_calls = usage_raw$n_calls),
                                                   by = usage_raw["pkg"],
                                                   FUN = sum) else
                                                     data.frame(pkg = character(), total_calls = integer())

# Ensure all declared packages are represented
all_declared <- declared$AllPlusSuggests
if (length(all_declared)) {
  missing_rows <- setdiff(all_declared, pkg_usage_counts$pkg)
  if (length(missing_rows)) {
    pkg_usage_counts <- rbind(pkg_usage_counts, data.frame(pkg = missing_rows, total_calls = 0L))
  }
}
pkg_usage_counts$declared_in <- ifelse(pkg_usage_counts$pkg %in% declared$Imports, "Imports",
                                       ifelse(pkg_usage_counts$pkg %in% declared$Depends, "Depends",
                                              ifelse(pkg_usage_counts$pkg %in% declared$Suggests, "Suggests", "")))

# Sort: runtime declared first, by usage desc
ord <- order(factor(pkg_usage_counts$declared_in, levels = c("Imports", "Depends", "Suggests", "")),
             -pkg_usage_counts$total_calls, pkg_usage_counts$pkg)
pkg_usage_counts <- pkg_usage_counts[ord, ]

# ---- 7) Print report ----
cat("\n=== Package usage summary ===\n")
print(pkg_usage_counts, row.names = FALSE)
cat("\nNotes:\n",
    "- 'total_calls' includes pkg::fun, pkg:::fun, and bare calls to functions imported via NAMESPACE.\n",
    "- A package with total_calls == 0 but listed in Imports/Depends is a candidate to move/remove.\n",
    "- Consider tests, examples, and vignettes separately if you use those (see extension hints in script).\n")

# ---- 8) (Optional) Show detailed per-function usage ----
if (nrow(usage_raw)) {
  detailed <- aggregate(list(n_calls = usage_raw$n_calls),
                        by = usage_raw[, c("pkg", "fun", "accessor")],
                        FUN = sum)
  cat("\n=== Detailed function-level usage (top 50) ===\n")
  detailed <- detailed[order(-detailed$n_calls), ]
  print(utils::head(detailed, 50), row.names = FALSE)
}

# Make the data available in the global env if sourced
assign("pkg_usage_summary", pkg_usage_counts, envir = .GlobalEnv)
assign("pkg_usage_detailed", if (exists("detailed")) detailed else usage_raw, envir = .GlobalEnv)

    
    