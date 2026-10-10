.ft_py_packages <- c("numpy", "pandas", "opencv-python-headless", "pillow", "requests",
                     "torch", "torchvision", "transformers>=4.45", "ultralytics>=8.3",
                     "open_clip_torch>=2.24")
.ft_py_modules <- c("numpy", "pandas", "cv2", "PIL", "requests", "torch", "transformers",
                    "ultralytics", "open_clip")

#' Install the 'Python' back end
#'
#' Creates a 'Python' environment and installs the packages that the photo back
#' end needs: 'PyTorch', 'transformers' (Grounding DINO, DINOv2),
#' 'ultralytics' (SAM 2.1), 'open_clip_torch' and 'OpenCV'. This is needed only
#' for [ft_process()] and [ft_extended_traits()]; everything else in
#' \pkg{FlowerTrait} runs in R alone. Installation downloads about 2-3 GB, and
#' the first run downloads the model weights (about 1.5 GB).
#'
#' @param envname Name of the environment.
#' @param method `"virtualenv"` (default) or `"conda"`.
#' @param python Optional path to the 'Python' executable used to create a
#'   virtual environment (version 3.9 or later).
#' @param cpu_only If `TRUE`, install the CPU build of 'PyTorch' (smaller; no
#'   GPU support).
#' @param pip_options Extra options passed to `pip install`.
#' @return `envname`, invisibly.
#' @seealso [ft_use_backend()], [ft_backend_available()]
#' @examples
#' \dontrun{
#' ft_install()                 # once
#' ft_install(cpu_only = TRUE)  # laptop without a GPU
#' }
#' @export
ft_install <- function(envname = "r-FlowerTrait", method = c("virtualenv", "conda"),
                       python = NULL, cpu_only = FALSE, pip_options = character()) {
  method <- match.arg(method)
  pkgs <- .ft_py_packages
  if (method == "virtualenv") {
    if (!reticulate::virtualenv_exists(envname)) {
      args <- list(envname = envname)
      if (!is.null(python)) args$python <- python
      do.call(reticulate::virtualenv_create, args)
    }
    if (cpu_only) {
      reticulate::virtualenv_install(envname, c("torch", "torchvision"),
                                     pip_options = c("--index-url",
                                                     "https://download.pytorch.org/whl/cpu"))
      pkgs <- setdiff(pkgs, c("torch", "torchvision"))
    }
    reticulate::virtualenv_install(envname, pkgs, pip_options = pip_options)
  } else {
    if (!envname %in% reticulate::conda_list()$name) {
      reticulate::conda_create(envname, python_version = "3.11")
    }
    if (cpu_only) {
      reticulate::conda_install(envname, c("torch", "torchvision"), pip = TRUE,
                                pip_options = c("--index-url",
                                                "https://download.pytorch.org/whl/cpu"))
      pkgs <- setdiff(pkgs, c("torch", "torchvision"))
    }
    reticulate::conda_install(envname, pkgs, pip = TRUE, pip_options = pip_options)
  }
  message("Installed. In a new R session call ft_use_backend(\"", envname, "\").")
  invisible(envname)
}

#' Select and check the 'Python' back end
#'
#' `ft_use_backend()` selects the environment made by [ft_install()] for the
#' rest of the R session (it sets the option `FlowerTrait.python`).
#' `ft_backend_available()` reports whether all back-end modules can be
#' imported. The back end runs in a separate 'Python' process, so 'PyTorch' is
#' never loaded into the R session. Without `ft_use_backend()`, the 'Python'
#' found by 'reticulate' is used.
#'
#' @inheritParams ft_install
#' @param python Path to a 'Python' executable with the back-end packages
#'   (overrides `envname`).
#' @return `ft_use_backend()`: the path of the 'Python' executable, invisibly.
#'   `ft_backend_available()`: `TRUE` or `FALSE`.
#' @examples
#' \dontrun{
#' ft_use_backend()
#' ft_backend_available()
#' }
#' @export
ft_use_backend <- function(envname = "r-FlowerTrait", method = c("virtualenv", "conda"),
                           python = NULL) {
  method <- match.arg(method)
  if (is.null(python)) {
    python <- if (method == "virtualenv") {
      reticulate::virtualenv_python(envname)
    } else {
      reticulate::conda_python(envname)
    }
  }
  if (!file.exists(python)) stop("Python not found: ", python, call. = FALSE)
  options(FlowerTrait.python = python)
  invisible(python)
}

#' @rdname ft_use_backend
#' @export
ft_backend_available <- function(python = NULL) {
  py <- tryCatch(.ft_python(python), error = function(e) "")
  if (!nzchar(py) || !file.exists(py)) return(FALSE)
  code <- paste0("import ", paste(.ft_py_modules, collapse = ", "))
  st <- suppressWarnings(system2(py, c("-c", shQuote(code)), stdout = FALSE, stderr = FALSE))
  identical(as.integer(st), 0L)
}

.ft_flower_words <- c("flower", "flowers", "blossom", "bloom", "floret", "flower head", "flowerhead",
                      "capitulum", "inflorescence", "petal", "corolla", "daisy", "orchid", "lily", "rose",
                      "tulip", "poppy", "iris", "sunflower")

#' Process flower photos with the back end
#'
#' Runs the validated photo pipeline on each photo:
#' \enumerate{
#'   \item 'Grounding DINO' (base) with the prompt "flower" on the whole photo
#'     (reduced to 480-800 px) finds the main flower: the largest of the boxes
#'     scoring at least 85% of the best box, unless it encloses two or more other
#'     confident flowers (an inflorescence). Other flowers with a score of 0.30
#'     or more are counted, and flowers cut by the frame are flagged;
#'   \item a square crop with a 15% margin is taken at native resolution
#'     (longer side at most 1024 px);
#'   \item 'Grounding DINO' runs again inside the crop and the same rule picks a
#'     box centred in the middle 60% (two-stage detection);
#'   \item 'SAM' 2.1-B outlines the flower from that box; two jittered boxes
#'     (0.9 and 1.1 times) give the outline stability, and the stage-1 box gives
#'     the agreement between the two stages;
#'   \item the outline is clipped to the box plus 10%, the largest part is kept
#'     and holes are filled; quality metrics are computed;
#'   \item the photo filter (logistic heads on 'OpenCLIP' and 'DINOv2'
#'     embeddings plus quality metrics) gives the probabilities that the photo is
#'     usable, shows the flower face-on, shows an open flower, shows the whole
#'     flower and is sharp;
#'   \item core traits (crop at most 1024 px) and extended traits (crop at most
#'     768 px) are computed.
#' }
#' Results are written to `out_dir` (`traits.csv`, `crops/`, `masks/`,
#' `photos/` for downloaded photos) and read back with [ft_read_results()], which
#' applies a decision rule with [ft_filter()]. Runs can be resumed: photos
#' already in `traits.csv` are skipped.
#'
#' The prompt, photo filter and trait catalogue were validated for flowers of
#' angiosperms. Other prompts are possible but untested and give a warning.
#'
#' @param photos Photos to process: one photo (file path or URL), a character
#'   vector of paths or URLs, a folder (searched recursively; photos in
#'   sub-folders named after species get that species, see [ft_read_folder()]),
#'   or a data frame with a `url` or `path` column (for example from
#'   [ft_gbif_photos()], [ft_inat_photos()] or [ft_read_folder()]). Other
#'   columns such as `photo_id` and `species` are kept.
#' @param out_dir Output folder.
#' @param prompt Detector text prompt. The validated prompt is `"flower"`.
#' @param two_stage Run the second detection inside the crop (validated
#'   default). `FALSE` is faster but less accurate.
#' @param photo_filter Run the photo filter (needed for the default decision
#'   rule).
#' @param embeddings Save the photo-filter features of each photo
#'   (`embeddings/<photo_id>.npz`), for example to train new filter heads on
#'   your own labels.
#' @param sam_weights Path to the 'SAM' 2.1 weights; downloaded on first use.
#'   The default is a file in the user cache folder of \pkg{FlowerTrait}.
#' @param limit Optional maximum number of photos (for a test run).
#' @param rule Decision rule applied to the results; see [ft_filter()].
#' @param white_balance Colour correction applied to the crop before the traits
#'   are computed (the photo filter always sees the uncorrected crop); see
#'   [ft_white_balance()]. The validated default is `"none"`.
#' @param threads Optional number of CPU threads for 'PyTorch'.
#' @param python Optional path to the 'Python' executable; see
#'   [ft_use_backend()].
#' @return A `ft_results` data frame (see [ft_read_results()]).
#' @seealso [ft_install()], [ft_filter()], [ft_contact_sheet()]
#' @examples
#' \dontrun{
#' ft_use_backend()
#' ex <- ft_example_photos()
#' res <- ft_process(ex$photo, out_dir = file.path(tempdir(), "ft_run"))
#' summary(res)
#'
#' # flowering observations found on GBIF
#' ph <- ft_gbif_photos("Papaver rhoeas", n = 20)
#' res <- ft_process(ph, out_dir = "PATH_TO_OUTPUT_FOLDER")
#'
#' # one photo, or a folder with one sub-folder per species
#' res1 <- ft_process("PATH_TO_ONE_PHOTO.jpg", out_dir = "PATH_TO_OUTPUT_FOLDER")
#' sp <- ft_taxonomy(ft_read_folder("PATH_TO_SPECIES_FOLDERS"))
#' res2 <- ft_process(sp, out_dir = "PATH_TO_OUTPUT_FOLDER")
#' }
#' @export
ft_process <- function(photos, out_dir, prompt = "flower", two_stage = TRUE,
                       photo_filter = TRUE, embeddings = FALSE, sam_weights = NULL, limit = NULL,
                       rule = "default", white_balance = c("none", "gray_world", "white_patch",
                                                           "shades_of_gray", "gray_pixels"),
                       threads = NULL, python = NULL) {
  white_balance <- match.arg(white_balance)
  .ft_check_prompt(prompt)
  items <- .ft_items(photos)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  inp <- file.path(out_dir, "input.csv")
  utils::write.csv(items, inp, row.names = FALSE)
  if (is.null(sam_weights)) {
    cache <- tools::R_user_dir("FlowerTrait", "cache")
    dir.create(cache, showWarnings = FALSE, recursive = TRUE)
    sam_weights <- file.path(cache, "sam2.1_b.pt")
  }
  argv <- c("--input", normalizePath(inp), "--out", normalizePath(out_dir),
            "--url-col", "path", "--id-col", "photo_id", "--prompt", prompt,
            "--sam-weights", sam_weights)
  if (!two_stage) argv <- c(argv, "--one-stage")
  if (!photo_filter) argv <- c(argv, "--no-filter")
  if (embeddings) argv <- c(argv, "--embeddings")
  if (!is.null(threads)) argv <- c(argv, "--threads", as.character(as.integer(threads)))
  if (!is.null(limit)) argv <- c(argv, "--limit", as.character(as.integer(limit)))
  if (white_balance != "none") argv <- c(argv, "--white-balance", white_balance)
  .ft_run_python("flowertrait.run_photos", argv, python)
  ft_read_results(out_dir, rule = if (photo_filter) rule else "qc")
}

#' Read back-end results
#'
#' Reads `traits.csv` written by [ft_process()] (or by the command-line tool
#' `python -m flowertrait.run_photos`), adds the columns of `input.csv` when it
#' is present (for example species and occurrence key from [ft_gbif_photos()]),
#' and applies a decision rule with [ft_filter()].
#'
#' @param x Output folder of [ft_process()] or path to a `traits.csv` file.
#' @inheritParams ft_filter
#' @return A data frame of class `ft_results`, one row per photo, with the
#'   back-end columns, `keep`, `reason` and `view`. The output folder is stored
#'   in `attr(x, "dir")`.
#' @examples
#' f <- system.file("extdata", "example_results.csv", package = "FlowerTrait")
#' res <- ft_read_results(f)
#' table(res$reason)
#' @export
ft_read_results <- function(x, rule = "default") {
  f <- if (dir.exists(x)) file.path(x, "traits.csv") else x
  if (!file.exists(f)) stop("No results file found: ", f, call. = FALSE)
  res <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
  inp <- file.path(dirname(f), "input.csv")
  if (file.exists(inp)) {
    meta <- utils::read.csv(inp, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
    add <- setdiff(names(meta), c(names(res), "path"))
    if (length(add)) res <- cbind(res, meta[match(res$photo_id, meta$photo_id), add, drop = FALSE])
  }
  for (k in intersect(c("mask_ok", "cut_by_frame", "keep"), names(res))) res[[k]] <- .ft_lgl(res[[k]])
  if (!"p_usable" %in% names(res) && rule %in% c("default", "strict", "face_view")) {
    warning("No photo-filter columns; using rule = \"qc\".", call. = FALSE)
    rule <- "qc"
  }
  res <- ft_filter(res, rule = rule)
  rownames(res) <- NULL
  attr(res, "dir") <- dirname(f)
  res
}

#' Extended traits from crops and outlines (back end)
#'
#' Computes the full trait set of the back end (colour classes and colour names,
#' granularity, texture, pattern landscape metrics, radial pattern, symmetry and
#' shape) for crops and their
#' outlines, for example to re-measure outlines corrected by hand. Requires the
#' 'Python' back end (only 'NumPy', 'pandas' and 'OpenCV' are used).
#'
#' @param image Paths to crops (JPEG or PNG).
#' @param mask Paths to outline masks (PNG, non-zero = flower), one per crop.
#' @param max_side Longer side (pixels) used for the extended traits; 768 is the
#'   validated value.
#' @inheritParams ft_process
#' @return A data frame with one row per crop: `photo_id`, quality metrics and
#'   traits (`status` is `"ok"` or the reason for missing traits).
#' @examples
#' \dontrun{
#' ft_use_backend()
#' ex <- ft_example_photos()
#' ft_extended_traits(ex$crop, ex$mask)
#' }
#' @export
ft_extended_traits <- function(image, mask, max_side = 768, python = NULL) {
  if (length(image) != length(mask)) stop("Give one mask per image.", call. = FALSE)
  miss <- c(image, mask)[!file.exists(c(image, mask))]
  if (length(miss)) stop("Files not found: ", paste(miss, collapse = ", "), call. = FALSE)
  tmp <- tempfile("ft_ext_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  pairs <- data.frame(photo_id = .ft_ids(image), image = normalizePath(image),
                      mask = normalizePath(mask), stringsAsFactors = FALSE)
  utils::write.csv(pairs, file.path(tmp, "pairs.csv"), row.names = FALSE)
  out <- file.path(tmp, "extended.csv")
  .ft_run_python("flowertrait.api", c("--pairs", file.path(tmp, "pairs.csv"), "--out", out,
                                      "--max-side", as.character(as.integer(max_side))), python)
  utils::read.csv(out, stringsAsFactors = FALSE, colClasses = c(photo_id = "character"))
}

.ft_python <- function(python = NULL) {
  py <- python %||% getOption("FlowerTrait.python") %||% Sys.getenv("FLOWERTRAIT_PYTHON", "")
  if (!nzchar(py)) py <- reticulate::py_config()$python
  py
}

# Runs a back-end module in a separate Python process (PyTorch is never loaded
# into the R session, which avoids thread-library conflicts with R).
.ft_run_python <- function(module, args, python = NULL) {
  py <- .ft_python(python)
  if (!file.exists(py)) stop("Python not found: ", py, ". See ft_install().", call. = FALSE)
  old <- Sys.getenv("PYTHONPATH", unset = NA)
  pp <- system.file("python", package = "FlowerTrait")
  Sys.setenv(PYTHONPATH = if (is.na(old) || !nzchar(old)) pp else
    paste(pp, old, sep = .Platform$path.sep))
  on.exit(if (is.na(old)) Sys.unsetenv("PYTHONPATH") else Sys.setenv(PYTHONPATH = old))
  st <- system2(py, c("-m", module, shQuote(args)))
  if (!identical(as.integer(st), 0L)) {
    stop("The Python back end stopped with status ", st, "; see the messages above.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.ft_check_prompt <- function(prompt) {
  words <- tolower(trimws(unlist(strsplit(prompt, "[.,;]"))))
  ok <- length(words) > 0 && all(words %in% .ft_flower_words)
  if (!ok) {
    warning("FlowerTrait is validated with the prompt 'flower'; prompt '", prompt,
            "' is untested.", call. = FALSE)
  }
  invisible(ok)
}

.ft_items <- function(photos) {
  if (is.data.frame(photos)) {
    src <- if ("path" %in% names(photos) && any(!is.na(photos$path))) "path" else "url"
    if (!src %in% names(photos)) stop("`photos` needs a `url` or `path` column.", call. = FALSE)
    items <- photos
    items$path <- photos[[src]]
    if (!"photo_id" %in% names(items)) items$photo_id <- .ft_ids(items$path)
    items <- items[!is.na(items$path), , drop = FALSE]
  } else if (is.character(photos) && length(photos) == 1L && dir.exists(photos)) {
    items <- ft_read_folder(photos)
  } else if (is.character(photos)) {
    local <- !grepl("^https?://", photos)
    if (any(local & !file.exists(photos))) {
      stop("Files not found: ", paste(photos[local & !file.exists(photos)], collapse = ", "),
           call. = FALSE)
    }
    p <- photos
    p[local] <- normalizePath(p[local])
    items <- data.frame(photo_id = .ft_ids(photos), path = p, stringsAsFactors = FALSE)
  } else {
    stop("`photos` must be a folder, file paths, URLs or a data frame.", call. = FALSE)
  }
  if (anyDuplicated(items$photo_id)) stop("`photo_id` values must be unique.", call. = FALSE)
  items$photo_id <- as.character(items$photo_id)
  items[, c("photo_id", "path", setdiff(names(items), c("photo_id", "path"))), drop = FALSE]
}

.ft_ids <- function(p) {
  tools::file_path_sans_ext(basename(sub("\\?.*$", "", p)))
}

.ft_lgl <- function(v) {
  if (is.logical(v)) return(v)
  out <- toupper(trimws(as.character(v))) %in% c("TRUE", "T", "1")
  out[is.na(v) | trimws(as.character(v)) == ""] <- NA
  out
}
