#' Keep usable photos with a validated decision rule
#'
#' Applies one of the decision rules evaluated on the benchmark photos of the
#' validation. Checks are made in this order and the first failed check gives
#' the `reason`:
#' \enumerate{
#'   \item processing error;
#'   \item outline quality (`mask_ok`: a detection, at least 600 outline pixels
#'     and at most 10% of the crop border touched);
#'   \item outline stability `stab >= stability` (rules `"strict"` and
#'     `"face_view"` only);
#'   \item photo filter `p_usable >= p_usable` (an open flower that is entirely
#'     visible and sharp);
#'   \item rule `"face_view"` only: the flower is seen face-on
#'     (`p_face_view >= p_face`).
#' }
#' Kept photos get a `view`: `"face"` or `"side"`. Other flowers in the photo
#' (`n_other_flowers`) are reported but are not a reason for rejection, because
#' most flowering plants are photographed with several flowers.
#'
#' The share of photos kept and of wrong outlines among them for each rule, in
#' the benchmark photos, are given in [ft_validation] and the package article.
#'
#' @param x Data frame with the back-end columns (`mask_ok`, `stab`,
#'   `p_usable`, `p_face_view`), such as [ft_read_results()] or
#'   [ft_validation].
#' @param rule `"default"` (outline QC and photo filter), `"strict"` (default
#'   plus outline stability), `"face_view"` (strict plus face view), `"qc"`
#'   (outline QC only) or `"none"`.
#' @param p_usable,stability,p_face Thresholds; the validated values are 0.5,
#'   0.8 and 0.5.
#' @return `x` with the columns `keep`, `reason` and `view`.
#' @examples
#' res <- ft_example_results()
#' ft_filter(res, "strict")[, c("photo_id", "keep", "reason")]
#'
#' # the validation photos: share kept by each rule
#' sapply(c("qc", "default", "strict", "face_view"),
#'        function(r) mean(ft_filter(ft_validation, r)$keep))
#' @export
ft_filter <- function(x, rule = c("default", "strict", "face_view", "qc", "none"),
                      p_usable = 0.5, stability = 0.8, p_face = 0.5) {
  rule <- match.arg(rule)
  if (!is.data.frame(x)) stop("`x` must be a data frame.", call. = FALSE)
  n <- nrow(x)
  need <- switch(rule, none = character(), qc = "mask_ok", default = c("mask_ok", "p_usable"),
                 strict = c("mask_ok", "stab", "p_usable"),
                 face_view = c("mask_ok", "stab", "p_usable", "p_face_view"))
  miss <- setdiff(need, names(x))
  if (length(miss)) {
    stop("Columns missing for rule \"", rule, "\": ", paste(miss, collapse = ", "), call. = FALSE)
  }
  num <- function(k, fill) {
    v <- if (k %in% names(x)) suppressWarnings(as.numeric(x[[k]])) else rep(fill, n)
    v[is.na(v)] <- fill
    v
  }
  reason <- rep(NA_character_, n)
  fail <- function(cond, why) {
    i <- is.na(reason) & cond
    reason[i] <<- why
  }
  if ("error" %in% names(x)) {
    e <- as.character(x$error)
    fail(!is.na(e) & nzchar(trimws(e)), "processing error")
  }
  if (rule != "none") {
    ok <- .ft_lgl(x$mask_ok)
    fail(is.na(ok) | !ok, "outline failed QC")
  }
  if (rule %in% c("strict", "face_view")) fail(num("stab", 0) < stability, "unstable outline")
  if (rule %in% c("default", "strict", "face_view")) {
    fail(num("p_usable", 0) < p_usable, "unclear photo (no open flower, hidden, blurry or cut off)")
  }
  view <- rep(NA_character_, n)
  if ("p_face_view" %in% names(x)) view <- ifelse(num("p_face_view", 1) >= p_face, "face", "side")
  if (rule == "face_view") fail(view == "side", "side view")
  x$keep <- is.na(reason)
  view[!x$keep] <- NA
  reason[x$keep] <- if (all(is.na(view))) "kept" else paste(view[x$keep], "view")
  x$reason <- reason
  x$view <- view
  if (!inherits(x, "ft_results")) class(x) <- c("ft_results", class(x))
  attr(x, "rule") <- rule
  x
}

#' @export
`[.ft_results` <- function(x, ...) {
  r <- NextMethod()
  if (!is.data.frame(r)) return(r)
  if (all(c("photo_id", "keep") %in% names(r))) {
    attr(r, "rule") <- attr(x, "rule")
    attr(r, "dir") <- attr(x, "dir")
  } else {
    class(r) <- setdiff(class(r), "ft_results")
  }
  r
}

#' @export
print.ft_results <- function(x, ...) {
  cat("<ft_results> ", nrow(x), " photos, ", sum(x$keep, na.rm = TRUE), " kept (rule: ",
      attr(x, "rule") %||% "?", ")\n", sep = "")
  show <- intersect(c("photo_id", "species", "keep", "reason", "D_px", "L_mean", "C_mean",
                      "colour_name", "petal_n"), names(x))
  print(utils::head(as.data.frame(x)[, show, drop = FALSE], 10L), row.names = FALSE)
  if (nrow(x) > 10L) cat("... ", nrow(x) - 10L, " more rows\n", sep = "")
  invisible(x)
}

#' Summarise or plot the decisions in a results table
#'
#' @param object,x A `ft_results` data frame.
#' @param ... Passed to [graphics::barplot()] (plot) or ignored (summary).
#' @return `summary()`: a data frame with the number and share of photos per
#'   decision. `plot()`: the same table, invisibly, after drawing a bar chart.
#' @examples
#' res <- ft_example_results()
#' summary(res)
#' plot(res)
#' @name ft_results
NULL

#' @rdname ft_results
#' @export
summary.ft_results <- function(object, ...) {
  tab <- table(factor(object$reason))
  out <- data.frame(reason = names(tab), n = as.integer(tab), share = as.numeric(tab) / nrow(object),
                    stringsAsFactors = FALSE)
  out$kept <- out$reason %in% unique(object$reason[object$keep])
  out <- out[order(!out$kept, -out$n), c("reason", "kept", "n", "share")]
  rownames(out) <- NULL
  out
}

#' @rdname ft_results
#' @export
plot.ft_results <- function(x, ...) {
  s <- summary.ft_results(x)
  op <- graphics::par(mar = c(4, 17, 2, 1))
  on.exit(graphics::par(op))
  graphics::barplot(rev(s$n), names.arg = rev(s$reason), horiz = TRUE, las = 1,
                    col = rev(ifelse(s$kept, "#2E8B57", "#C0504D")), border = NA,
                    xlab = "Photos", main = paste0("Decisions (rule: ", attr(x, "rule"), ")"), ...)
  invisible(s)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
