#' Replace categorical dummy variables (and their interactions) with a continuous version
#'
#' Given a model formula, removes all main-effect and interaction terms that involve
#' a set of categorical dummy variables, and replaces them with a continuous variable
#' (main effect + corresponding interactions).
#'
#' Special calls such as \code{strata()}, \code{rcs()}, \code{ns()}, etc. are preserved.
#'
#' @param formula   A \code{formula} object.
#' @param cat_vars  Character vector of categorical dummy names to remove (e.g.
#'                  \code{c("cat_low", "cat_high")}).
#' @param cont_var  Character string giving the continuous replacement variable
#'                  (e.g. \code{"score"}).
#' @param add_main  Logical. Should the main effect of \code{cont_var} be added?
#'                  Default \code{TRUE}.
#'
#' @return A new \code{formula} with the categorical terms replaced.
#'
#' @examples
#' f <- Surv(time, event) ~ age + cat_low + cat_high + cat_low:x + cat_high:x + strata(z)
#' replace_cat_with_continuous(f, c("cat_low", "cat_high"), "score")
#' # Surv(time, event) ~ age + strata(z) + score + score:x
replace_cat_with_continuous <- function(formula, cat_vars, cont_var, add_main = TRUE) {
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula object.", call. = FALSE)
  }
  cat_vars <- as.character(cat_vars)
  cont_var <- as.character(cont_var)[1L]

  tf     <- terms(formula, keep.order = TRUE)
  labels <- attr(tf, "term.labels")

  # ------------------------------------------------------------------
  # 1. Identify labels that contain any of the categorical variables
  # ------------------------------------------------------------------
  has_cat <- vapply(labels, function(lbl) {
    parts <- strsplit(lbl, "(?<!:):(?!:)", perl = TRUE)[[1]]
    any(parts %in% cat_vars)
  }, logical(1), USE.NAMES = FALSE)

  # ------------------------------------------------------------------
  # 2. Collect interaction partners (everything in the removed term
  #    except the cat_vars themselves)
  # ------------------------------------------------------------------
  partners <- if (any(has_cat)) {
    unique(unlist(lapply(labels[has_cat], function(lbl) {
      parts <- strsplit(lbl, "(?<!:):(?!:)", perl = TRUE)[[1]]
      setdiff(parts, cat_vars)
    })))
  } else {
    character(0)
  }
  # Drop any partners that are also being removed (e.g. cat1:cat2)
  partners <- setdiff(partners, cat_vars)

  # ------------------------------------------------------------------
  # 3. Build the new RHS term list
  # ------------------------------------------------------------------
  new_labels <- labels[!has_cat]

  if (add_main && !(cont_var %in% new_labels)) {
    new_labels <- c(new_labels, cont_var)
  }

  for (p in partners) {
    int_term <- paste(cont_var, p, sep = ":")
    if (!(int_term %in% new_labels)) {
      new_labels <- c(new_labels, int_term)
    }
  }

  # ------------------------------------------------------------------
  # 4. Reconstruct the formula preserving the response & environment
  # ------------------------------------------------------------------
  resp <- deparse(formula[[2]], width.cutoff = 500L)
  if (length(new_labels) == 0L) {
    new_rhs <- "1"
  } else {
    new_rhs <- paste(new_labels, collapse = " + ")
  }

  new_formula <- stats::as.formula(paste(resp, "~", new_rhs),
                                   env = environment(formula))
  new_formula
}
