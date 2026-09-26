# Libraries --------------------------------------------------------------------

library(psych)
library(dplyr)
library(tidyr)
library(haven)
library(descr)
library(ggplot2)
library(readr)
library(openxlsx)

# Functions --------------------------------------------------------------------




#...............................................................................

save_output_to_workbook <- function(wb, sheet_name, output) {
  
  # Remove sheet if it already exists
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  
  # Create worksheet
  addWorksheet(wb, sheet_name)
  
  # Try to convert output to a data frame
  output_df <- tryCatch(
    as.data.frame(output),
    error = function(e) NULL
  )
  
  # If conversion worked, save as a regular table
  if (!is.null(output_df)) {
    
    writeData(
      wb,
      sheet_name,
      output_df,
      rowNames = TRUE
    )
    
  } else {
    
    # If conversion failed, capture the printed output
    output_text <- capture.output(
      print(output)
    )
    
    # Save each printed line in its own row
    writeData(
      wb,
      sheet_name,
      data.frame(Output = output_text),
      rowNames = FALSE,
      colNames = FALSE
    )
  }
}




#...............................................................................

summarise_categorical <- function(data, var) {
  
  var_name <- if (is.character(var)) var else deparse(substitute(var))
  
  data %>%
    count(.data[[var_name]], name = "n") %>%
    mutate(
      percent = round(n / sum(n) * 100, 1),
      variable = var_name
    ) %>%
    relocate(variable)
}




#...............................................................................

summarise_numeric <- function(.data, vars) {
  
  present <- intersect(vars, names(.data))
  
  if (length(present) == 0) {
    stop("None of the requested variables are in the dataset.")
  }
  
  .data %>%
    summarise(
      across(
        all_of(present),
        list(
          n = ~sum(!is.na(.x)),
          mean = ~round(mean(.x, na.rm = TRUE), 2),
          sd = ~round(sd(.x, na.rm = TRUE), 2),
          min = ~round(min(.x, na.rm = TRUE), 2),
          max = ~round(max(.x, na.rm = TRUE), 2),
          skew = ~round(psych::skew(.x, na.rm = TRUE), 2),
          kurtosis = ~round(psych::kurtosi(.x, na.rm = TRUE), 2)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.*)_(n|mean|sd|min|max|skew|kurtosis)"
    )
}



#...............................................................................

desc_by_cell_input <- function(data, group_vars, var) {
  # data: your dataset (e.g., dat_full)
  # group_vars: vector of column names to group by (quoted or unquoted)
  # var: name of the variable to summarize (quoted or unquoted)
  # Accept var as either a symbol (Count) or string ("Count")
  var_name <- if (is.character(var)) var else deparse(substitute(var))
  
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n  = sum(!is.na(.data[[var_name]])),
      M  = mean(.data[[var_name]], na.rm = TRUE),
      SD = sd(.data[[var_name]],  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(variable = var_name) %>%
    relocate(variable)
}


# Example of use: 
# desc_by_cell_input(dat_full, group_vars = c("structure", "motivation"), var = "Count")





#...............................................................................

apa_corr_matrix <- function(
    data,
    vars,
    labels = NULL,                     # optional vector; positional or named mapping var -> label
    type = c("auto","tetrachoric","phi","pearson","spearman","kendall"),
    digits = 2,
    stars = TRUE,
    star_cutoffs = c("***"=0.001,"**"=0.01,"*"=0.05),
    blank_upper = TRUE,
    blank_diag  = TRUE,
    use   = "pairwise",                # for psych::corr.test
    adjust = "none",                   # p adjustment for non-tetra methods
    coerce_factors = FALSE,
    bootstrap_p = FALSE,               # only used for tetrachoric
    B = 500,                           # bootstrap reps
    seed = NULL,
    return = c("table","both")         # "both" also returns r, p, and notes
){
  type <- match.arg(type)
  return <- match.arg(return)
  
  df <- data[, vars, drop = FALSE]
  
  # helper: 0/1 binary?
  is_binary01 <- function(x) is.numeric(x) && all(na.omit(x) %in% c(0,1))
  
  # optional factor -> numeric
  if (coerce_factors) {
    df[] <- lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
  }
  
  # resolve labels
  var_names <- colnames(df)
  if (!is.null(labels)) {
    if (!is.null(names(labels))) {
      match_idx <- match(var_names, names(labels))
      var_names <- ifelse(!is.na(match_idx), labels[match_idx], var_names)
    } else {
      if (length(labels) != ncol(df)) stop("labels must match length of vars or be a named vector.")
      var_names <- labels
    }
  }
  
  # auto type
  if (type == "auto") {
    type <- if (all(vapply(df, is_binary01, logical(1)))) "tetrachoric" else "pearson"
  }
  
  note <- NULL
  r <- p <- NULL
  
  if (type %in% c("pearson","spearman","kendall")) {
    ct <- psych::corr.test(df, use = use, method = type, adjust = adjust)
    r <- ct$r; p <- ct$p
  } else if (type == "phi") {
    if (!all(vapply(df, is_binary01, logical(1)))) {
      stop("type='phi' requires all selected variables to be numeric 0/1.")
    }
    ct <- psych::corr.test(df, use = use, method = "pearson", adjust = adjust)
    r <- ct$r; p <- ct$p
    note <- "Note. Phi correlations (Pearson computed on 0/1 items)."
  } else if (type == "tetrachoric") {
    if (!all(vapply(df, is_binary01, logical(1)))) {
      stop("type='tetrachoric' requires all selected variables to be numeric 0/1.")
    }
    tc <- psych::tetrachoric(df)
    r <- tc$rho
    # default: no p-values for tetrachoric
    p <- matrix(NA_real_, nrow = nrow(r), ncol = ncol(r), dimnames = dimnames(r))
    note <- "Note. Tetrachoric correlations among 0/1 items. p-values not computed."
    
    # optional bootstrap to approximate significance (slow)
    if (bootstrap_p) {
      if (!is.null(seed)) set.seed(seed)
      n <- nrow(df); k <- ncol(df)
      # store bootstrapped r for each pair
      r_boot <- array(NA_real_, dim = c(k, k, B))
      for (b in seq_len(B)) {
        idx <- sample.int(n, n, replace = TRUE)
        rb <- try(psych::tetrachoric(df[idx, , drop = FALSE])$rho, silent = TRUE)
        if (!inherits(rb, "try-error"))
          r_boot[,,b] <- rb
      }
      # compute a p-like measure: 2*min(prop>0, prop<0)
      p_est <- matrix(NA_real_, k, k, dimnames = dimnames(r))
      for (i in 1:k) for (j in 1:k) if (i != j) {
        samp <- r_boot[i,j,]
        samp <- samp[is.finite(samp)]
        if (length(samp) > 10) {
          prop_pos <- mean(samp > 0, na.rm = TRUE)
          prop_neg <- mean(samp < 0, na.rm = TRUE)
          p_est[i,j] <- 2 * min(prop_pos, prop_neg)
        }
      }
      p <- p_est
      note <- paste0(
        "Note. Tetrachoric correlations among 0/1 items. p-values approximated via bootstrap (B=",
        B, ")."
      )
    }
  }
  
  # round & stars
  stars_mat <- matrix("", nrow = nrow(r), ncol = ncol(r), dimnames = dimnames(r))
  if (stars && !all(is.na(p))) {
    for (lab in names(star_cutoffs)) stars_mat[p < star_cutoffs[lab]] <- lab
  }
  
  fmt <- function(x) format(round(x, digits), nsmall = digits)
  formatted <- matrix(
    paste0(fmt(r), ifelse(is.na(p), "", stars_mat)),
    nrow = nrow(r), ncol = ncol(r),
    dimnames = list(var_names, var_names)
  )
  
  # blanking rules
  if (blank_upper) {
    formatted[upper.tri(formatted, diag = blank_diag)] <- ""
  }
  
  out_tbl <- as.data.frame(formatted, stringsAsFactors = FALSE)
  attr(out_tbl, "note") <- note
  attr(out_tbl, "type") <- type
  
  if (return == "table") {
    return(out_tbl)
  } else {
    return(list(
      table = out_tbl,
      r = r,
      p = p,
      note = note,
      type = type
    ))
  }
}




