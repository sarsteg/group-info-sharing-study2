# Libraries --------------------------------------------------------------------

library(psych)
library(dplyr)
library(tidyr)
library(haven)
library(descr)
library(ggplot2)
library(readr)
library(openxlsx)
library(janitor)
library(writexl)
library(openxlsx2)


# Flags ------------------------------------------------------------------------

run_model <- TRUE
load_artifacts <- !run_model
# run_model determines if the code will run again and save as artifact
# if false, then the code will load the artifact and skip the model run

# | eval: !expr run_model
# | eval: !expr load_artifacts

# Functions --------------------------------------------------------------------




#...............................................................................

save_output_to_workbook <- function(
    sheet_name,
    output,
    wb = get("wb", envir = .GlobalEnv)
) {
  
  # Remove sheet if it already exists
  if (sheet_name %in% openxlsx2::wb_get_sheet_names(wb)) {
    wb <- openxlsx2::wb_remove_worksheet(
      wb,
      sheet = sheet_name
    )
  }
  
  # Create worksheet
  wb <- openxlsx2::wb_add_worksheet(
    wb,
    sheet = sheet_name
  )
  
  # Try to convert output to a data frame
  output_df <- tryCatch(
    as.data.frame(output),
    error = function(e) NULL
  )
  
  # If conversion worked, save as a regular table
  if (!is.null(output_df)) {
    
    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = sheet_name,
      x = output_df,
      row_names = TRUE
    )
    
  } else {
    
    # If conversion failed, capture the printed output
    output_text <- capture.output(
      print(output)
    )
    
    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = sheet_name,
      x = data.frame(Output = output_text),
      row_names = FALSE,
      col_names = FALSE
    )
  }
  
  wb
}

# Use
# wb <- save_output_to_workbook(
#   "KMO",
#   kmo_strategy, 
#   wb
# )



#...............................................................................

save_figure_to_workbook <- function(
    wb,
    sheet_name,
    figure_file,
    plot_obj = NULL, # ggplot object
    plot_fun = NULL, # base R plotting function
    width = 8,
    height = 6,
    dpi = 300
) {
  
  # Remove sheet if it already exists
  if (sheet_name %in% openxlsx2::wb_get_sheet_names(wb)) {
    
    wb <- openxlsx2::wb_remove_worksheet(
      wb,
      sheet = sheet_name
    )
  }
  
  # Create worksheet
  wb <- openxlsx2::wb_add_worksheet(
    wb,
    sheet = sheet_name
  )
  
  # Save ggplot figure
  if (!is.null(plot_obj) && inherits(plot_obj, "ggplot")) {
    
    ggplot2::ggsave(
      filename = figure_file,
      plot = plot_obj,
      width = width,
      height = height,
      dpi = dpi
    )
    
  } else if (!is.null(plot_fun) && is.function(plot_fun)) {
    
    png(
      filename = figure_file,
      width = width,
      height = height,
      units = "in",
      res = dpi
    )
    
    plot_fun()
    
    dev.off()
    
  } else {
    
    stop(
      "Provide either a ggplot object in plot_obj or a base R plotting function in plot_fun."
    )
  }
  
  # Insert figure into workbook
  wb <- openxlsx2::wb_add_image(
    wb,
    sheet = sheet_name,
    dims = "A1",
    file = figure_file,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )
  
  return(wb)
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

evaluate_efa_solution <- function(model,
                                  solution_name,
                                  salient_cutoff = .30,
                                  residual_cutoff = .10) {
  
  # Pattern coefficients
  
  pattern <- as.matrix(unclass(model$loadings))
  abs_pattern <- abs(pattern)
  
  # Number of salient loadings for each item
  
  n_salient_item <- rowSums(
    abs_pattern >= salient_cutoff
  )
  
  # Total salient indicators per factor
  
  n_salient_factor <- colSums(
    abs_pattern >= salient_cutoff
  )
  
  # Items with no salient loading
  
  no_salient_items <- rownames(pattern)[
    n_salient_item == 0
  ]
  
  # Items loading saliently on more than one factor
  
  cross_loading_items <- rownames(pattern)[
    n_salient_item > 1
  ]
  
  # Factors with fewer than 3 salient indicators
  
  weak_factors <- names(
    n_salient_factor[
      n_salient_factor < 3
    ]
  )
  
  # Unique salient indicators
  
  unique_salient <- matrix(
    FALSE,
    nrow = nrow(pattern),
    ncol = ncol(pattern),
    dimnames = dimnames(pattern)
  )
  
  for (j in seq_len(ncol(pattern))) {
    
    unique_salient[, j] <-
      abs_pattern[, j] >= salient_cutoff &
      n_salient_item == 1
  }
  
  n_unique_salient_factor <- colSums(
    unique_salient
  )
  
  # Factors with fewer than 3 uniquely salient indicators
  
  weak_factors_unique <- names(
    n_unique_salient_factor[
      n_unique_salient_factor < 3
    ]
  )
  
  # Communalities
  
  communalities <- model$communality
  
  # Interfactor correlations
  
  if (!is.null(model$Phi) && ncol(model$Phi) > 1) {
    
    phi_values <- abs(
      model$Phi[lower.tri(model$Phi)]
    )
    
    max_factor_correlation <- max(
      phi_values,
      na.rm = TRUE
    )
    
  } else {
    
    max_factor_correlation <- NA_real_
  }
  
  # Residual correlations
  
  residual_matrix <- model$residual
  
  residual_values <- abs(
    residual_matrix[
      lower.tri(residual_matrix)
    ]
  )
  
  n_large_residuals <- sum(
    residual_values >= residual_cutoff,
    na.rm = TRUE
  )
  
  prop_large_residuals <- mean(
    residual_values >= residual_cutoff,
    na.rm = TRUE
  )
  
  # Variance explained
  
  variance_explained <- sum(
    model$Vaccounted["Proportion Var", ],
    na.rm = TRUE
  )
  
  # Solution-level summary
  
  summary_table <- tibble::tibble(
    
    Solution = solution_name,
    
    Factors = ncol(pattern),
    
    `Factors with < 3 Salient Items` =
      sum(n_salient_factor < 3),
    
    `Factors with < 3 Unique Salient Items` =
      sum(n_unique_salient_factor < 3),
    
    `Items with No Salient Loading` =
      length(no_salient_items),
    
    `Cross-Loading Items` =
      length(cross_loading_items),
    
    `Minimum Communality` =
      min(communalities, na.rm = TRUE),
    
    `Mean Communality` =
      mean(communalities, na.rm = TRUE),
    
    `Maximum |Factor Correlation|` =
      max_factor_correlation,
    
    `Residuals >= |.10|` =
      n_large_residuals,
    
    `Proportion Residuals >= |.10|` =
      prop_large_residuals,
    
    `Variance Explained` =
      variance_explained,
    
    RMSR = model$rms,
    
    TLI = model$TLI,
    
    RMSEA = model$RMSEA[1],
    
    BIC = model$BIC
  )
  
  # Return everything
  
  list(
    
    summary = summary_table,
    
    pattern = pattern,
    
    structure = model$Structure,
    
    factor_correlations = model$Phi,
    
    communalities = communalities,
    
    residuals = residual_matrix,
    
    salient_per_factor = n_salient_factor,
    
    unique_salient_per_factor =
      n_unique_salient_factor,
    
    no_salient_items =
      no_salient_items,
    
    cross_loading_items =
      cross_loading_items,
    
    weak_factors =
      weak_factors,
    
    weak_factors_unique =
      weak_factors_unique
  )
}





#...............................................................................

efa_item_diagnostics <- function(model,
                                 solution_name,
                                 salient_cutoff = .30,
                                 item_labels = NULL,
                                 suppress_below_cutoff = TRUE) {
  
  pattern <- as.matrix(unclass(model$loadings))
  structure <- as.matrix(unclass(model$Structure))
  
  abs_pattern <- abs(pattern)
  
  primary_index <- apply(
    abs_pattern,
    1,
    which.max
  )
  
  primary_factor <- colnames(pattern)[primary_index]
  
  primary_loading <- vapply(
    seq_len(nrow(pattern)),
    function(i) {
      pattern[i, primary_index[i]]
    },
    numeric(1)
  )
  
  second_loading <- apply(
    abs_pattern,
    1,
    function(x) {
      sort(x, decreasing = TRUE)[2]
    }
  )
  
  n_salient <- rowSums(
    abs_pattern >= salient_cutoff
  )
  
  # Preserve complete pattern coefficients for diagnostics
  pattern_display <- pattern
  
  # Optionally suppress coefficients below the salient cutoff
  if (suppress_below_cutoff) {
    pattern_display[
      abs(pattern_display) < salient_cutoff
    ] <- NA_real_
  }
  
  pattern_df <- as.data.frame(
    pattern_display,
    check.names = FALSE
  )
  
  names(pattern_df) <- paste0(
    "Pattern_",
    names(pattern_df)
  )
  
  structure_df <- as.data.frame(
    structure,
    check.names = FALSE
  )
  
  names(structure_df) <- paste0(
    "Structure_",
    names(structure_df)
  )
  
  item_ids <- rownames(pattern)
  
  if (!is.null(item_labels)) {
    item_text <- unname(item_labels[item_ids])
  } else {
    item_text <- item_ids
  }
  
  diagnostics_df <- tibble::tibble(
    Solution = solution_name,
    Item_ID = item_ids,
    Item = item_text,
    Primary_Factor = primary_factor,
    Primary_Loading = primary_loading,
    Second_Loading = second_loading,
    Loading_Difference =
      abs(primary_loading) - second_loading,
    Communality =
      as.numeric(model$communality),
    N_Salient_Loadings =
      n_salient,
    Cross_Loading =
      n_salient > 1,
    No_Salient_Loading =
      n_salient == 0
  )
  
  dplyr::bind_cols(
    diagnostics_df,
    pattern_df,
    structure_df
  )
}




#...............................................................................


extract_alpha_results <- function(alpha_object,
                                  solution,
                                  factor_name) {
  
  overall <- tibble::tibble(
    Solution = solution,
    Factor = factor_name,
    N_Items = nrow(alpha_object$item.stats),
    Raw_Alpha = alpha_object$total$raw_alpha,
    Standardized_Alpha = alpha_object$total$std.alpha,
    Average_Interitem_Correlation = alpha_object$total$average_r
  )
  
  item_stats <- alpha_object$item.stats %>%
    tibble::rownames_to_column("Item") %>%
    dplyr::select(
      Item,
      raw.r,
      std.r,
      r.cor,
      r.drop,
      mean,
      sd
    )
  
  alpha_drop <- alpha_object$alpha.drop %>%
    tibble::rownames_to_column("Item") %>%
    dplyr::select(
      Item,
      Alpha_if_Deleted = raw_alpha
    )
  
  item_results <- item_stats %>%
    dplyr::left_join(
      alpha_drop,
      by = "Item"
    ) %>%
    dplyr::mutate(
      Solution = solution,
      Factor = factor_name,
      .before = 1
    )
  
  list(
    overall = overall,
    items = item_results
  )
}



#...............................................................................

get_factor_reliability <- function(data,
                                   factor_items) {
  
  overall_results <- list()
  item_results <- list()
  
  for (factor_name in names(factor_items)) {
    
    items <- factor_items[[factor_name]]
    
    alpha_result <- psych::alpha(
      data %>%
        dplyr::select(
          dplyr::all_of(items)
        ),
      warnings = FALSE,
      check.keys = FALSE
    )
    
    # Overall reliability for the factor
    overall_results[[factor_name]] <- tibble::tibble(
      Factor = factor_name,
      N_Items = length(items),
      Alpha = alpha_result$total$raw_alpha,
      Standardized_Alpha = alpha_result$total$std.alpha,
      Average_Interitem_Correlation = alpha_result$total$average_r
    )
    
    # Item-level reliability
    item_stats <- alpha_result$item.stats %>%
      tibble::rownames_to_column("Reliability_Item") %>%
      dplyr::select(
        Reliability_Item,
        Corrected_Item_Total = r.drop
      )
    
    if (length(items) >= 3) {
      
      alpha_drop <- alpha_result$alpha.drop %>%
        tibble::rownames_to_column("Reliability_Item") %>%
        dplyr::select(
          Reliability_Item,
          Alpha_if_Deleted = raw_alpha
        )
      
    } else {
      
      alpha_drop <- tibble::tibble(
        Reliability_Item = items,
        Alpha_if_Deleted = NA_real_
      )
    }
    
    item_results[[factor_name]] <- dplyr::left_join(
      item_stats,
      alpha_drop,
      by = "Reliability_Item"
    ) %>%
      dplyr::mutate(
        Factor = factor_name,
        .before = 1
      )
  }
  
  list(
    overall = dplyr::bind_rows(overall_results),
    
    items = dplyr::bind_rows(item_results) %>%
      dplyr::mutate(
        Item_ID = dplyr::if_else(
          Reliability_Item == "q9_1_4_r",
          "q9_1_4",
          Reliability_Item
        )
      ) %>%
      dplyr::select(
        Item_ID,
        Factor,
        Corrected_Item_Total,
        Alpha_if_Deleted
      )
  )
}


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






# Formatting for p values included
format_stat <- function(x, p_value = FALSE) {
  
  
  if (p_value) {
    
    rounded_2 <- round_half_up(x, 2)
    rounded_3 <- round_half_up(x, 3)
    
    p_text <- ifelse(
      is.na(x), "",
      ifelse(
        x < .001,
        "< .001",
        ifelse(
          rounded_2 == 0,
          sub("^0", "", sprintf("%.3f", rounded_3)),
          sub("^0", "", sprintf("%.2f", rounded_2))
        )
      )
    )
    
    stars <- ifelse(
      is.na(x), "",
      ifelse(
        x < .01, "**",
        ifelse(x < .05, "*", "")
      )
    )
    
    return(paste0(p_text, stars))
  }
  
  # Other statistics: 2-decimal half-up rounding
  rounded_2 <- round_half_up(x, 2)
  
  ifelse(
    is.na(x),
    "",
    sprintf("%.2f", rounded_2)
  )
}


# End --------------------------------------------------------------------------
print("common loaded")