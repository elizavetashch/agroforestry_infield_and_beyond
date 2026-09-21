make_gam_equation <- function(model,
                              response = "yield_rel",
                              digits = 4,
                              include_intercept = TRUE,
                              significant_only = FALSE) {
  
  # Accept either the model or summary(model)
  sm <- if (inherits(model, "summary.gam")) model else summary(model)
  
  # --------------------------------------------------
  # Parametric coefficients
  # --------------------------------------------------
  
  ptab <- sm$p.table
  
  if (is.null(ptab)) {
    stop("No parametric coefficients found in the model.")
  }
  
  terms <- rownames(ptab)
  
  # Remove intercept if requested
  keep <- if (include_intercept) {
    TRUE
  } else {
    terms != "(Intercept)"
  }
  
  ptab <- ptab[keep, , drop = FALSE]
  terms <- rownames(ptab)
  
  # Optionally retain only significant parametric terms
  if (significant_only) {
    ptab <- ptab[ptab[, "Pr(>|t|)"] < 0.05, , drop = FALSE]
    terms <- rownames(ptab)
  }
  
  # --------------------------------------------------
  # Format variable names
  # --------------------------------------------------
  
  format_term <- function(x) {
    
    replacements <- c(
      "treeage" = "\\mathrm{treeage}",
      "AFage"   = "\\mathrm{AFage}",
      "PC1_c"   = "\\mathrm{PC1}_{c}",
      "PC1_s"   = "\\mathrm{PC1}_{s}",
      "l_shdi"  = "\\mathrm{SHDI}",
      "l_ed"    = "\\mathrm{ED}"
    )
    
    if (x %in% names(replacements)) {
      replacements[[x]]
    } else {
      paste0("\\mathrm{", x, "}")
    }
  }
  
  # --------------------------------------------------
  # Build parametric part
  # --------------------------------------------------
  
  equation <- character(0)
  
  for (i in seq_len(nrow(ptab))) {
    
    term <- terms[i]
    estimate <- ptab[i, "Estimate"]
    
    # Intercept
    if (term == "(Intercept)") {
      equation <- c(
        equation,
        formatC(estimate, format = "f", digits = digits)
      )
      next
    }
    
    # Predictor
    term_latex <- format_term(term)
    
    # Sign
    sign <- ifelse(estimate >= 0, " + ", " - ")
    
    # Absolute coefficient
    coef <- formatC(
      abs(estimate),
      format = "f",
      digits = digits
    )
    
    equation <- c(
      equation,
      paste0(
        sign,
        coef,
        "\\,(",
        term_latex,
        ")"
      )
    )
  }
  
  # --------------------------------------------------
  # Smooth terms
  # --------------------------------------------------
  
  stable_names <- rownames(sm$s.table)
  
  if (!is.null(stable_names)) {
    
    smooth_names <- stable_names
    
    # Remove random-effect smooths if desired
    smooth_names <- smooth_names[
      !grepl("^s\\(year\\)", smooth_names)
    ]
    
    if (significant_only) {
      pvals <- sm$s.table[stable_names, "p-value"]
      smooth_names <- smooth_names[
        pvals[match(smooth_names, stable_names)] < 0.05
      ]
    }
    
    if (length(smooth_names) > 0) {
      
      smooth_terms <- paste0(
        "f_{",
        seq_along(smooth_names),
        "}(",
        smooth_names,
        ")"
      )
      
      equation <- c(
        equation,
        paste0(" + ", smooth_terms)
      )
    }
  }
  
  # --------------------------------------------------
  # Clean up smooth-term names
  # --------------------------------------------------
  
  equation <- paste(equation, collapse = "")
  
  equation <- gsub(
    "s\\(swf_mat\\.tmat\\):L\\.swf_mat",
    "\\mathrm{SWF}",
    equation
  )
  
  equation <- gsub(
    "s\\(distance_to_tree_strip\\)",
    "\\mathrm{distance}",
    equation
  )
  
  equation <- gsub(
    "ti\\(distance_to_tree_strip,swf_slope_0to200\\)",
    "\\mathrm{distance,SWF}_{0-200}",
    equation
  )
  
  equation <- gsub(
    "ti\\(distance_to_tree_strip,swf_slope_200to500\\)",
    "\\mathrm{distance,SWF}_{200-500}",
    equation
  )
  
  equation <- gsub(
    "ti\\(distance_to_tree_strip,swf_slope_500to1000\\)",
    "\\mathrm{distance,SWF}_{500-1000}",
    equation
  )
  
  equation <- gsub(
    "ti\\(distance_to_tree_strip,treeage\\)",
    "\\mathrm{distance,treeage}",
    equation
  )
  
  # Replace f_i(function-name) with readable notation
  equation <- gsub(
    "f_([0-9]+)\\(\\s*([^\\)]+)\\s*\\)",
    "f_{\\1}(\\2)",
    equation
  )
  
  # --------------------------------------------------
  # Final LaTeX equation
  # --------------------------------------------------
  
  paste0(
    "$$\\text{",
    response,
    "} = ",
    equation,
    "$$"
  )
}

