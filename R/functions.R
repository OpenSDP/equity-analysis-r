# Packages
##Begin function
#' Gap Test
#' @description Explores dataset to find widest achievement gaps
#' @param df data, should be wide format, one row per student, and have column 
#' for grade level (class: data frame)
#' @param grade  name of tested grade column in dataset (class: character)
#' @param outcome name of outcome variable (usually test scores) in dataset 
#' (class: character)
#' @param features vector of features in dataset where testing for gaps 
#' (class: character)
#' @param n set 'n' largest gaps the function outputs at the end 
#' (class: integer, default: 5)
#' @param sds dataframe containing the standard deviations for all outcomes 
#' (class: data frame)
#' @param cut minimum number of students for level in a gap (class: integer, 
#' default = 50)
#' @param med indicator if would like function to compute standardized 
#' difference of medians instead of means (class: boolean, default: FALSE)
#' @param verbose (class:boolean, default: FALSE) indicator of whether the 
#' user would like additional details for each group comparison including 
#' sample sizes and alternative effect size calculations (Hedge's g and 
#' correlation coefficient translation of Cohen's d).
#' @author Dashiell Young-Saver
#' @author OpenSDP
#' 
#' 
#' @details Th
#' @references 
#'
#' @return Outputs a data.frame containing several different metrics for 
#' comparing the differences between the groups including Cohen's effect size, 
#' Hedges g, and the correlation coefficient. Where necessary, small sample 
#' size corrections are applied. 
#' 
#' @export
#'
#' @examples
gap.test <- function(df, grade, outcome, features, n = 5, sds = NULL, 
                     cut = 50, med = FALSE, verbose = FALSE, effect_size = "cohens_d") {
  # Check for user-provided standard deviations
  if (is.null(sds)) {
    # Notify user
    message("No standard deviations provided. 
            Will use standard deviations calculated from provided dataset.")
    # Find standard deviation for each grade
    sds.by.grades <- tapply(df[,outcome], df[,grade],sd)
    
    # Create standard deviation dataframe
    sds <- data.frame(grade_level = names(sds.by.grades),
                         out = sds.by.grades)
    colnames(sds)[1] <- grade
    colnames(sds)[2] <- outcome
    rownames(sds) <- NULL
    sds[,1] <- as.integer(as.character(sds[,1]))
    sds[,2] <- as.numeric(as.character(sds[,2]))
    
  }
  
  # Test for alignment between the sds and the
  if (!any(colnames(sds) == grade) | 
     !any(colnames(sds) == outcome) |
     dim(sds)[1] > 20 ) {
    
    #Make example sd dataframe
    ex.sds <- data.frame(grade_level = c(3,4,5,6,7,8),
                         math_ss = c(148.22,145.65,143.06,145.00,128.79,121.22),
                         rdg_ss = c(132.00,127.53,128.76,123.57,120.79,124.79),
                         wrtg_ss = c(NA,513.66,NA,NA,514.66,NA))
    
    message("sds object formatted incorrectly. Should be formatted 
            like the following example, as a dataframe, first column is grade level
            other columns are standard deviations for different test subjects,
            feature names match names of dataframe features and function
            inputs for 'grade' and 'outcome'.")
    print(ex.sds)
    
    stop("Either re-format the input to sds or use function without
            providing standard deviations")
    
  }
  
  # Test to see if data is correct class
  # TODO - try coercing sds grades into an integer if needed
  if (class(sds[,grade]) %nin% c('integer', 'numeric') | # this is too strict
    class(sds[,outcome]) != 'numeric') {
      #Make example dataframe of sds
      ex.sds <- data.frame(grade_level = c(3,4,5,6,7,8),
                           math_ss = c(148.22,145.65,143.06,145.00,128.79,121.22),
                           rdg_ss = c(132.00,127.53,128.76,123.57,120.79,124.79),
                           wrtg_ss = c(NA,513.66,NA,NA,514.66,NA))
      message("Error: Grade level in sds table should be class
            'integer' and standard deviation columns shuld be class 'numeric'.
              Like example below:")
      
      print(ex.sds)
      
      stop("Either re-format the input to sds or use function without
           providing standard deviations")
      
  }
  
  # Convert features to factors
  # TODO - check if these need to be factors?
  df[,features] <- lapply(df[,features, drop = FALSE], as.character)
  df[,features] <- lapply(df[,features, drop = FALSE], as.factor)
  
  # Get all grade levels for the chosen tested subject
  grades <- unique(df[!is.na(df[,outcome]),grade])
  
  #Will store all calculated achievement gaps and effect sizes
  
  output.table <- data.frame(level_1 = NULL,
                             lvl1_n = NULL,
                             level_2 = NULL,
                             lvl2_n = NULL, 
                             feature = NULL,
                             grade_level = NULL,
                             outcome = NULL,
                             raw_diffs = NULL, 
                             cohens_d = NULL,
                             hedgesg = NULL,
                             # corr_coef = NULL,
                             stringsAsFactors = FALSE)

  # Loop over grade levels
  for (gr in grades) {
    # Break down data by grade
    dat.grade <- df[df[,grade] == gr,]
    # Get standard deviation for scale scores at that grade level
    sd.gr <- sds[sds[,grade] == gr,outcome]
    # Loop over features
    for (feature in features) {
      # Get levels of feature
      lvl <- levels(dat.grade[,feature])
      
      lvl.table <- table(dat.grade[,feature])
      lvl <- names(lvl.table)
      # Get matrix of all combos of levels
      com.lvl <- combn(lvl, 2)
      
      # Loop over combinations
      for (i in 1:dim(com.lvl)[2]) {
        # Get levels you will compare gaps for
        level1 <- com.lvl[1, i]
        level2 <- com.lvl[2, i]
        
        #Measure gap (in terms of standardized median difference)
        level1.data <- dat.grade[dat.grade[,feature] == level1,outcome]
        level2.data <- dat.grade[dat.grade[,feature] == level2,outcome]
        # Gap here is based on medians
        if (med) {
          gap <- (median(level1.data) - median(level2.data))/sd.gr
        } else {
          gap <- (mean(level1.data) - mean(level2.data))/sd.gr
        }
        
        # Append gap to list and name in
        if (med) {
          raw_diff <- median(level1.data) - median(level2.data)
        } else {
          raw_diff <- mean(level1.data) - mean(level2.data)
        }
        
        # Measure the effect size using cohens_d and hedges_g
        cohens_d <- raw_diff / cohens_d_sd(level1.data, level2.data)
        hedges_g <- raw_diff / hedges_g_sd(level1.data, level2.data)
        
        if (length(level1.data) < 50 | length(level2.data) < 50) {
          message("Applying small sample-size correction to Hedges-g")
          hedges_g <- adjust_hedges_g(raw_diff, level1.data, level2.data)
        }
        
        # TODO: Tidy this bit up
        output.table.tmp <- data.frame(level_1 = level1,
                                       lvl1_n = length(level1.data),
                                   level_2 = level2,
                                   lvl2_n = length(level2.data), 
                                   feature = feature,
                                   grade_level = gr,
                                   outcome = outcome,
                                   mean_diff = raw_diff, 
                                   cohens_d = cohens_d,
                                   hedges_g = hedges_g,
                                   corr_coef = NA,
                                   stringsAsFactors = FALSE)
        output.table <- rbind(output.table, output.table.tmp)
      } #End loop over combinations
      
    } #End loop over features
    
  } #End loop over grade levels
  
  if (missing(n)) {
    n <- ifelse(nrow(output.table) < 10, 
                nrow(output.table), 10)
    message(paste0("No cutoff provided, setting it to ", n))
  } else {
    n <- ifelse(n > nrow(output.table), nrow(output.table), 
                n)
  }
  
  if (!is.null(cut)) {
    output.table$cohens_d[output.table$lvl1_n < cut | 
                               output.table$lvl2_n < cut] <- NA
    output.table$mean_diff[output.table$lvl1_n < cut | 
                               output.table$lvl2_n < cut] <- NA
    output.table$hedges_g[output.table$lvl1_n < cut | 
                               output.table$lvl2_n < cut] <- NA
    output.table$corr_coef[output.table$lvl1_n < cut | 
                           output.table$lvl2_n < cut] <- NA
  }
  
  output.table$level_1 <- as.character(output.table$level_1)
  output.table$level_2 <- as.character(output.table$level_2)
  output.table$feature <- as.character(output.table$feature)
  output.table$outcome <- as.character(output.table$outcome)
  output.table <- output.table[order(abs(output.table$cohens_d), 
                                     decreasing = TRUE), ]
  if (!verbose) {
    output.table$effect_size <- output.table[, effect_size]
    out_vars <- c("level_1", "level_2", "feature", "grade_level", 
                  "outcome", "effect_size", "mean_diff")
  } else {
    out_vars <- names(output.table)
  }
  rownames(output.table) <- NULL
  # Return table of effect sizes
  return(output.table[1:n, out_vars])
}

# Convenience function - x without y
"%nin%" <- function(x, y) !x %in% y #--  x without y

#' Cluster standard errors for lm objects
#'
#' @param model, an lm model object in R
#' @param cluster a vector the same length of the data fit to the model object 
#' that defines group membership
#'
#' @return an adjusted variance-covariance matrix accounting for clustering
#'
#' @examples
get_cluster_vcov <- function(model, cluster){
  # cluster is an actual vector of clusters from data passed to model
  # from: http://rforpublichealth.blogspot.com/2014/10/easy-clustered-standard-errors-in-r.html
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  cluster <- as.character(cluster)
  # calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1))*((N - 1)/(N - K))
  # calculate the uj's
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  # use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat = crossprod(uj)/N)
  return(vcovCL)
}


# Annotate a ggplot with proficiency levels

#' Annotate a ggplot with proficiency levels
#'
#' @param plot a ggplot2 object to be modified
#' @param prof_levels a dataframe formatted with proficiency labels and values, see 
#' example for details should include columns for grade and subject as well
#' @param direction character, either "horizontal" or "vertical", 
#' should proficiency level lines be drawn horizontally or vertically
#' @param grade 
#' @param subject 
#'
#' @return
#' @export
#'
#' @examples
add_ref_levels <- function(plot, prof_levels, direction = c("horizontal", "vertical"), 
                           grade, subject) {
  direction <- match.arg(direction)
  # TODO - improve this so it can place the annotations on the facets correctly
  # Get only the subjects and grades we need, drop the rest
  plot_levels <- prof_levels[prof_levels$grade == grade, ]
  plot_levels <- plot_levels[plot_levels$subject == subject, ]
  
  # Get range of the Y axis of the chart we are augmenting
  if(!is.null(ggplot_build(plot)$layout$panel_scales_y[[1]]$range_c)){
    y_range <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range_c$range
  } else {
    y_range <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
  }
  # get range of the X axis of the chart we are augmenting
  if(!is.null(ggplot_build(plot)$layout$panel_scales_x[[1]]$range_c)){
    x_range <- ggplot_build(plot)$layout$panel_scales_x[[1]]$range_c$range
  } else {
    x_range <- ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
  }
  # Set text size based on presence of faceting
  if(!is.null(ggplot_build(plot)$layout$facet$params$facets)) {
    text_size <- 1.5
  } else {
    text_size <- 4
  }
  
  nudge_y <- diff(y_range) / 10
  nudge_x <- diff(x_range) / 20
  
  # Decide on whether we are drawing horizontal or vertical reference lines
  if(direction == "horizontal"){
    plot <- plot + geom_hline(data = plot_levels, aes(yintercept = score), 
                              linetype = 2) + 
      geom_text(data = plot_levels, aes(y = score, # put the label up high
                                        # bump the label to the right of the line
                                        x = x_range[2], 
                                        label = prof_level), 
                size = text_size, nudge_y = nudge_y/2, nudge_x = nudge_x)  + # set the text size
      expand_limits(x = x_range[2] + diff(x_range)/5) # make the plot big enough
    
  } else if(direction == "vertical") { 
    plot <- plot + geom_vline(data = plot_levels, aes(xintercept = score), 
                              linetype = 2) + 
      geom_text(data = plot_levels, aes(x = score, y = y_range[2], 
                                        label = prof_level, fill = NULL), 
                size = text_size, nudge_x = nudge_x, nudge_y = nudge_y/2)  + 
      expand_limits(y = y_range[2] + diff(y_range)/5)
  }
  
  return(plot)
}




# Hedges g unadjusted
hedges_g_sd <- function(x, y) {
  x_n <- length(x)
  y_n <- length(y)
  sd_x <- sd(x)
  sd_y <- sd(y)
  
  numer <- ((x_n -1) * sd_x^2) + ((y_n - 1) * sd_y^2)
  denom <- x_n + y_n - 2
  out <- sqrt(numer/denom)
  out
}


# Use the maximum likelihood estimator of Cohen's D by Hedges and Olkin
cohens_d_sd <- function(x, y) {
  x_n <- length(x)
  y_n <- length(y)
  sd_x <- sd(x)
  sd_y <- sd(y)
  
  numer <- ((x_n -1) * sd_x^2) + ((y_n - 1) * sd_y^2)
  denom <- x_n + y_n 
  out <- sqrt(numer/denom)
  out
}

# Adjust hedges g for small sample sizes
adjust_hedges_g <- function(diff, x, y) {
  g <- diff / hedges_g_sd(x, y)
  g_star <- (1 - (3 / (4*(length(x) + length(y)) - 9))) * g
  return(g_star)
}


#' Plot the results of a gap test table
#'
#' @param df 
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
autoplot.gap_test <- function(df, what = "effect_size") {
  df$comp_name <- paste(df$level_1, df$level_2, sep = "-")
  df$effect_size <- df[, what]
  
  ylims <- c(min(df$effect_size), max(df$effect_size))
  if (ylims[1] > -0.1) {
    ylims[1] <- -0.1
  }
  if (ylims[2] < 0.1) {
    ylims[2] <- 0.1
  }
  
  df$grade_label <- paste0("Grade\n", df$grade_level)
  df$es_label <- round(df$effect_size, 2)
  df$grade_label_y <- df$effect_size / 2
  df$es_label_y <- df$effect_size + (sign(df$effect_size) * 0.01)
  
  
  
  ylims <- c(min(df$es_label_y), max(df$es_label_y))
  if (ylims[1] > -0.1) {
    ylims[1] <- -0.1
  }
  if (ylims[2] < 0.1) {
    ylims[2] <- 0.1
  }
  
  # TODO - fix the expression in the caption
  
  barp <- ggplot(df, aes(x = reorder(comp_name, abs(effect_size)),
                         group = grade_level,
                         y = effect_size)) +
    geom_hline(yintercept = 0.1, linetype = "solid", 
               color = "red", size = 1) +
    geom_hline(yintercept = -0.1, linetype = "solid", 
               color = "red", size = 1) +
    geom_hline(yintercept = 0, linetype = 2, color = "blue", 
               size = 1) + 
    geom_bar(position = "dodge", stat = "identity", color = "black", fill = "gray70") +
    geom_text(aes(y = grade_label_y, label = grade_label), position = position_dodge(width = 0.9)) +
    geom_label(aes(y = df$es_label_y, label = es_label), 
               position = position_dodge(width = 0.9)) +
    scale_x_discrete(name = "Comparison") +
    scale_y_continuous(name = "Effect Size", limits = ylims) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8)) +
    labs(title = paste0("Top ", nrow(df), " gap effect sizes"), 
         caption = expression("Calculation: d = " ~ frac(bar(Delta), sigma[pooled]) ~ "\n" ~
                                "Effect Size = " ~  frac(d, sqrt(d^2 + 4)))) + 
    facet_grid(~feature) + theme_bw()
  print(barp)
  
}
