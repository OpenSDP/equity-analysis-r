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
#' @param comp indicator to output additional comparative gap graphics 
#' (class: boolean, default: FALSE)
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
                     comp = FALSE, cut = 50, med = FALSE, verbose = FALSE) {
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
  if(class(sds[,grade]) %nin% c('integer', 'numeric') | # this is too strict
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
      
  } #End of conditional
  
  # Convert features to factors
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
                             effect_size = NULL,
                             raw_diffs = NULL, 
                             hedgesg = NULL,
                             corr_coef = NULL,
                             stringsAsFactors = FALSE)

  #Loop over grade levels
  for(gr in grades){
    
    #Break down data by grade
    dat.grade <- df[df[,grade]==gr,]
    
    #Get standard deviation for scale scores at that grade level
    sd.gr <- sds[sds[,grade]==gr,outcome]
    
    #Loop over features
    for(feature in features){
      
      #Get levels of feature
      lvl <- levels(dat.grade[,feature])
      
      #Cut levels if cut point given
      # if(!is.null(cut)){
      #   
      #   #Table of factor values
      #   lvl.table <- table(dat.grade[,feature])
      #   
      #   #Initialize vector of levels to cut
      #   lvl <- names(lvl.table[lvl.table >= cut])
      #   
      #   #See if any levels left
      #   if(length(lvl) < 2){
      #     # Change to warning
      #     # Change language
      #     warning(paste("Cut point too high: no data left to compare for",
      #                feature,gr,outcome))
      #     
      #   }#End inner conditional
      #   
      # }#End outer conditional
      
      lvl <- levels(dat.grade[,feature])
      lvl.table <- table(dat.grade[,feature])
      lvl <- names(lvl.table)
      
      #Get matrix of all combos of levels
      com.lvl <- combn(lvl, 2)
      
      #Loop over combinations
      for(i in 1:dim(com.lvl)[2]){
        
        #Get levels you will compare gaps for
        level1 <- com.lvl[1,i]
        level2 <- com.lvl[2,i]
        
        #Measure gap (in terms of standardized median difference)
        level1.data <- dat.grade[dat.grade[,feature]==level1,outcome]
        level2.data <- dat.grade[dat.grade[,feature]==level2,outcome]
        # Gap here is based on medians
        if(med) {
          gap <- (median(level1.data) - median(level2.data))/sd.gr
        } else {
          gap <- (mean(level1.data) - mean(level2.data))/sd.gr
        }
        #Append gap to list and name in
        
        if(med) {
          raw_diff <- median(level1.data) - median(level2.data)
          # raw_diffs <- append(raw_diffs, raw_diff, length(raw_diffs))
          # sd.gr is either user-specified or calculated from observed data
          d <- raw_diff / sd.gr
        } else {
          raw_diff <- mean(level1.data) - mean(level2.data)
          # raw_diffs <- append(raw_diffs, raw_diff, length(raw_diffs))
          d <- raw_diff / sd.gr
        }
        
        
        # Correlation coefficient
        r <- d/sqrt(d^2+4)
        # hedges g
        # g <- d / sqrt(length(c(level1.data, level2.data)) / df)
        
        # TODO: Tidy this bit up
        output.table.tmp <- data.frame(level_1 = level1,
                                       lvl1_n = length(level1.data),
                                   level_2 = level2,
                                   lvl2_n = length(level2.data), 
                                   feature = feature,
                                   grade_level = gr,
                                   outcome = outcome,
                                   effect_size = d,
                                   raw_diffs = raw_diff, 
                                   hedgesg = NA,
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
  
  if(!is.null(cut)) {
    output.table$effect_size[output.table$lvl1_n < cut | 
                               output.table$lvl2_n < cut] <- NA
    output.table$raw_diffs[output.table$lvl1_n < cut | 
                               output.table$lvl2_n < cut] <- NA
    output.table$hedgesg[output.table$lvl1_n < cut | 
                               output.table$lvl2_n < cut] <- NA
    output.table$corr_coef[output.table$lvl1_n < cut | 
                           output.table$lvl2_n < cut] <- NA
  }
  
  output.table$level_1 <- as.character(output.table$level_1)
  output.table$level_2 <- as.character(output.table$level_2)
  output.table$feature <- as.character(output.table$feature)
  output.table$outcome <- as.character(output.table$outcome)
  output.table <- output.table[order(abs(output.table$effect_size), 
                                     decreasing = TRUE), ]
  if(!verbose) {
    out_vars <- c("level_1", "level_2", "feature", "grade_level", 
                  "outcome", "effect_size", "raw_diffs")
  } else {
    out_vars <- names(output.table)
  }
  
  
  rownames(output.table) <- NULL
  #Return table of effect sizes
  return(output.table[1:n, out_vars])
}

#End function
"%nin%" <- function(x, y) !x %in% y #--  x without y

gap.test2 <- function(df, grade, outcome, features, n = 3, sds = NULL, 
                     comp = FALSE, cut = 50, med = FALSE, outlbl = NULL) {
  
  #See if no standard deviations provided
  if(is.null(sds)){
    
    
    #Notify user
    message("No standard deviations provided. \n Will use standard deviations calculated from provided dataset.")
    
    #Find standard deviation for each grade
    sds.by.grades <- tapply(df[,outcome], df[,grade],sd)
    
    #Create standard deviation dataframe
    sds <- data.frame(grade_level = names(sds.by.grades),
                      out = sds.by.grades)
    colnames(sds)[1] <- grade
    colnames(sds)[2] <- outcome
    rownames(sds) <- NULL
    sds[,1] <- as.integer(as.character(sds[,1]))
    sds[,2] <- as.numeric(as.character(sds[,2]))
    
  } #End of conditional
  
  #Test to make sure feature names and dimensions are correct
  if(!any(colnames(sds)==grade) | 
     !any(colnames(sds)==outcome) |
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
    
  }#End of conditional
  
  #Test to see if data is correct class
  if(class(sds[,grade]) != 'integer' | 
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
    
  }#End of conditional
  
  #Convert features to factors
  df[,features] <- lapply(df[,features, drop = FALSE], as.character)
  df[,features] <- lapply(df[,features, drop = FALSE], as.factor)
  
  # Get all grade levels for the chosen tested subject
  grades <- unique(df[!is.na(df[,outcome]),grade])
  
  #Will store all calculated achievement gaps and effect sizes
  gaps <- vector()
  effects <- vector()
  effects.level1 <- vector()
  effects.level2 <- vector()
  effects.f <- vector()
  effects.gr <- vector()
  effects.outcome <- vector()
  mean_diffs <- vector()
  
  #Stores outcome label, based on user input
  if(is.null(outlbl)){
    
    #Store label as column name
    outlbl <- outcome
  } else if(class(outlbl)!="character"){
    
    stop("outlbl must be of type 'character'")
  }
  
  #Loop over grade levels
  for(gr in grades){
    
    #Break down data by grade
    dat.grade <- df[df[,grade]==gr,]
    
    #Get standard deviation for scale scores at that grade level
    sd.gr <- sds[sds[,grade]==gr,outcome]
    
    #Loop over features
    for(feature in features){
      
      #Get levels of feature
      lvl <- levels(dat.grade[,feature])
      
      #Cut levels if cut point given
      if(!is.null(cut)){
        
        #Table of factor values
        lvl.table <- table(dat.grade[,feature])
        
        #Initialize vector of levels to cut
        lvl <- names(lvl.table[lvl.table >= cut])
        
        #See if any levels left
        if(length(lvl) < 2){
          # Error
          stop(paste("Cut point too high: no data left to compare for",
                     feature,gr,outcome))
          
        }#End inner conditional
        
      }#End outer conditional
      
      #Get matrix of all combos of levels
      com.lvl <- combn(lvl,2)
      
      #Loop over combinations
      for(i in 1:dim(com.lvl)[2]){
        
        #Get levels you will compare gaps for
        level1 <- com.lvl[1,i]
        level2 <- com.lvl[2,i]
        
        #Measure gap (in terms of standardized median difference)
        level1.data <- dat.grade[dat.grade[,feature]==level1,outcome]
        level2.data <- dat.grade[dat.grade[,feature]==level2,outcome]
        gap <- (median(level1.data) - median(level2.data))/sd.gr
        
        #Append gap to list and name in
        gaps <- append(gaps,gap,length(gaps))
        names(gaps)[length(gaps)] <- paste(level1,"-",level2,", ",
                                           feature,'\n',"Grade ",gr,
                                           ", ",outcome, sep="")
        
        #Append mean difference to list
        mean_diff <- mean(level1.data) - mean(level2.data)
        mean_diffs <- append(mean_diffs, mean_diff, length(mean_diffs))
        
        #Measure the effect size (r)
        var.1 <- var(level1.data)
        var.2 <- var(level2.data)
        sd.pooled <- sqrt((var.1+var.2)/2)
        d <- (mean(level1.data) - mean(level2.data))/sd.pooled
        r <- d/sqrt(d^2+4)
        
        #Append effect size and information to lists and name
        effects <- append(effects,r,length(effects))
        names(effects)[length(effects)] <- paste(level1,"-",level2,", ",
                                                 feature,'\n',"Grade ",gr,
                                                 ", ",outlbl, sep="")
        effects.level1 <- append(effects.level1, level1, length(effects.level1))
        effects.level2 <- append(effects.level2, level2, length(effects.level2))
        effects.f <- append(effects.f, feature, length(effects.f))
        effects.gr <- append(effects.gr, gr, length(effects.gr))
        effects.outcome <- append(effects.outcome, outcome, length(effects.outcome))
        
        
      } #End loop over combinations
      
    } #End loop over features
    
  } #End loop over grade levels
  
  #Sort gaps and effect sizes largest to smallest in magnitude
  sorted.gaps <- gaps[order(abs(gaps), decreasing=TRUE)]
  effects.sorted <- effects[order(abs(effects), decreasing = TRUE)]
  effects.level1 <- effects.level1[order(abs(effects), decreasing = TRUE)]
  effects.level2 <- effects.level2[order(abs(effects), decreasing = TRUE)]
  effects.f <- effects.f[order(abs(effects), decreasing = TRUE)]
  effects.gr <- effects.gr[order(abs(effects), decreasing = TRUE)]
  effects.outcome <- effects.outcome[order(abs(effects), decreasing = TRUE)]
  mean_diffs <- mean_diffs[order(abs(effects), decreasing = TRUE)]
  
  
  
  ##If want to show gaps by feature, will make and show plots
  if(comp){
    
    #Initialize
    effects.feature <-vector()
    i <- 1
    
    #Loop over features
    for(feature in features){
      
      #Loop over each effect size
      for(i in 1:length(effects.sorted)){
        
        #Draw out effect sizes for current loop feature
        if(regexpr(feature, names(effects.sorted[i]))[1] != -1){
          
          #Store in gaps.feature
          effects.feature <- append(effects.feature,effects.sorted[i],length(effects.feature))
          
        } #End conditional
        
      }#End loop over effect sizes
      
      #Keep highest 20 effects (if applicable)
      if(length(effects.feature) > 20){
        effects.feature <- effects.feature[1:20]
      }
      
      #Turn into dataframe
      dat.effects <- data.frame(effects.feature)
      dat.effects$names <- rownames(dat.effects)
      rownames(dat.effects) <- NULL
      
      #Determine y-axis limits
      if(min(dat.effects$effects.feature) < -0.11){
        limit1 <- min(dat.effects$effects.feature) - 0.03
      }
      else{
        limit1 <- -.11
      }
      if(max(dat.effects$effects.feature) > 0.11){
        limit2 <- max(dat.effects$effects.feature) + 0.03
      }
      else{
        limit2 <- .11
      }
      
      #Barplot for feature
      barp <- ggplot(dat.effects, aes(x= reorder(names, abs(effects.feature)), y=effects.feature)) +
        geom_bar(position="dodge",stat="identity")+
        scale_x_discrete(name = "Comparison")+
        scale_y_continuous(name = "Effect Size", limits = c(limit1,limit2))+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))+
        geom_hline(yintercept=0.1, linetype="solid", 
                   color = "red", size=1) +
        geom_hline(yintercept=-0.1, linetype="solid", 
                   color = "red", size=1) +
        geom_hline(yintercept = 0, linetype = 2, color = "blue", 
                   size = 1) + 
        ggtitle(feature)
      
      print(barp)
      
      #Empty vector
      effects.feature <-vector()
      
    }#End loop over features
    
  }#End conditional
  
  #Prints n largest gaps and meaning
  #Standardized difference of medians
  #Will only do this if user asks
  if(med){
    # TODO: Where does N come from
    #Make into dataframe
    plot.df.med <- data.frame(names <- names(sorted.gaps[1:n]),
                              med.s.gaps <- sorted.gaps[1:n])
    rownames(plot.df.med) <- NULL
    
    #Determine y-axis limits
    if(min(plot.df.med$med.s.gaps) < -0.11){
      limit1 <- min(plot.df.med$med.s.gaps) - 0.03
    }
    else{
      limit1 <- -.11
    }
    if(max(plot.df.med$med.s.gaps) > 0.11){
      limit2 <- max(plot.df.med$med.s.gaps) + 0.03
    }
    else{
      limit2 <- .11
    }
    
    #Barplot for standardized difference of medians
    barp <- ggplot(plot.df.med, aes(x= reorder(names, abs(med.s.gaps)), y=med.s.gaps)) +
      geom_bar(position="dodge",stat="identity")+
      scale_x_discrete(name = "")+
      scale_y_continuous(name = "Standardized median difference", limits = c(limit1,limit2))+
      theme(axis.text=element_text(vjust=0.5, size=12),
            axis.title=element_text(size = 14))+
      geom_text(aes(y = med.s.gaps + .02*sign(med.s.gaps), label=round(med.s.gaps, 3)), 
                size=4.5)+
      labs(title = paste0("Top ", n, " standardized difference of medians"),
           caption = expression("Calculation: " ~ frac(Delta ~ scriptstyle(medians), sigma))) + theme_bw()
    
    
    print(barp)
    
  }#End conditional
  
  ##Visualize top 'n' effect sizes
  #Make into dataframe
  plot.df.effect <- data.frame(names <- names(effects.sorted[1:n]),
                               med.s.eff <- effects.sorted[1:n])
  rownames(plot.df.effect) <- NULL
  
  
  #Determine y-axis limits
  if(min(plot.df.effect$med.s.eff) < -0.11){
    limit1 = min(plot.df.effect$med.s.eff) - 0.03
  } else {
    limit1 = -.11
  } 
  
  if(max(plot.df.effect$med.s.eff) > 0.11){
    limit2 = max(plot.df.effect$med.s.eff) + 0.03
  } else {
    limit2 = .11
  }
  
  #Barplot for effect sizes
  barp <- ggplot(plot.df.effect, aes(x= reorder(names, abs(med.s.eff)), y=med.s.eff)) +
    geom_bar(position="dodge",stat="identity")+
    scale_x_discrete(name = "")+
    scale_y_continuous(name = "Effect Size", limits = c(limit1,limit2))+
    theme(axis.text=element_text(vjust=0.5, size=12),
          axis.title=element_text(size=14))+
    geom_hline(yintercept=0.1, linetype="solid", 
               color = "red", size=1)+
    geom_hline(yintercept=-0.1, linetype="solid", 
               color = "red", size=1)+
    geom_hline(yintercept = 0, linetype = 2, color = "blue", 
               size = 1) + 
    geom_text(aes(y = med.s.eff+.01*sign(med.s.eff), label=round(med.s.eff, 3)), 
              size=4.5)+
    labs(title = paste0("Top ", n, " effect sizes"), 
         caption = expression("Calculation: d = " ~ frac(bar(Delta), sigma[pooled]) ~ "\n" ~
                                "Effect Size = " ~  frac(d, sqrt(d^2 + 4)))) + 
    theme_bw()
  
  print(barp)
  
  #Output a table of groups with largest effect sizes
  output.table <- data.frame(level_1 = effects.level1[1:n],
                             level_2 = effects.level2[1:n],
                             feature = effects.f[1:n],
                             grade_level = effects.gr[1:n],
                             outcome = effects.outcome[1:n],
                             effect_size = effects.sorted[1:n],
                             mean_diff = mean_diffs[1:n])
  
  output.table$level_1 <- as.character(output.table$level_1)
  output.table$level_2 <- as.character(output.table$level_2)
  output.table$feature <- as.character(output.table$feature)
  output.table$outcome <- as.character(output.table$outcome)
  
  rownames(output.table) <- NULL
  
  #Return table of effect sizes
  return(output.table)
  
}



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
  
  # Decide on whether we are drawing horizontal or vertical reference lines
  if(direction == "horizontal"){
    plot <- plot + geom_hline(data = plot_levels, aes(yintercept = score), 
                              linetype = 2) + 
      geom_text(data = plot_levels, aes(y = score + 40, # put the label up high
                                        # bump the label to the right of the line
                                        x = x_range[2] + diff(x_range)/10, 
                                        label = prof_level), 
                size = text_size)  + # set the text size
      expand_limits(x = x_range[2] + diff(x_range)/5) # make the plot big enough
    
  } else if(direction == "vertical") { 
    plot <- plot + geom_vline(data = plot_levels, aes(xintercept = score), 
                              linetype = 2) + 
      geom_text(data = plot_levels, aes(x = score + 80, y = y_range[2] + diff(y_range)/10, 
                                        label = prof_level, fill = NULL), 
                size = text_size)  + 
      expand_limits(y = y_range[2] + diff(y_range)/5)
  }
  
  return(plot)
}



#' Plot the results of a gap test table
#'
#' @param df 
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
autoplot.gap_test <- function(df) {
  df$comp_name <- paste(df$level_1, df$level_2, sep = "-")
  
  ylims <- c(min(df$effect_size), max(df$effect_size))
  if (ylims[1] > -0.1) {
    ylims[1] <- -0.1
  }
  if (ylims[2] < 0.1) {
    ylims[2] <- 0.1
  }
  
  # names(gaps)[length(gaps)] <- paste(level1,"-",level2,", ",
  #                                    feature,'\n',"Grade ",gr,
  #                                    ", ",outcome, sep="")
  # 
  barp <- ggplot(df, aes(x= reorder(comp_name, abs(effect_size)), 
                         y = effect_size), group = grade_level) +
    geom_bar(position="dodge",stat="identity") +
    scale_x_discrete(name = "Comparison") +
    scale_y_continuous(name = "Effect Size", 
                       limits = ylims) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=8))+
    geom_hline(yintercept=0.1, linetype="solid", 
               color = "red", size=1) +
    geom_hline(yintercept=-0.1, linetype="solid", 
               color = "red", size=1) +
    geom_hline(yintercept = 0, linetype = 2, color = "blue", 
               size = 1) + 
    labs(title = paste0("Top ", nrow(df), " gap effect sizes"), 
         caption = expression("Calculation: d = " ~ frac(bar(Delta), sigma[pooled]) ~ "\n" ~
                                "Effect Size = " ~  frac(d, sqrt(d^2 + 4)))) + 
    facet_grid(~feature) + theme_bw()
  print(barp)
  
}
