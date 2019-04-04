# UNIVARIATE ANALYSIS
# Summary of numeric data
summary_num_x <- function(data, x) {
    
    x <- enquo(x)
    
    num_x <- data %>%
        summarize(
            N = n(),
            Mean = mean(!! x, na.rm = TRUE), 
            SD = sd(!! x, na.rm = TRUE),
            Median = quantile(!! x, probs = 0.5, na.rm = TRUE),
            IQR = IQR(!! x, na.rm = TRUE),
            Min = quantile(!! x, probs = 0, na.rm = TRUE),
            Max = quantile(!! x, probs = 1, na.rm = TRUE),
            Missing = sum(is.na(!! x))
        )
    
    print(num_x)
    
}

# Summary of categorical (factor) data
summary_fct_x <- function(data, x) {
    
    x <- enquo(x)
    
    fct_x <- data %>%
        count(!! x) %>%
        mutate(Freq = n / sum(n) * 100) %>%
        rename(Levels = !! x, N = n) %>%
        mutate(Freq = round(Freq,  digits = 1))

    print(fct_x)

}

# BIVARIATE ANALYSIS
# Summary of numeric data by categorical (factor) data
summary_bi <- function(data, x, y, pairwise = TRUE) {
    
    # Creating the dataframe
    x <- enquo(x)
    y <- enquo(y)
    df <- data %>% 
        select(x = !! x, y = !! y)
    
    # Creating the summary table
    tbl <- df %>% 
        group_by(y) %>% 
        summarize(
            N = n(),
            Mean = mean(x, na.rm = TRUE), 
            SD = sd(x, na.rm = TRUE),
            Median = quantile(x, probs = 0.5, na.rm = TRUE),
            IQR = IQR(x, na.rm = TRUE),
            Min = quantile(x, probs = 0, na.rm = TRUE),
            Max = quantile(x, probs = 1, na.rm = TRUE),
            Missing = sum(is.na(x))
        ) %>%
        rename(
            Levels = y
        )
    
    # Testing for associations
    # Mann-Whitney if y == 2, Kruskal-Wallis if y > 2
    if (nlevels(df$y) > 2) {
        test_bi <- kruskal.test(x ~ y, df)
    }
    
    if (nlevels(df$y) == 2) {
        test_bi <- wilcox.test(x ~ y, df)
    }
    
    # Testing for pairwise associations if y > 2
    if (pairwise == TRUE) {
        test_pairwise <- with(df, pairwise.wilcox.test(x, y, p.adjust.method = "bonferroni"))
    }
    
    print(tbl)
    print(test_bi)
    if(nlevels(df$y) > 2 & pairwise == TRUE) print(test_pairwise)
     
}

# Summary of a contingency table between 2 factor variables x and y
# useNA = c("no", "ifany", "always")
# prop = c("cols", "rows")
summary_contingency <- function(data, x, y, useNA = "ifany", prop = c("cols", "rows")) {
    
    x <- enquo(x)
    y <- enquo(y)
    df <- data %>% 
        select(x = !! x, y = !! y)
    
    # Creating the crosstables with frequencies and percentages
    tbl <- table(df$x, df$y, useNA = useNA)
    prop_cols <- round(prop.table(tbl, margin = 2) * 100, digits = 1)
    prop_rows <- round(prop.table(tbl, margin = 1) * 100, digits = 1)
    
    # Testing for associations
    test <- with(df, chisq.test(x, y))
    
    # Printing the tables
    print(tbl)
    
    if("cols" %in% prop) {
        cat("\n Percentage by columns")
        print(prop_cols)
    }
    
    if("rows" %in% prop) {
        cat("\n Percentage by rows")
        print(prop_rows)
    }
    
    print(test)
    
}

# Creating a function for risk ratios and P values
RR <- function(data, x, y){
    
    x <- enquo(x)
    y <- enquo(y)
    df <- data %>% 
        select(x = !! x, y = !! y)
    
    rr_tbl <- epitools::riskratio(df$x, df$y, verbose = TRUE)
    exact <- Exact::exact.test(table(df$x, df$y), to.plot = FALSE)
    print(rr_tbl$data)
    print(exact)
    print(round(rr_tbl$measure, 2))
}

# PLOTS
# A function to serial plots of a numeric y varible over a factor x variable by a group variable
plot_serial <- function(data, x, y, group) {
    
    x = enquo(x)
    y = enquo(y)
    group = enquo(group)
    
    g <- ggplot(data, aes(x = !! x, y = !! y, group = !! group)) +
        geom_line(size = 1, alpha = 0.5) +
        geom_point(size = 3, alpha = 0.5) +
        labs(x = "", y = "")
    
    return(g)
}

# A function to compare paired values in a time-series (plots and Wilcoxon test)
# x = periods of the time series (v.g., pre and post)
# y = values to be compared
# group = grouping variable (v.g., patient's ID)
compare_ts <- function(data, x, y, group) {
    
    x = enquo(x)
    y = enquo(y)
    group = enquo(group)
    df <- data %>% 
        select(x = !! x, y = !! y, group = !! group)
    
    # Plot
    g <- df %>% 
        ggplot(aes(x = x, y = y, group = group)) +
        geom_point(size = 3, alpha = 0.5) +
        geom_line(size = 1, alpha = 0.5) + 
        labs(x = "", y = "")
    
    # Wilcox test for associations
    df_test <- df %>%
        select(group, x, y) %>% 
        spread(key = x, value = y)
    
    colnames(df_test) <- c("Group", "Pre", "Post")
    
    test <- with(df_test, wilcox.test(Pre, Post, paired = TRUE))
    
    # Printing the results
    print(g)
    print(test)
}