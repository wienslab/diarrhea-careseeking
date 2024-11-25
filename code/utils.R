# summarize variable categories for table
summarize_cats <- function(indat) {
  
  cats <- indat %>%
    # melt long
    reshape2::melt(measure.vars = names(indat)) %>%
    # calculate number and percent
    group_by(variable, value) %>%
    mutate(Total = length(value)) %>%
    ungroup() %>%
    group_by(variable) %>%
    mutate(Percent = round(Total/length(variable)*100,1)) %>%
    ungroup() %>%
    unique() %>%
    # prettier variable names
    mutate(variable = gsub('_', ' ', variable),
           variable = str_to_sentence(variable),
           value = ifelse(is.na(value), 'Not reported', value)) %>%
    arrange(desc(variable), value)
  
  # return
  return(cats)
}


# make simple kable table with merged first column
pretty_table <- function(intab, title = NULL) {
  
  tab <- intab %>%
    # make kable
    kable(escape = F, font = 12, row.names = FALSE, caption = title) %>%
    # simple striped, bordered table
    kable_styling(bootstrap_options = c('bordered', 'striped', 'condensed'), 
                  position = 'left', full_width = FALSE) %>%
    # rows are black
    row_spec(1:nrow(intab), color='black') %>%
    # collapse rows
    collapse_rows(columns = 1, valign = 'top')
  
  # return
  return(tab)
}


# make an odds ratio table include reference categories
or_table <- function(coef_df, cov_vector = covs, cov_cats_df = cov_cats) {
  
  # get odds ratios
  df_or <- data.frame('Category' = gsub(paste(cov_vector, collapse = '|'), '', coef_df$mod_id),
                      'Odds ratio' = paste0(round(coef_df[,3],2),' (',
                                            round(coef_df[,4],2),' - ',
                                            round(coef_df[,5],2),')'),
                      'Significant' = ifelse((coef_df[,4] < 1 & coef_df[,5] < 1) | 
                                               (coef_df[,4] > 1 & coef_df[,5] > 1),
                                             1, 0)) 
  
  # get variable names
  df_or <- df_or %>%
    mutate(Category = ifelse(Category == '', '1', Category), # binary variables
           Variable = gsub(paste(df_or$Category, collapse = '|'), '', coef_df$mod_id),
           `Odds ratio` = Odds.ratio) %>%
    dplyr::select(Variable, Category, `Odds ratio`, Significant)
  
  # add reference categories
  df_or <- df_or %>%
    full_join(cov_cats_df %>% filter(Variable %in% df_or$Variable)) %>%
    arrange(Variable, Category) %>%
    mutate(`Odds ratio` = ifelse(Reference == 1, '1 [Reference]', `Odds ratio`)) %>%
    dplyr::select(-Reference) %>%
    # tidy significance column
    mutate(` ` = ifelse(Significant == 0 | is.na(Significant), '', '**')) %>%
    dplyr::select(-Significant)
  
  # return
  return(df_or)
}


# proportion seeking care by grouping variable of interest
plot_seekprop <- function(indat, x_col, 
                          y_col = 'propseek',
                          rotate_x_labs = FALSE,
                          color_col = x_col,
                          palette = 'Dark2', a = 0.75,
                          num_colors = length(unique(indat[,color_col]))) {
  # plot
  ggplot(indat, aes(x = as.factor(get(x_col)), y = get(y_col))) +
    # boxplot without outliers
    geom_boxplot(outlier.shape = NA) +
    # points (will include the outliers)
    geom_jitter(aes(color = as.factor(get(color_col))),
                alpha = a) +
    theme_bw() +
    # wrap x axis labels
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    # color by grouping variable
    scale_color_manual(values=brewer.pal(n=num_colors, name=palette)) +
    # format axes
    theme(axis.text.x = element_text(angle = ifelse(rotate_x_labs, 45, 0), 
                                     hjust = ifelse(rotate_x_labs, 1, 0.5)),
          axis.text = element_text(size = 10),
          legend.position = 'none') +
    ylab('Proportion seeking care') + xlab('') +
    # title based on grouping variable
    ggtitle(str_to_sentence(gsub('_', ' ', x_col, )))
}


# country-level data coverage plot
data_map <- function(dt_plot, shp_plot, ad, 
                     num_max = 10,
                     plotting_prop = FALSE) {
  
  dt_plot <- dt_plot %>%
    # subset to gegraphic level
    filter(variable==ad) %>%
    # set an numbers above max num to max
    mutate(plot_var = ifelse(observations > num_max, num_max, observations))
  
  # add to shapefile
  dt_shp <- merge(shp_plot, dt_plot, by.x = 'ISO_A3', by.y = 'country_iso3', all = T)
  
  # plot
  myplot <- 
    ggplot(data = dt_shp) +
    geom_sf(colour = 'grey20', aes(fill = plot_var), show.legend = T) +
    coord_sf(xlim = c(-155, 145), ylim = c(-35, 80)) + 
    ggtitle(str_to_title(ad)) +
    theme(text = element_text(size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(),
          panel.border = element_rect(colour = 'black', fill=NA, size = 0.5)) +
    if (plotting_prop == TRUE) {
      scale_fill_viridis_c('Proportion', na.value = 'grey80', limits = c(0, 1))
    } else {
      scale_fill_viridis_c('Number', na.value = 'grey80', 
                           option = 'plasma', limits = c(1, num_max))
    } 
  return(myplot)
}

