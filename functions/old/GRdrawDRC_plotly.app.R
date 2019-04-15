GRdrawDRC_plotly.app <- function(fitData, metric = c("GR", "rel_cell"), experiments = list(),
                          min = "auto", max = "auto",
                          color = "experiment",
                          points = c("average", "all", "none"),
                          curves = c("sigmoid", "line", "biphasic", "sigmoid_high", "sigmoid_low", "none"),
                          bars = c("none", "sd", "se"),
                          xrug = c("none", "GR50", "GEC50", "IC50", "EC50"),
                          yrug = c("none", "GRinf", "GRmax", "Einf", "Emax"),
                          theme = c("classic", "minimal", "bw"),
                          palette = c("default","npg", "aaas"),
                          facet = "none",
                          plot_type = c("static", "interactive"),
                          output_type = c("together", "separate")#,
                          #legend = c("none", "right", "only")
) {
  tic("drawing plots - making data frames")
  # make all inputs length 1
  metric = metric[1]
  points = points[1]
  curves = curves[1]
  bars = bars[1]
  xrug = xrug[1]
  yrug = yrug[1]
  theme = theme[1]
  palette = palette[1]
  plot_type = plot_type[1]
  output_type = output_type[1]
  # get grouping variables
  group_vars = GRmetrics::GRgetGroupVars(fitData)
  # assertthat::assert_that(palette %in% c("default","npg", "aaas"), 
  #                         msg = "palette must be one of the following: 'default', 'npg', 'aaas'")
  # check that metric is allowed
  assertthat::assert_that(is.character(metric))
  assertthat::assert_that(metric %in% c("GR", "rel_cell"))
  # check that curve parameter is allowed
  assertthat::assert_that(is.character(curves))
  assertthat::assert_that(curves %in% c("sigmoid", "line", "biphasic", "sigmoid_high", "sigmoid_low", "none"))
  # check that color variable is allowed
  print(color)
  assertthat::assert_that(color %in% c("experiment", group_vars), 
                          msg = "'color' must be either 'experiment' or one of the grouping variables")
  color = dplyr::ensym(color)
  # check that xrug and yrug are allowed
  # if(curves == "biphasic") {
  #   if(metric == "GR") {
  #     xrug_options = c("none", "GEC50_1", "GEC50_2")
  #     yrug_options = c("none", "GRinf_1", "GRinf_2", "GRmax")
  #   } else {
  #     xrug_options = c("none", "EC50_1", "EC50_2")
  #     yrug_options = c("none", "Einf_1", "Einf_2", "Emax")
  #   }
  # } else {
  #   if(metric == "GR") {
  #     xrug_options = c("none", "GR50", "GEC50")
  #     yrug_options = c("none", "GRinf", "GRmax")
  #   } else {
  #     xrug_options = c("none", "IC50", "EC50")
  #     yrug_options = c("none", "Einf", "Emax")
  #   }
  # }
  # assertthat::assert_that(is.character(xrug))
  # assertthat::assert_that(xrug %in% xrug_options)
  # assertthat::assert_that(is.character(yrug))
  # assertthat::assert_that(yrug %in% yrug_options)
  # check that facets are allowed
  #assertthat::assert_that(length(facet) == 1)
  assertthat::assert_that(all(facet %in% group_vars) | identical(facet, "none"),
                          msg = 'facet must be either "none" or a subset of the grouping variables')
  assertthat::assert_that(metric != "IC", msg = 'For the traditional dose-response curve based on relative cell counts, please use metric = "rel_cell" instead of metric = "IC". This notation has been changed as of Version 1.3.2.')
  # data frame for points
  data = GRmetrics::GRgetValues(fitData)
  # data frame for metrics, to make curves and rugs
  parameter_list = GRmetrics::GRgetMetrics(fitData)[[metric]]
  if(curves == "sigmoid") { parameterTable =  parameter_list$sigmoid$normal }
  if(curves == "sigmoid_high") { parameterTable =  parameter_list$sigmoid$high }
  if(curves == "sigmoid_low") { parameterTable =  parameter_list$sigmoid$low }
  if(curves == "biphasic") { parameterTable =  parameter_list$biphasic$normal }
  if(curves == "line") { parameterTable =  parameter_list$sigmoid$normal }
  if(curves == "none") { parameterTable =  parameter_list$sigmoid$normal }
  
  
  if(length(group_vars) == 0) data$experiment = "All Data"
  # change experiment to character from factor if necessary
  data$experiment = as.character(data$experiment) 
  # filter to only selected experiments
  # if(!identical(experiments, "all")) {
  #   parameterTable %<>% dplyr::filter(experiment %in% experiments)
  #   data %<>% dplyr::filter(experiment %in% experiments)
  # }
  if(!identical(experiments, list() )) {
    assertthat::assert_that( class(experiments) == "list" )
    assertthat::assert_that( sum(!names(experiments) %in% group_vars) == 0 )
    for(i in 1:length(experiments)) {
      temp_grp = names(experiments)[i]
      temp_list = experiments[[i]]
      ### make sure all groups for filtering exist
      assertthat::assert_that(sum(!temp_list %in% unique(data[[temp_grp]])) == 0)
      ### filter data
      data = data[ data[[temp_grp]] %in% temp_list,]
      parameterTable = parameterTable[ parameterTable[[temp_grp]] %in% temp_list,]
    }
  }
  min = min(data$concentration, na.rm = TRUE)
  max = max(data$concentration, na.rm = TRUE)
  # define x support for curve
  len = (log10(max) - log10(min))*20
  concentration = 10^(seq(log10(min) - 1, log10(max) + 1, length.out = len))
  # define function for sigmoid curve mapping
  .create_curve_data = function(EC50, Einf, h, fit, flat, experiment, cc) {
    df = data.frame(experiment = experiment, concentration = cc, log10_concentration = log10(cc))
    if(fit == "curve") df$y_val = Einf + (1 - Einf)/(1 + (cc / (10^EC50))^h)
    if(fit == "flat") df$y_val = flat
    return(df)
  }
  # define function for biphasic sigmoid curve mapping
  .create_biphasic_data = function(EC50_1, Einf_1, h_1, EC50_2, Einf_2, h_2,
                                   fit, flat,
                                   experiment, cc) {
    df = data.frame(experiment = experiment, concentration = cc, log10_concentration = log10(cc))
    term1 = 1 + (Einf_1 + (1 - Einf_1)/(1 + (cc / (10^EC50_1)) ^ h_1))
    term2 = 1 + (Einf_2 + (1 - Einf_2)/(1 + (cc / (10^EC50_2)) ^ h_2))
    if(fit == "curve") { df$y_val = 2^( 0.5*( log2(term1) + log2(term2) ) ) - 1 }
    if(fit == "flat")  df$y_val = flat
    return(df)
  }
  # make list (tibble) of inputs for curve
  if(metric == "rel_cell") {
    if(grepl("sigmoid", curves)) {
      curve_input_list = parameterTable %>% dplyr::select(log10_EC50, Einf, h, 
                                                          fit, flat, experiment) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::rename(EC50 = log10_EC50) %>%
        #dplyr::rename(fit_type = fit_rel_cell, flat_fit = flat_fit_rel_cell) %>% 
        dplyr::as_tibble() %>%
        dplyr::mutate(c = list(concentration))
    } else if(grepl("biphasic", curves)) {
      curve_input_list = parameterTable %>% dplyr::select(log10_EC50_1, Einf_1, h_1, 
                                                          log10_EC50_2, Einf_2, h_2,
                                                          fit, flat, experiment) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::rename(EC50_1 = log10_EC50_1,
                      EC50_2 = log10_EC50_2) %>% 
        dplyr::as_tibble() %>% dplyr::mutate(cc = list(concentration))
    }
  } else if(metric == "GR") {
    if(grepl("sigmoid", curves)) {
      curve_input_list = parameterTable %>% dplyr::select(log10_GEC50, GRinf, h_GR, 
                                                          fit, flat, experiment) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::rename(EC50 = log10_GEC50, Einf = GRinf, h = h_GR) %>% 
        dplyr::as_tibble() %>% dplyr::mutate(cc = list(concentration))
    } else if(grepl("biphasic", curves)) {
      curve_input_list = parameterTable %>% dplyr::select(log10_GEC50_1, GRinf_1, h_GR_1, 
                                                          log10_GEC50_2, GRinf_2, h_GR_2,
                                                          fit, flat, experiment) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::rename(EC50_1 = log10_GEC50_1, Einf_1 = GRinf_1, h_1 = h_GR_1,
                      EC50_2 = log10_GEC50_2, Einf_2 = GRinf_2, h_2 = h_GR_2) %>% 
        dplyr::as_tibble() %>% dplyr::mutate(c = list(concentration))
    }
  }
  # make data frame for mapping "experiment" to grouping variables
  data_for_join = parameterTable %>% dplyr::select_at(c("experiment", group_vars)) %>%
    dplyr::mutate_if(is.factor, as.character)
  # data frame for curves to give to ggplot
  if(grepl("sigmoid", curves)) {
    curve_data_all = suppressWarnings(purrr::pmap_dfr(.l = curve_input_list, .f = .create_curve_data)) %>%
      dplyr::left_join(data_for_join, by = "experiment")
  } else if(grepl("biphasic", curves)) {
    curve_data_all = suppressWarnings(purrr::pmap_dfr(.l = curve_input_list, .f = .create_biphasic_data)) %>%
      dplyr::left_join(data_for_join, by = "experiment")
  }
  
  # data frame for (all) points
  if(metric == "GR") {
    data %<>% dplyr::select_at(c(group_vars, "concentration", "log10_concentration", 
                                 "GRvalue", "experiment")) %>%
      dplyr::rename(y_val = GRvalue)
  } else if(metric == "rel_cell") {
    data %<>% dplyr::select_at(c(group_vars, "concentration", "log10_concentration", 
                                 "rel_cell_count", "experiment")) %>%
      dplyr::rename(y_val = rel_cell_count)
  }
  # data frame for (average) points
  data_mean = data %>% dplyr::group_by_at(c(group_vars, "experiment", 
                                            "concentration", "log10_concentration")) %>%
    dplyr::summarise(y_val_mean = mean(y_val, na.rm = TRUE), 
                     y_val_sd = sd(y_val, na.rm = TRUE),
                     ynum = length(y_val)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(y_val_se = y_val_sd/sqrt(ynum))
  
  # round to 3 significant digits
  data %<>% dplyr::mutate_if(is.numeric, function(x) signif(x,3)) %>%
    dplyr::mutate_at(group_vars, function(x) factor(x, levels = sort(unique(x)))) # points (all)
  data_mean %<>% dplyr::mutate_if(is.numeric, function(x) signif(x,3)) %>%
    dplyr::mutate_at(group_vars, function(x) factor(x, levels = sort(unique(x)))) # points (average)
  parameterTable %<>% dplyr::mutate_if(is.numeric, function(x) signif(x,3)) %>%
    dplyr::mutate_at(group_vars, function(x) factor(x, levels = sort(unique(x)))) # rugs
  if(!curves %in% c("line","none")) {
    curve_data_all %<>% dplyr::mutate_if(is.numeric, function(x) signif(x,3)) %>%
      dplyr::mutate_at(group_vars, function(x) factor(x, levels = sort(unique(x)))) # curves
  }
  # initialize plot
  p = plotly::plot_ly()
  # add curves to the plot
  .create_plots = function(p, data, data_mean, parameterTable, curve_data_all, legend = "none", leg_colors, color) {
    if(curves == "line") {
      text = with(data_mean, (sprintf(ifelse(metric == "GR", paste0("GR value: ", "%.3f<br>log10(concentration): %.3f"),paste0("Relative cell viability: ", "%.3f<br>log10(concentration): %.3f") ), y_val_mean, log10_concentration)))
      p = p %>% plotly::add_lines(data = data_mean, x = ~log10_concentration, y = ~y_val_mean, 
                                    color = ~eval(color), split = ~experiment, legendgroup = ~eval(color), 
                                    text = text)
    } else if(curves %in% c("sigmoid", "biphasic", "sigmoid_low", "sigmoid_high")) {
      text = with(curve_data_all, sprintf(ifelse(metric == "GR", 
                            paste0("GR value: ", "%.3f<br>log10(concentration): %.3f"),
                            paste0("Relative cell viability: ", "%.3f<br>log10(concentration): %.3f") ), y_val, log10_concentration))
      p = p %>% plotly::add_lines(data = curve_data_all, x = ~log10_concentration, y = ~y_val,
                                  split = ~experiment, color = ~eval(color), legendgroup = ~eval(color), 
                                  text = text)
    } else if(curves == "none") {
      # do nothing
    }
    # add points to the plot
    if(points == "average") {
      text = text = with(data_mean, sprintf(ifelse(metric == "GR", paste0("GR value: ", "%.3f<br>log10(concentration): %.3f"),
                                   paste0("Relative cell viability: ", "%.3f<br>log10(concentration): %.3f") ), y_val_mean, log10_concentration))
      p = p %>% plotly::add_markers(data = data_mean, x = ~log10_concentration, y = ~y_val_mean, 
                                    color = ~eval(color), split = ~experiment, legendgroup = ~eval(color), 
                                    text = text)
    } else if(points == "all") {
      text = with(data, sprintf(ifelse(metric == "GR", paste0("GR value: ", "%.3f<br>log10(concentration): %.3f"),
                            paste0("Relative cell viability: ", "%.3f<br>log10(concentration): %.3f") ), y_val, log10_concentration))
      p = p %>% plotly::add_markers(data = data, x = ~log10_concentration, y = ~y_val, 
                                    color = ~eval(color), legendgroup = ~eval(color), split = ~experiment,
                                    text = text)
    } else if(points == "none") {
      # do nothing
    }
    # add error bars to the plot
    bar_width = 0
    if(bars == "sd") {
      # p = p + ggplot2::geom_errorbar(data = data_mean, ggplot2::aes(x = log10_concentration, y = y_val_mean,
      #                                                               ymin = y_val_mean - y_val_sd, ymax = y_val_mean + y_val_sd, 
      #                                                               text = sprintf(ifelse(metric == "GR", paste0("GR value: ", "%.3f<br>log10(concentration): %.3f"),
      #                                                                                     paste0("Relative cell viability: ", "%.3f<br>log10(concentration): %.3f") ), y_val_mean, log10_concentration),
      #                                                               colour = !!color, group = experiment), width = bar_width)
    } else if(bars == "se") {
      # p = p + ggplot2::geom_errorbar(data = data_mean, ggplot2::aes(x = log10_concentration, y = y_val_mean,
      #                                                               ymin = y_val_mean - y_val_se, ymax = y_val_mean + y_val_se, 
      #                                                               text = sprintf(ifelse(metric == "GR", paste0("GR value: ", "%.3f<br>log10(concentration): %.3f"),
      #                                                                                     paste0("Relative cell viability: ", "%.3f<br>log10(concentration): %.3f") ), y_val_mean, log10_concentration),
      #                                                               colour = !!color, group = experiment), width = bar_width)
    }
    # p = p +# ggplot2::xlab('Concentration (log10 scale)')
    #   ggplot2::xlab("") + ggplot2::ylab("")
    if(metric == "GR") {
      # # set x and y range for plot, set labels, add horizontal lines
      # p = p + ggplot2::coord_cartesian(xlim = c(log10(min)-0.1,
      #                                           log10(max)+0.1),
      #                                  ylim = c(-1, 1.5), expand = F) +
      #   ggplot2::ylab("") +
      #   ggplot2::xlab("") +
      #   ggplot2::geom_hline(yintercept = 0, size = 1, colour = "#1F77B4")

    } else if(metric == "rel_cell") {
      # set x and y range for plot, set labels, add horizontal lines
      # p = p + ggplot2::coord_cartesian(xlim = c(log10(min)-0.1,
      #                                           log10(max)+0.1),
      #                                  ylim = c(0, 1.5), expand = F) +
      #   ggplot2::ylab("") +
      #   ggplot2::xlab("")

    }
    
    # add rugs to plot
    # rug_size = 1.1
    # if(xrug != "none") p = p + ggplot2::geom_rug(data = parameterTable,
    #                                              ggplot2::aes_string(x = paste0("log10_", xrug), colour = color, group = "experiment"), size = rug_size)
    # if(yrug != "none") p = p + ggplot2::geom_rug(data = parameterTable,
    #                                              ggplot2::aes_string(y = yrug, colour = color, group = "experiment"), size = rug_size)
    return(p)
    # configure plot facets
    # if(!identical(facet, "none")) {
    #   facet = dplyr::syms(facet)
    #   #p = p + lemon::facet_rep_wrap(facet, ncol = 5)
    #   p = p + ggplot2::facet_wrap(facet, ncol = 5)
    # }
    # # add theme to plot, keep aspect ratio 1:1
    # p = p + ggplot2::theme_classic() + ggplot2::theme(legend.position = legend)  +
    #   ggplot2::scale_colour_manual(values=leg_colors) #+ do.call(theme, args = list())
    # # add palette to plot
    # ###p = p + scale_colour_npg()
    # # return ggplot or plotly object
    # if(plot_type == "interactive") return(plotly::ggplotly(p))
    # if(plot_type == "static") return(p + ggplot2::theme(aspect.ratio = 1))
  }
  toc()
  if(output_type == "separate") {
    tic("separate plots")
    ### get legend only
    ### call create plot function once to output one plot object
    leg_groups = unique(data[[color]])
    leg_len = length(leg_groups)
    leg_colors = scales::hue_pal()(leg_len)
    names(leg_colors) = leg_groups
    if(curves %in% c("line", "none"))  curve_data_all = NA
    out_legend = .create_plots(p = p, data = data, data_mean = data_mean, 
                               parameterTable = parameterTable, curve_data_all = curve_data_all,
                               legend = "right", leg_colors = leg_colors, 
                               color = color)
    out_legend = cowplot::ggdraw(cowplot::get_legend(out_legend))
    facet_char = paste0(facet, collapse = "_")
    if(length(facet) > 1) {
      facet_list = syms(facet)
      facet = sym(facet_char)
      data %<>% dplyr::mutate(!!facet := paste(!!!facet_list, sep = "_"))
      data_mean %<>% dplyr::mutate(!!facet := paste(!!!facet_list, sep = "_"))
      parameterTable %<>% dplyr::mutate(!!facet := paste(!!!facet_list, sep = "_"))
      if(!curves %in% c("line", "none")) {
        curve_data_all %<>% dplyr::mutate(!!facet := paste(!!!facet_list, sep = "_"))
      }
    }
    data_list  = split(data, f = data[[facet_char]])
    data_mean_list  = split(data_mean, f = data_mean[[facet_char]])
    parameterTable_list = split(parameterTable, f = parameterTable[[facet_char]])
    if(!curves %in% c("line", "none")) {
      curve_data_all_list = split(curve_data_all, f = curve_data_all[[facet_char]])
    } else {
      curve_data_all_list = NA
    }
    
    leg_colors_list = lapply(data_list, function(x) leg_colors[unique(x[[color]])])
    data_input_list = list(p = lapply(1:length(unique( data[[facet_char]] )), function(x) return(p)), 
                           data = data_list, data_mean = data_mean_list, 
                           parameterTable = parameterTable_list, curve_data_all = curve_data_all_list,
                           legend = "none", leg_colors = leg_colors_list, 
                           color = lapply(1:length(unique( data[[facet_char]] )), function(x) return(color) ))
    out = purrr::pmap(.l = data_input_list, .f = .create_plots)
    toc()
    return(list(plot = out, legend = out_legend))
  } else if(identical(output_type, "together")) {
    tic("one plot")
    leg_groups = unique(data[[color]])
    leg_len = length(leg_groups)
    leg_colors = scales::hue_pal()(leg_len)
    names(leg_colors) = leg_groups
    
    ### call create plot function once to output one plot object
    out = .create_plots(p = p, data = data, data_mean = data_mean, 
                        parameterTable = parameterTable, curve_data_all = curve_data_all,
                        legend = "right", leg_colors = leg_colors,
                        color = color)
    toc()
    return(list(plot = out))
  }
}
