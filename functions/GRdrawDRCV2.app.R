GRdrawDRCV2.app = function(fitData, 
                       points = c("average", "all", "none"),
                       curves = c("sigmoid", "line", "none"),
                       color = "experiment",
                       experiments = list(),
                       plot_type = c("static", "interactive"),
                       output_type = c("together", "separate")
                       ) {
  #### check input #####
  # make all inputs length 1
  #metric = metric[1]
  points = points[1]
  curves = curves[1]
  # bars = bars[1]
  # xrug = xrug[1]
  # yrug = yrug[1]
  # theme = theme[1]
  # palette = palette[1]
  plot_type = plot_type[1]
  output_type = output_type[1]

  #########
  # data frame for points
  fit_df = fitData$metadata$gr_table
  # data frame for metrics, to make curves and rugs
  params_GR_s = fitData$assays$GR$sigmoid$static
  params_GR_d = fitData$assays$GR$sigmoid$toxic
  
  # get grouping variables
  group_vars = GRmetrics::GRgetGroupVars(fitData)
  #group_vars = fitData$metadata$groupingVariables
  # check that color variable is allowed
  assertthat::assert_that(color %in% c("experiment", group_vars), 
                          msg = "'color' must be either 'experiment' or one of the grouping variables")
  color = dplyr::ensym(color)
  id = unique(c(group_vars, "concentration"))
  data = fit_df %>%
    reshape2::melt(id.vars = id,
                   measure.vars = c("GR_static", "GR_toxic", "GRvalue"),
                   value.name = "GRvalue", variable.name = "GR_metric"
                    )
  # filter to only selected experiments
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
      params_GR_s = params_GR_s[ params_GR_s[[temp_grp]] %in% temp_list,]
      params_GR_d = params_GR_d[ params_GR_d[[temp_grp]] %in% temp_list,]
    }
  }
  ### what should I do with GR values with concentration zero?
  #ctrl_df = data %>% dplyr::filter(Conc == 0)
  grp = syms(c(id, "GR_metric"))
  #exp_sym = syms(c(group_vars, "treatment_duration__hrs"))
  exp_sym = syms(group_vars)
  data_mean = data %>%
    dplyr::group_by(!!!grp) %>%
    dplyr::summarize(GRvalue = mean(GRvalue, na.rm = T)) %>%
    dplyr::ungroup() 
  data_mean %<>%
    dplyr::mutate(experiment = paste(!!!exp_sym)) %>%
    dplyr::filter(concentration > 0) %>%
    dplyr::filter(GR_metric %in% c("GR_toxic", "GR_static"))
  data %<>%
    dplyr::mutate(experiment = paste(!!!exp_sym)) %>%
    dplyr::filter(concentration > 0) %>%
    dplyr::filter(GR_metric %in% c("GR_toxic", "GR_static"))
  
  min = min(data$concentration, na.rm = TRUE)
  max = max(data$concentration, na.rm = TRUE)
  # define x support for curve -- with a given number of points between each log value
  points_per_log10 = 10
  len = (log10(max) - log10(min))*points_per_log10
  concentration = 10^(seq(log10(min) - 1, log10(max) + 1, length.out = len))
  # define functions for sigmoid curve mapping
  .create_GR_s_data = function(EC50, Einf, h, fit, flat, experiment, cc) {
    df = data.frame(experiment = experiment, concentration = cc, 
                    log10_concentration = log10(cc))
    if(fit == "curve") df$GRvalue = Einf + (1 - Einf)/(1 + (cc / (10^EC50))^h)
    if(fit == "flat") df$GRvalue = flat
    return(df)
  }
  .create_GR_d_data = function(EC50, Einf, h, fit, flat, experiment, cc) {
    df = data.frame(experiment = experiment, concentration = cc, 
                    log10_concentration = log10(cc))
    if(fit == "curve") df$GRvalue = Einf + (0 - Einf)/(1 + (cc / (10^EC50))^h)
    if(fit == "flat") df$GRvalue = flat
    return(df)
  }
  curve_inputs_GR_s = params_GR_s %>% 
    dplyr::select(log10_GEC50, GRinf, h_GR, fit, flat, experiment) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::rename(EC50 = log10_GEC50, Einf = GRinf, h = h_GR) %>% 
    dplyr::as_tibble() %>% dplyr::mutate(cc = list(concentration))
  curve_inputs_GR_d = params_GR_d %>% 
    dplyr::select(log10_GEC50, GRinf, h_GR, fit, flat, experiment) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::rename(EC50 = log10_GEC50, Einf = GRinf, h = h_GR) %>% 
    dplyr::as_tibble() %>% dplyr::mutate(cc = list(concentration))
  # make data frame for mapping "experiment" to grouping variables
  data_for_join_GR_s = params_GR_s %>% dplyr::select_at(c("experiment", group_vars)) %>%
    dplyr::mutate_if(is.factor, as.character)
  data_for_join_GR_d = params_GR_s %>% dplyr::select_at(c("experiment", group_vars)) %>%
    dplyr::mutate_if(is.factor, as.character)
  # data frame for curves to give to ggplot
  curve_data_all_GR_s = suppressWarnings(
    purrr::pmap_dfr(.l = curve_inputs_GR_s, 
                    .f = .create_GR_s_data)) %>%
      dplyr::left_join(data_for_join_GR_s, by = "experiment") %>%
    dplyr::mutate(GR_metric = "GR_static")
  curve_data_all_GR_d = suppressWarnings(
    purrr::pmap_dfr(.l = curve_inputs_GR_d, 
                    .f = .create_GR_d_data)) %>%
    dplyr::left_join(data_for_join_GR_d, by = "experiment") %>%
    dplyr::mutate(GR_metric = "GR_toxic")
  curve_data_all = dplyr::bind_rows(curve_data_all_GR_s, curve_data_all_GR_d)
  n_plots = length(unique(data$experiment))
  if(n_plots > 25) warning("Too many plots [change warning message later]")
  p = ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, size = 1, colour = "gray") +
    ggplot2::geom_hline(yintercept = 1, size = 1, colour = "gray")
  #### start create plots function #########
  parameterTable = dplyr::bind_rows(params_GR_s, params_GR_d)
  
  # round to 3 significant digits and convert grouping variables to factors
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
  ### make sure facets are in the correct order
  curve_data_all %<>% mutate(GR_metric = as.factor(GR_metric) %>% relevel(ref = "GR_static"))
  data %<>% mutate(GR_metric = as.factor(GR_metric) %>% relevel(ref = "GR_static"))
  data_mean %<>% mutate(GR_metric = as.factor(GR_metric) %>% relevel(ref = "GR_static"))
  
  
  
  .create_plots = function(p, data, data_mean, parameterTable, curve_data_all, legend = "none", leg_colors) {
    if(curves == "line") {
      p = p + ggplot2::geom_line(data = data_mean, 
                ggplot2::aes(x = log10(concentration), y = GRvalue, 
                  text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                  colour = GR_metric, group = GR_metric), size = 1.1)
    } else if(curves == "sigmoid") {
      p = p + ggplot2::geom_line(data = curve_data_all, 
                ggplot2::aes(x = log10(concentration), y = GRvalue, 
                  text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                  colour = GR_metric, group = GR_metric), size = 1.1)
    }
    if(points == "average") {
      p = p + ggplot2::geom_point(data = data_mean,
                aes(x = log10(concentration), y = GRvalue, 
                    text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                    colour = GR_metric, group = GR_metric))
    } else if (points == "all") {
      p = p + ggplot2::geom_point(data = data,
                aes(x = log10(concentration), y = GRvalue, 
                    text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                    colour = GR_metric, group = GR_metric))
    }
    # add error bars to the plot
    # bar_width = 0
    # if(bars == "sd") {
    #   p = p + ggplot2::geom_errorbar(data = data_mean, 
    #         ggplot2::aes(x = log10_concentration, y = y_val_mean,
    #            ymin = y_val_mean - y_val_sd, ymax = y_val_mean + y_val_sd, 
    #             colour = !!color, group = experiment), width = bar_width)
    # } else if(bars == "se") {
    #   p = p + ggplot2::geom_errorbar(data = data_mean, 
    #         ggplot2::aes(x = log10_concentration, y = y_val_mean,
    #            ymin = y_val_mean - y_val_se, ymax = y_val_mean + y_val_se, 
    #                colour = !!color, group = experiment), width = bar_width)
    # }
    # set x and y range for plot, set labels, add horizontal lines
    p = p + ggplot2::coord_cartesian(xlim = c(log10(min)-0.1,
                                              log10(max)+0.1),
                                     ylim = c(-1, 1.5), expand = F) +
      ggplot2::ylab("") +
      ggplot2::xlab("")
      #ggplot2::ggtitle("Concentration vs. GR values") +
      #ggplot2::ylab('GR value')
    #ggplot2::geom_hline(yintercept = 0.5, size = 1, linetype = "dashed") +
    #ggplot2::geom_hline(yintercept = -1, size = 1, linetype = "dashed")
    # configure plot facets
    p = p + ggplot2::facet_wrap(~experiment, ncol = 5)
    # add theme to plot, keep aspect ratio 1:1
    p = p + ggplot2::theme_classic() + ggplot2::theme(legend.position = legend)#+ do.call(theme, args = list())
    # add palette to plot
    ###p = p + scale_colour_npg()
    if(plot_type == "interactive") return(plotly::ggplotly(p))
    if(plot_type == "static") return(p + ggplot2::theme(aspect.ratio = 1))
  }
  ###### end create plots function ####
  if(output_type == "separate") {
    ### get legend only
    ### call create plot function once to output one plot object
    #leg_groups = unique(data[[color]])
    leg_groups = c("GR_static", "GR_toxic")
    leg_len = length(leg_groups)
    leg_colors = scales::hue_pal()(leg_len)
    names(leg_colors) = leg_groups
    out_legend = .create_plots(p = p, data = data, data_mean = data_mean, 
                               parameterTable = parameterTable, 
                               curve_data_all = curve_data_all,
                               legend = "right", leg_colors = leg_colors)
    out_legend = cowplot::ggdraw(cowplot::get_legend(out_legend))
      
    data_list  = split(data, f = data$experiment)
    data_mean_list  = split(data_mean, f = data_mean$experiment)
    parameterTable_list = split(parameterTable, f = parameterTable$experiment)
    curve_data_all_list = split(curve_data_all, f = curve_data_all$experiment)
    
    leg_colors_list = lapply(data_list, function(x) return(leg_colors))
    data_input_list = list(p = lapply(1:length(unique( data$experiment )), 
                                      function(x) return(p)), 
                           data = data_list, data_mean = data_mean_list, 
                           parameterTable = parameterTable_list, curve_data_all = curve_data_all_list,
                           legend = "none", leg_colors = leg_colors_list 
                           #color = lapply(1:length(unique( data$experiment )), function(x) return(color) )
                           )
    out = purrr::pmap(.l = data_input_list, .f = .create_plots)
    return(list(plot = out, legend = out_legend))
  } else {
    # leg_groups = c("GR_static", "GR_toxic")
    # leg_len = length(leg_groups)
    # leg_colors = scales::hue_pal()(leg_len)
    # names(leg_colors) = leg_groups
    # ### call create plot function once to output one plot object
    # out = .create_plots(p = p, data = data, data_mean = data_mean, 
    #                     parameterTable = parameterTable, curve_data_all = curve_data_all,
    #                     legend = "right", leg_colors = leg_colors)
    
    if(curves == "line") {
      p = p + ggplot2::geom_line(data = data_mean, 
                                 ggplot2::aes(x = log10(concentration), y = GRvalue, 
                                              text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                                              colour = !!color, group = experiment), size = 1.1)
    } else if(curves == "sigmoid") {
      p = p + ggplot2::geom_line(data = curve_data_all, 
                                 ggplot2::aes(x = log10(concentration), y = GRvalue, 
                                              text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                                              colour = !!color, group = experiment), size = 1.1)
    }
    if(points == "average") {
      p = p + ggplot2::geom_point(data = data_mean,
                                  aes(x = log10(concentration), y = GRvalue, 
                                      text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                                      colour = !!color, group = experiment))
    } else if (points == "all") {
      p = p + ggplot2::geom_point(data = data,
                                  aes(x = log10(concentration), y = GRvalue, 
                                      text = sprintf("GR value: %.3f<br>log10(concentration): %.3f", GRvalue, log10(concentration) ),
                                      colour = !!color, group = experiment))
    }
    # add error bars to the plot
    # bar_width = 0
    # if(bars == "sd") {
    #   p = p + ggplot2::geom_errorbar(data = data_mean, 
    #         ggplot2::aes(x = log10_concentration, y = y_val_mean,
    #            ymin = y_val_mean - y_val_sd, ymax = y_val_mean + y_val_sd, 
    #             colour = !!color, group = experiment), width = bar_width)
    # } else if(bars == "se") {
    #   p = p + ggplot2::geom_errorbar(data = data_mean, 
    #         ggplot2::aes(x = log10_concentration, y = y_val_mean,
    #            ymin = y_val_mean - y_val_se, ymax = y_val_mean + y_val_se, 
    #                colour = !!color, group = experiment), width = bar_width)
    # }
    # set x and y range for plot, set labels, add horizontal lines
    p = p + ggplot2::coord_cartesian(xlim = c(log10(min)-0.1,
                                              log10(max)+0.1),
                                     ylim = c(-1, 1.5), expand = F) +
      ggplot2::ylab("") +
      ggplot2::xlab("")
    #ggplot2::ggtitle("Concentration vs. GR values") +
    #ggplot2::ylab('GR value')
    #ggplot2::geom_hline(yintercept = 0.5, size = 1, linetype = "dashed") +
    #ggplot2::geom_hline(yintercept = -1, size = 1, linetype = "dashed")
    # configure plot facets
    p = p + ggplot2::facet_wrap(~GR_metric, ncol = 5)
    # add theme to plot, keep aspect ratio 1:1
    p = p + ggplot2::theme_classic() + ggplot2::theme(legend.position = "right")#+ do.call(theme, args = list())
    # add palette to plot
    ###p = p + scale_colour_npg()
    if(plot_type == "interactive") p = plotly::ggplotly(p)
    if(plot_type == "static") p = p + ggplot2::theme(aspect.ratio = 1)
    
    return(list(plot = p))
  }
}