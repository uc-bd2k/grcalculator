drawDRC_new = function(fitData, metric = c("GR", "rel_cell"), experiments = list(),
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
                   ){
  data = fitData$data
  data_mean = fitData$data_mean
  curve_data_all = fitData$curve_data_all
  parameterTable = fitData$parameterTable
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
  
  if(length(group_vars) == 0) data$experiment = "All Data"
  # change experiment to character from factor if necessary
  data$experiment = as.character(data$experiment) 
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
  # initialize plot
  p = ggplot2::ggplot()
  # add curves to the plot

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