createPlotData <- function(fitData, metric, curves, num_points = 20) {
  tic("drawing plots - making data frames")
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
  # get grouping variables
  group_vars = GRmetrics::GRgetGroupVars(fitData)
  assertthat::assert_that(is.character(metric))
  assertthat::assert_that(metric %in% c("GR", "rel_cell"))
  # check that curve parameter is allowed
  assertthat::assert_that(is.character(curves))
  assertthat::assert_that(curves %in% c("sigmoid", "line", "biphasic", "sigmoid_high", "sigmoid_low", "none"))
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
  min = min(data$concentration, na.rm = TRUE)
  max = max(data$concentration, na.rm = TRUE)
  # define x support for curve
  len = (log10(max) - log10(min))*num_points
  concentration = 10^(seq(log10(min) - 1, log10(max) + 1, length.out = len))
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
  return(list(data = data, curve_data_all = curve_data_all, data_mean = data_mean, parameterTable = parameterTable))
}