logistic_fit_GR = function(input_data,groupingColumns) {
  experiments = levels(input_data$experiment)
  parameters = matrix(data = NA, ncol = 3, nrow = length(experiments))
  parameters = as.data.frame(parameters)
  colnames(parameters) = c('Hill','GRinf','EC50')
  if(length(groupingColumns) > 0) {
    metadata = matrix(data = NA, ncol = length(groupingColumns), nrow = length(experiments))
    metadata = as.data.frame(metadata)
    colnames(metadata) = groupingColumns
  } else {
    metadata = NULL
  }
  pval = NULL
  GRmax = NULL
  GR_mean = NULL
  AOC = NULL
  R_square = NULL
  for(i in 1:length(experiments)) {
    # print(i)
    data_exp = input_data[input_data$experiment == experiments[i],]
    concs = sort(unique(data_exp$concentration))
    l = length(concs)
    max_concs = data_exp[data_exp$concentration %in% concs[c(l,l-1)],]
    GRmax[i] = min(max_concs$GR)
    #     metadata[i,] = data_exp[1,1:5]
    if(!is.null(metadata)) {
      metadata[i,] = data_exp[1,groupingColumns, drop = F]
    }
    GR_mean[i] = mean(data_exp$GR)
    #=============== constrained fit ===============
    c = unique(data_exp$concentration)
    priors = c(2, 0.1, median(c))
    lower = c(.1, -1, min(c)*1e-2)
    upper = c(5, 1, max(c)*1e2)
    if(dim(data_exp)[1] > 1) {
      output_model_new = try(drm(GR~concentration, experiment, data=data_exp, fct=LL.3u(names=c('Hill','GRinf','EC50')), start = priors, lowerl = lower, upperl = upper))
      if(class(output_model_new)!="try-error") {
        parameters[i,] = c(as.numeric(coef(output_model_new)))
        # F-test for the significance of the sigmoidal fit
        Npara = 3 # N of parameters in the growth curve
        Npara_flat = 1 # F-test for the models
        RSS2 = sum(residuals(output_model_new)^2)
        RSS1 = sum((data_exp$GR - mean(data_exp$GR))^2)
        df1 = (Npara - Npara_flat)
        df2 = (length(data_exp$GR) - Npara + 1)
        f_value = ((RSS1-RSS2)/df1)/(RSS2/df2)
        f_pval = pf(f_value, df1, df2, lower.tail = F)
        pval[i] = f_pval
        R_square[i] = 1 - RSS2/RSS1
      }
    }
    
    # Trapezoid rule for integration of GR_AOC
    GRavg = NULL
    for(j in 1:length(concs)) {
      data_trapz = data_exp[data_exp$concentration == concs[j],]
      GRavg[j] = mean(data_trapz$GR)
    }
    AOC[i] = sum((1 - (GRavg[1:(length(GRavg)-1)]+GRavg[2:length(GRavg)])/2)*diff(log10(concs), lag = 1))/(log10(concs[length(concs)]) - log10(concs[1]))
  }
  # Calculate GR50 from parameters
  parameters$GR50 = with(parameters, EC50*((1-GRinf)/(0.5-GRinf) - 1)^(1/Hill))
  parameters$`log10(GR50)` = log10(parameters$GR50)
  parameters$`log10(EC50)` = log10(parameters$EC50)
  parameters$GRmax = GRmax
  parameters$GR_AOC = AOC
  parameters = parameters[,c('GR50','GRmax','GR_AOC','EC50','GRinf','Hill','log10(GR50)','log10(EC50)')]
  parameters$`log2(Hill)` = log2(parameters$Hill)
  parameters$r2 = R_square
  if(is.null(pval)) {pval = NA}
  parameters$pval = pval
  # Re-order rows to match reference_output
  parameters$experiment = experiments
  pcutoff = .05 # Threshold for F-test pval
  parameters$fit = with(parameters, ifelse(pval >= .05 | is.na(EC50), "flat","sigmoid"))
  parameters$flat_fit = GR_mean
  if(!is.null(metadata)) {
    parameters = cbind(metadata, parameters)
  }
  # Make a new table with specified values added, all numbers formatted to 3 sig figs, etc.
  parameters_show = parameters
  ECcol = which(colnames(parameters_show) == 'EC50')
  colnames(parameters_show)[ECcol] = 'GEC50'
  # Add specified values for flat fits: GEC50 = 0, Hill = 0.01 and GR50 = +/- Inf
  for(i in 1:dim(parameters_show)[1]) {
    if(parameters_show$fit[i] == "flat") {
      parameters_show$GEC50[i] = 0
      parameters_show$Hill[i] = 0.01
      parameters_show$GR50[i] = ifelse(parameters_show$flat_fit[i] > .5, Inf, -Inf)
      parameters_show$GRinf[i] = parameters_show$flat_fit[i]
      parameters$GRinf[i] = parameters_show$flat_fit[i]
      parameters$pval[i] = pval[i]
      parameters_show$pval[i] = pval[i]
    }
  }
  # Add GR50 = +/-Inf for any curves that don't reach GR = 0.5
  for(i in 1:dim(parameters_show)[1]) {
    if(is.na(parameters_show$GR50[i])) {
      parameters_show$GR50[i] = ifelse(parameters_show$flat_fit[i] > .5, Inf, -Inf)
    }
  }
  delete_cols = which(colnames(parameters_show) %in% c('log2(Hill)','log10(GR50)', 'log10(EC50)','flat_fit'))
  parameters_show = parameters_show[,-delete_cols]
  parameters_show$GR50 = as.numeric(prettyNum(parameters_show$GR50, digits = 3))
  parameters_show$GRmax = as.numeric(prettyNum(parameters_show$GRmax, digits = 3))
  parameters_show$GR_AOC = as.numeric(prettyNum(parameters_show$GR_AOC, digits = 3))
  parameters_show$GEC50 = as.numeric(prettyNum(parameters_show$GEC50, digits = 3))
  parameters_show$GRinf = as.numeric(prettyNum(parameters_show$GRinf, digits = 3))
  parameters_show$Hill = as.numeric(prettyNum(parameters_show$Hill, digits = 3))
  parameters_show$r2 = as.numeric(prettyNum(parameters_show$r2, digits = 3))
  parameters_show$pval = as.numeric(prettyNum(parameters_show$pval, digits = 3))
  returnList = list(parameters, parameters_show)
  return(returnList)
}