# Function to check for slightly misspelled column names
check_col_names = function(col_names, expected) {
  check_cols = col_names[!col_names %in% expected]
  missing_cols = expected[!expected %in% col_names]
  n <- max(length(check_cols), length(missing_cols))
  length(check_cols) <- n                  
  length(missing_cols) <- n
  df = data.frame(missing = missing_cols, cols = check_cols, check.names = F)
  if(length(check_cols) > 0) {
    for(i in 1:length(check_cols)) {
      hits = agrep(check_cols[i], missing_cols, value = T)
      df$missing[i] = hits[1]
    }
  }
  df = df[!is.na(df$missing),]
  test_cols = NULL
  print(df)
  if(dim(df)[1] > 0) {
    for(i in 1:dim(df)[1]) {
      test_cols = c(test_cols, agrepl(df$missing[i], df$cols[i])[1])
    }
    df = df[test_cols,]
  }
  return(df)
}
