##Functions to clean and pre-process the data
load_data = function ()
{
  Performance <- read.csv("Student_performance_data _.csv")
  return (Performance)
}

remove_columns = function (df)
{
  df$StudentID = NULL
  df$GPA = NULL
  return(df)
}

introduce_success_column = function(df)
{
  df$Success = ifelse(df$GradeClass < 2, 1, 0)
  df$GradeClass = NULL
  return(df)
}

scale_columns = function(df)
{
  #If we add/remove other columns, then we need to change this
  df[,1:12] = scale(df[,1:12])
  return(df)
}

##Functions to return the different data sets depending on need
get_data = function()
{
  df = load_data()
  df = remove_columns(df)
  return (df)
}

get_data_with_success = function()
{
  df = load_data()
  df = remove_columns(df)
  df = introduce_success_column(df)
  return (df)
}

get_data_scaled = function()
{
  df = load_data()
  df = remove_columns(df)
  df = scale_columns(df)
  return (df)
}

get_data_scaled_with_success = function()
{
  df = load_data()
  df = remove_columns(df)
  df = introduce_success_column(df)
  df = scale_columns(df)
  return(df)
}