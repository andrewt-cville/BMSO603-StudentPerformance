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

columns_to_factors = function (df)
{
  df$Gender = as.factor(df$Gender)
  df$Ethnicity = as.factor(df$Ethnicity)
  df$ParentalEducation = as.factor(df$ParentalEducation)
  df$Tutoring = as.factor(df$Tutoring)
  df$ParentalSupport = as.factor(df$ParentalSupport)
  df$Extracurricular = as.factor(df$Extracurricular)
  df$Sports = as.factor(df$Sports)
  df$Music = as.factor(df$Music)
  df$Volunteering = as.factor(df$Volunteering)
  df$GradeClass = as.factor(df$GradeClass)
  
  return(df)
}

factor_minus_gradeclass = function (df)
{
  df$Gender = as.factor(df$Gender)
  df$Ethnicity = as.factor(df$Ethnicity)
  df$ParentalEducation = as.factor(df$ParentalEducation)
  df$Tutoring = as.factor(df$Tutoring)
  df$ParentalSupport = as.factor(df$ParentalSupport)
  df$Extracurricular = as.factor(df$Extracurricular)
  df$Sports = as.factor(df$Sports)
  df$Music = as.factor(df$Music)
  df$Volunteering = as.factor(df$Volunteering)
  
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
  df = columns_to_factors(df)
  return (df)
}

get_data_with_success = function()
{
  df = load_data()
  df = remove_columns(df)
  df = factor_minus_gradeclass(df)
  df = introduce_success_column(df)
  df$Success = as.factor(df$Success)
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
  #df = factor_minus_gradeclass(df)
  df = introduce_success_column(df)
  df$Success = as.factor(df$Success)
  df = scale_columns(df)
  return(df)
}

