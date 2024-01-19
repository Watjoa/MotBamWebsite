# ---
#  title: Correlation table in APA format
#  author: Joachim Waterschoot
#  date: May, 2018
# ---

##########################################################################
##################### Correlation table in APA format ####################
##########################################################################

#-------- We need three things: --------#

# 1. In which directory/folder can we find your data file?
data_folder <- "/Users/joachimwaterschoot/Desktop"        # ex. windows: Z:\analyses\data
                            # ex. mac: /Users/joachimwaterschoot/Downloads

# 2. What is the (full) name of your data file?
name_file <- "Data_MAIN_groepfrussat.sav"    # bv. "MP_Laure.sav"


# 3. Would you like to see confidence intervals in your table?
confint <- TRUE    # change this by FALSE when you don't want to see them.

variables <- c('Resilience','Puzzel_CF','Puzzel_CS','index_competentie')

##########################################################################
############################## CTRL+A and RUN ############################
##########################################################################




txt.number <- function (number.in) 
{
  number.out <- sprintf("%1.2f", number.in)
}

txt.r<-function (ctest) 
{
  r.value = ctest$estimate
  p.value = ctest$p.value
  r.value.txt <- strip.leading.zero(sprintf("%1.2f", r.value))
  r.value.txt <- add.sig.stars(r.value.txt, p.value)
  string.out = sprintf("%s", r.value.txt)
  return(string.out)
}

strip.leading.zero <- function (string.in) 
{
  string.out = string.in
  id.r.is.one <- string.in == "1.00"
  id.r.is.mone <- string.in == "-1.00"
  string.out <- sub(pattern = "0.", replacement = ".", x = string.in)
  string.out[id.r.is.one] <- "1.00"
  string.out[id.r.is.mone] <- "-1.00"
  return(string.out)
}

add.sig.stars <-function (string.in, p.values.in) 
{
  string.out <- string.in
  L <- length(p.values.in)
  for (i in 1:L) {
    cur.p.value <- p.values.in[i]
    if (!is.na(cur.p.value)) {
      if ((cur.p.value < 0.05) & (cur.p.value > 0.01)) {
        string.out[i] <- paste(string.in[i], "*", sep = "")
      }
      else if (cur.p.value < 0.01) {
        string.out[i] <- paste(string.in[i], "**", sep = "")
      }
    }
  }
  return(string.out)
}

txt.ci<-function (cortest.result, strip_zero = TRUE) 
{
  ci.interval <- cortest.result$conf.int
  ci.lower <- ci.interval[1]
  ci.upper <- ci.interval[2]
  output <- txt.ci.brackets(ci.lower, ci.upper, strip_zero = strip_zero)
  return(output)
}

txt.ci.brackets<-function (LL, UL, strip_zero = TRUE) 
{
  ci.lower.txt <- sprintf("%1.2f", LL)
  ci.upper.txt <- sprintf("%1.2f", UL)
  if (strip_zero == TRUE) {
    ci.lower.txt <- strip.leading.zero(ci.lower.txt)
    ci.upper.txt <- strip.leading.zero(ci.upper.txt)
  }
  ci.txt <- sprintf("[%s, %s]", ci.lower.txt, ci.upper.txt)
  return(ci.txt)
}

RtfTable <- apaTables:::RtfTable







write.rtf.table <- function (filename, txt.body, landscape = FALSE, paper = "us", 
          table.title = NA, table.note = NA, table.number = NA) 
{
  doc.type <- list()
  doc.type$uslandscape <- "\\paperw15840 \\paperh12240 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
  doc.type$usportrait <- ""
  doc.type$a4landscape <- "\\paperw16834 \\paperh11909 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
  doc.type$a4portrait <- ""
  if (!any(paper == c("us", "a4"))) {
    paper <- "us"
  }
  if (landscape == TRUE) {
    orientation <- "landscape"
  }
  else {
    orientation <- "portrait"
  }
  
  table.number.str <- ""
  if (is.na(table.number)) {
    table.number.str <- "XX"
  }
  else {
    table.number <- round(table.number)
    table.number.str <- sprintf("%1.0f", table.number)
  }
  
  doc.spec <- paste(paper, orientation, sep = "")
  txt.format <- doc.type[[doc.spec]]
  txt.start <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times New Roman;}}"
  txt.end <- "}"
  blank.line <- c("{\\pard  \\par}")
  number.line <- sprintf("{\\pard Table %s \\par}", table.number.str)
  if (is.na(table.title)) {
    title.line <- sprintf("{\\pard\\i Table title goes here \\par}")
  }
  else {
    title.line <- sprintf("{\\pard\\i %s\\par}", table.title)
  }
  if (is.na(table.note)) {
    note.line <- sprintf("{\\i Table note goes here}")
  }
  else {
    note.line <- sprintf("{\\pard \\par}{\\pard{\\i Note.} %s\\par}", 
                         table.note)
  }
    txt.body <- c(number.line, blank.line, title.line, blank.line, 
                            txt.body, note.line)
    
 file.id <- pipe("pbcopy", "w") ## HIER BRANDT DE LAMP
    writeLines(txt.start, file.id)
    if (landscape == TRUE) {
   writeLines(txt.format, file.id)
  }
  length.body <- length(txt.body)
  for (i in 1:length.body) {
    writeLines(txt.body[i], file.id)
  }
  writeLines(txt.end, file.id)
  close(file.id)
 
}





apacor.table <- function (data, filename = NA, table.number = NA, show.conf.interval = TRUE, 
          landscape = TRUE) 
{
  data <- data
  table_number <- table.number
  if (show.conf.interval == FALSE) {
    cat("The ability to suppress reporting of reporting confidence intervals has been deprecated in this version.
        \nThe function argument show.conf.interval will be removed in a later version.\n")
  }
  show.conf.interval = TRUE
  show_conf_interval <- show.conf.interval
  if (is.na(filename)) {
    make_file_flag <- FALSE
  }
  else {
    make_file_flag <- TRUE
  }
  df_col <- dim(data)[2]
  column_is_numeric <- c()
  for (i in 1:df_col) {
    column_is_numeric[i] <- is.numeric(data[, i])
  }
  data <- data[, column_is_numeric]
  number_variables <- ncol(data)
  number_columns <- number_variables - 1
  output_cor <- matrix(" ", number_variables, number_columns)
  output_cor_rtf <- matrix(" ", number_variables, number_columns)
  output_ci <- matrix(" ", number_variables, number_columns)
  output_ci_rtf <- matrix(" ", number_variables, number_columns)
  output_descriptives <- matrix(" ", number_variables, 2)
  output_variable_names <- paste(as.character(1:number_variables), 
                                 ". ", names(data), sep = "")
  for (i in 1:number_variables) {
    output_descriptives[i, 1] <- txt.number(mean(data[, i], 
                                                 na.rm = TRUE))
    output_descriptives[i, 2] <- txt.number(sd(data[, i], 
                                               na.rm = TRUE))
    for (j in 1:number_variables) {
      if ((j < i)) {
        x <- data[, i]
        y <- data[, j]
        ctest <- cor.test(x, y)
        cor_string <- txt.r(ctest)
        output_cor[i, j] <- cor_string
        output_cor_rtf[i, j] <- cor_string
        cor_ci_string <- txt.ci(ctest)
        output_ci[i, j] <- cor_ci_string
        output_ci_rtf[i, j] <- paste("{\\fs20", cor_ci_string, 
                                     "}", sep = "")
      }
    }
  }
  left_padding <- c(" ", " ", " ")
  first_line <- c(output_variable_names[1], output_descriptives[1, 
                                                                ], output_cor[1, ])
  first_line_rtf <- c(output_variable_names[1], output_descriptives[1, 
                                                                    ], output_cor_rtf[1, ])
  second_line <- c(left_padding, output_ci[1, ])
  second_line_rtf <- c(left_padding, output_ci_rtf[1, ])
  third_line <- rep(" ", length(second_line))
  output_matrix_console <- rbind(first_line, second_line)
  output_matrix_rtf <- rbind(first_line_rtf, second_line_rtf)
  for (i in 2:number_variables) {
    first_line <- c(output_variable_names[i], output_descriptives[i, 
                                                                  ], output_cor[i, ])
    first_line_rtf <- c(output_variable_names[i], output_descriptives[i, 
                                                                      ], output_cor_rtf[i, ])
    second_line <- c(left_padding, output_ci[i, ])
    second_line_rtf <- c(left_padding, output_ci_rtf[i, ])
    third_line <- rep(" ", length(second_line))
    if (show_conf_interval == TRUE) {
      new_lines <- rbind(first_line, second_line, third_line)
      new_lines <- rbind(first_line, second_line, third_line)
      new_lines_rtf <- rbind(first_line_rtf, second_line_rtf, 
                             third_line)
    }
    else {
      new_lines <- rbind(first_line, third_line)
      new_lines_rtf <- rbind(first_line_rtf, third_line)
    }
    output_matrix_console <- rbind(output_matrix_console, 
                                   new_lines)
    output_matrix_rtf <- rbind(output_matrix_rtf, new_lines_rtf)
  }
  rownames(output_matrix_console) <- 1:nrow(output_matrix_console)
  colnames(output_matrix_console) <- c(c("Variable", "M", "SD"), 
                                       as.character(1:number_columns))
  rownames(output_matrix_rtf) <- rownames(output_matrix_console)
  colnames(output_matrix_rtf) <- colnames(output_matrix_console)
  if (show_conf_interval == TRUE) {
    table_title <- "Means, standard deviations, and correlations with confidence intervals\n"
  }
  else {
    table_title <- "Means, standard deviations, and correlations\n"
  }
  row_with_colnames <- colnames(output_matrix_console)
  df_temp <- data.frame(output_matrix_console, stringsAsFactors = FALSE)
  rownames(output_matrix_console) <- rep(" ", length((rownames(output_matrix_console))))
  table_body <- output_matrix_console
  if (show_conf_interval == TRUE) {
    table_note <- "Note. M and SD are used to represent mean and standard deviation, respectively.\nValues in square brackets indicate the 95% confidence interval.\nThe confidence interval is a plausible range of population correlations \nthat could have caused the sample correlation (Cumming, 2014).\n* indicates p < .05. ** indicates p < .01.\n"
  }
  else {
    table_note <- "Note. M and SD are used to represent mean and standard deviation, respectively.\n* indicates p < .05. ** indicates p < .01.\n"
  }
  tbl.console <- list(table.number = table_number, table.title = table_title, 
                      table.body = table_body, table.note = table_note)
  class(tbl.console) <- "apa.table"
  if (make_file_flag == TRUE) {
    colnames(output_matrix_rtf) <- c(c("Variable", "{\\i M}", 
                                       "{\\i SD}"), as.character(1:number_columns))
    number_columns <- dim(output_matrix_rtf)[2]
    blankLine <- rep("", number_columns)
    output_matrix_rtf <- rbind(blankLine, output_matrix_rtf)
    if (show_conf_interval == TRUE) {
      table_title <- "Means, standard deviations, and correlations with confidence intervals"
      table_note <- "{\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation. The confidence interval is a plausible range of population correlations that could have caused the sample correlation (Cumming, 2014). * indicates {\\i p} < .05. ** indicates {\\i p} < .01."
    }
    else {
      table_title <- "Means, standard deviations, and correlations"
      table_note <- "{\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. * indicates {\\i p} < .05. ** indicates {\\i p} < .01. "
    }
   rtfTable <- RtfTable$new(isHeaderRow = TRUE)
   rtfTable$setTableContent(output_matrix_rtf)
   rtfTable$setRowFirstColumnJustification("left")
    txt_body <- rtfTable$getTableAsRTF(FALSE, FALSE)
     write.rtf.table(filename = filename, txt.body = txt_body, 
    table.title = table_title, table.note = table_note, 
    landscape = landscape, table.number = table_number)
  }
  return(tbl.console)
}

setwd(data_folder)
data <- read.spss(name_file, to.data.frame=TRUE)
data <- data[,variables]
apacor.table(data,filename='CaviR.APAcorrelatiematrix.doc', table.number=1,show.conf.interval=confint,landscape=TRUE)

