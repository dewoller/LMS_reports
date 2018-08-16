


#!/usr/bin/env Rscript

source( "lib/functions_preload.R")
detach_all_packages()

safe_load( 'workflowr')
library(knitr)
library(sp)
suppressMessages(library(magrittr))
suppressMessages(library( rgeolocate))
suppressMessages(library( lubridate ))
library(tmap)
library(tmaptools)
suppressMessages(library(tidyverse))
library(ggpubr)

marks_root = '2018-HIM3HIB'
marks_root = '2018-HIM4IHS'


get_data = function( marks_root ) {
  logs_dir = paste0( getwd(), '/logs/' ) 

  log_root = paste0( 'logs_', marks_root )

  log_glob = paste0( log_root, '*.csv')
  log_re = paste0( '^', log_root, '.*\\.csv$')
  log_path = ifelse( length(Sys.glob( paste0( '/tmp/', log_glob)))!=0, 
                    '/tmp/', 
                    logs_dir 
                    )
  classlist_root = paste0( 'classlist_', marks_root )

  list.files( paste0( getwd(), '/logs/' ), paste0( '^', classlist_root, '.*\\.csv' )) %>% 
    tail(1 ) %>% 
    paste0( logs_dir, . ) %>% 
    read.csv () %>%
    set_names( names(.) %>% tolower() %>% str_replace_all(" ", "_")) %>%
    as.tibble() %>%
    filter( student_code != '' ) %>% 
    filter( display_subject_code != '' ) %>% 
    filter( display_subject_code != 'DISPLAY_SUBJECT_CODE' ) %>% 
    { . } -> classlist


  marks_glob = paste0( marks_root, '*.csv')
  marks_re = paste0( '^', marks_root, '.*\\.csv$')
  marks_path = ifelse( length(Sys.glob( paste0( '/tmp/', marks_glob)))!=0, 
                      '/tmp/', 
                      logs_dir 
                      )


  ip_location = 'data/ip_locations.csv'
  mmfile <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
  #

  list.files(log_path, log_re ) %>% 
    tail(1 ) %>% 
    { . } -> log_name

  #

  log_name %>%
    paste0(log_path, . ) %>% 
    { . } -> log_file_name

  log_file_name %>%
    read_csv() %>% 
    set_names( names(.) %>% tolower() %>% str_replace_all(" ", "_")) %>%
    mutate( time = as.Date( time, format='%d/%m/%y' )) %>% 
    mutate( mm_country=maxmind(ip_address, mmfile, "country_name")[[1]]) %>%
    mutate( semester_week = week( time ) - weeks_semester_start ) %>%
    { . } -> log
  system( paste0( 'mv "', log_file_name, '" logs/'))

  #
  list.files(marks_path, marks_re ) %>% 
    tail(1 ) %>% 
    paste0(marks_path, . ) %>% 
    { . } -> marks_file_name
  
  marks_file_name %>%
    read_csv() %>% 
    set_names( names(.) %>% 
              tolower() %>% 
              str_replace_all("[,–_& :()-]+", "_") %>%
              str_replace( '_$', '' )) %>%
    mutate( user_full_name = paste( first_name, surname )) %>%
    mutate( student_id_final_digit = last1( la_trobe_student_id )) %>%
    mutate_at(vars(ends_with('_real')),as.numeric) %>%
    mutate_at(vars(ends_with('_real_1')),as.numeric) %>%
    { . } -> marks
  #
  system( paste0( 'mv "', marks_file_name, '" logs/'))

  # get rid of pp who don't have marks
  log %>%
    count( user_full_name, sort = TRUE) -> loggers

  loggers %>% anti_join( marks ) -> losers
  log %<>% anti_join( losers )


  # get ip addresses
  #
  log %>% distinct( ip_address ) %>% 
    { . } -> all_ip 
  #
  if( file.exists( ip_location )) {
    df_ip_loc = read.csv( ip_location ) %>%
      as.tibble()
    all_ip %>% 
      anti_join( df_ip_loc ) %>% 
      { . } -> ip_to_get
  } else {
    ip_to_get = all_ip
  }

  # get ips that we do not have yet
  ip_to_get %$%
    ip_api(ip_address, delay=TRUE) %>% 
    { . } -> ip_got

  #
  ip_got %>% 
    as.tibble() %>%
    mutate( ip_address = ip_to_get$ip_address ) %>% 
    mutate( latitude = as.numeric( latitude)) %>%
    mutate( longitude = as.numeric( longitude)) %>%
    mutate( zip_code = as.numeric( zip_code)) %>%
    { . } -> ip_details

  #
  if( file.exists( ip_location )) {
    df_ip_loc %<>%
      bind_rows( ip_details ) 
  } else {
    df_ip_loc = ip_details 
  }
  #
  write.csv(  df_ip_loc, ip_location)
  #
  marks %<>% 
    rename( student_id = la_trobe_student_id ) %>%
    mutate( student_id = as.character( as.numeric(student_id) )) %>%
    inner_join( 
               
               classlist %>% 
               select( student_code, display_subject_code) %>%
               rename( student_id = student_code, 
                      subject_code = display_subject_code )
               
               , 
               by = 'student_id')
 
  log %<>% 
    inner_join( df_ip_loc, by='ip_address') %>%
    inner_join( marks %>% 
               select( user_full_name, student_id, student_id_final_digit, subject_code ), 
             by = 'user_full_name')
 #
  #
  log_name %>%
    substring( 6, 99) %>% 
    str_replace( '_.*', '' ) %>% 
    { . } -> class_name



  list( class_name = log_name, 
       log = log, 
       marks = marks 
  )


}


#-------------------------------------------------------------------------------------------
last1 = function( student_id ) {
  student_id %>%
    as.character() %>%
    substring(8,8)
}
#-------------------------------------------------------------------------------------------
last2 = function( student_id ) {
  student_id %>%
    as.character() %>%
    substring(7,8)
}
#-------------------------------------------------------------------------------------------


keep <- function(x, name) {assign(as.character(substitute(name)), x, pos = 1)}
#-------------------------------------------------------------------------------------------
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))


#-------------------------------------------------------------------------------------------
destring <- function(x,keep="0-9.-") {
  return( as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)) )
}


#-------------------------------------------------------------------------------------------
clipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}

#-------------------------------------------------------------------------------------------
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}


#-------------------------------------------------------------------------------------------
### Print a facet across multiple pages
#-------------------------------------------------------------------------------------------
print.gg_multiple <- function(x, page, ...) {
# Get total number of pages
  page_tot <- ggforce::n_pages(repair_facet(x))

# Get and check the page number to be drawn
    if (!missing(page)) {
      page_2_draw <- page
    } else {
      page_2_draw <- 1:page_tot
    }

# Prevent issue with repair_facet when page = NULL
  x$facet$params$page <- page_2_draw

# Begin multiple page ploting
    n_page_2_draw <- length(page_2_draw)

# Draw all pages
    for (p in seq_along(page_2_draw)) {
      x$facet$params$page <- page_2_draw[p]
        ggplot2:::print.ggplot(x = repair_facet(x), ...)

    }

# Prevent ggforce from droping multiple pages value
  x$facet$params$page <- page_2_draw
}
#-------------------------------------------------------------------------------------------

# Fix for ggforce facet_wrap_paginate
repair_facet <- function(x) {
  if (class(x$facet)[1] == 'FacetWrapPaginate' && 
      !'nrow' %in% names(x$facet$params)) {
    x$facet$params$nrow <- x$facet$params$max_row
  }
  x
}
#-------------------------------------------------------------------------------------------

page_print <- function(p )  {
  # Here we add our special class
  class(p) <- c('gg_multiple', class(p))

  # You can now print or save all pages
  print(p)
}




#-------------------------------------------------------------------------------------------
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
