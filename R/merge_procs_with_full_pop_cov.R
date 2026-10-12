#' Merge procedures with covariates of the full population
#'
#' This function joins the procedure information with covariate information.
#'
#' @param procedures_df Input procedures data frame. This is the output of create_ccsr_procedures where each row represents a unique participant id ("id") and each column represents a procedure
#' @param covariates_df Input covariates data frame. This is a data frame with at least one column representing the participant id ("id") and additional columns to be included as covariates in the regression.
#' @param append_cov_without_proc_as_controls Should individuals in the covariate data but not in the procedures data be appended as controls to the procedures data. Default is FALSE
#' @return Merged procedures and covariates data frame
#' @export

merge_procs_with_full_pop_cov <- function(procedures_df, covariates_df, append_cov_without_proc_as_controls = FALSE){

cov_but_no_proc_info_df <- covariates_df  %>% 
    anti_join(procedures_df, by = "id")  %>% 
    distinct(id)
    
message(glue::glue("{nrow(cov_but_no_proc_info_df)} rows in covariates data do not have a match in procedures data."))

    if (nrow(cov_but_no_proc_info_df) == 0) {
        proc_cov_df <- covariates_df  %>% 
            inner_join(procedures_df, by = "id")  %>% 
            mutate(across(where(is.logical), ~replace_na(., FALSE)))
    } else if (nrow(cov_but_no_proc_info_df) > 0) {
        if(append_cov_without_proc_as_controls == TRUE) {
            message("Appending these individuals as controls to the input data.")
            proc_cov_df <- covariates_df  %>% 
                left_join(procedures_df, by = "id")  %>% 
                mutate(across(where(is.logical), ~replace_na(., FALSE))) # if not in proc file, add FALSE for the proc codes 
        } else {
            message("These individuals will be excluded from the procedures data.")
            proc_cov_df <- covariates_df  %>% 
                inner_join(procedures_df, by = "id")  %>% 
                mutate(across(where(is.logical), ~replace_na(., FALSE))) 
        }
    }
    return(proc_cov_df)
}   