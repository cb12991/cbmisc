#' Find, Load, or Create Objects
#' 
#' This function retrieves an object if it exists in the current environment,
#' loads the object if it is saved in the working directory, or constructs the 
#' object if the necessary function and function parameters are provided. 
#'
#' @param obj object, symbol; the object that is being accessed. 
#' @param dir character, string; the directory to look for the object if not 
#'   loaded; defaults to the current working directory. 
#' @param fun function, call; the function that constructs the object if not found 
#' @param save boolean; indicate whether to save the created object or not 
#' @param overwrite boolean; indicates whether to overwrite the object if found
#' @param ... arguments to pass on to the constructor function
#'
#' @return an object of the same type as input. If a list, will append new 
#'   element if needed
#'   
#' @import rlang 
#' @importFrom stringr str_wrap 
#' @importFrom glue glue   
#' @export
#'
#' @examples
find_load_make <- function(
  obj,
  dir = getwd(),
  fun = NULL,
  ...,
  save = TRUE,
  save_nm = NULL,
  overwrite = FALSE
) {
  if (!'pacman' %in% list.files(.Library)) install.packages('pacman')
  pacman::p_load(rlang, stringr, glue)
  
  # Get the name of the object.
  obj_nm <- deparse(substitute(obj, environment()))
  
  # Get the name of the function
  fun_nm <- ifelse(is.null(fun), NULL, deparse(substitute(fun, environment())))
  
  # Declare indicator variable which will run function or not.
  run_fn <- FALSE
  
  # If the object passed was actually a call to a list element, it will have 
  # both the list name and list element name, so we'll need to separate (not
  # set up to work with nested lists though).
  if (grepl('\\${2,}|\\[{3,}', obj_nm)) {
    stop(
      Sys.time(), '\nNested lists (i.e., index depth > 1) are currently not ',
      'supported.', 
      call. = FALSE
    )
  } else if (grepl('\\$|\\"', obj_nm)) {
    obj_ele <- gsub(
      '^[^\\"|\\$]*(\\"|\\$)|\\"[^\\"]*$',
      '', 
      obj_nm
    )
    obj_nm <- gsub('(\\$|\\[{1,2}).*$', "", obj_nm)
  } else {
    obj_ele <- NULL
  }
  
  # `x` will be the return variable. 
  if (
    (
      exists(obj_nm, envir = .GlobalEnv)
    ) && (
      (
        is.null(obj_ele) 
      ) || (
        !is.null(do.call(`[`, list(as.symbol(obj_nm), obj_ele))[1])
      )
    )
  ) {
    # Object is already loaded in the global environment and is either not a 
    # list or is a list with the object element also included. Grab from global
    # environment.
    if (is.null(obj_ele)) {
      x <- get(obj_nm, envir = .GlobalEnv)  
    } else {
      x <- do.call(`[`, list(as.symbol(obj_nm), obj_ele))
    }
    
    if (!overwrite) {
      # User did not indicate overwrite status, so just get from global 
      # environment.
      cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
      inform(
        message = c(
          'Already In Global Environment',
          i = str_wrap(
            glue(
              'Function provided but object `{obj_nm}`, element `{obj_ele}` 
               already loaded in global environment and overwrite parameter is 
               `FALSE`. Did not call function.'
            )
          )
        )
      )
    } else {
      # User did indicate overwrite status, so proceed to recreate object. 
      
      if (!is.null(fun)) {
        # Function provided, so flag indicator variable to recreate object. 
        run_fn <- TRUE
        
        cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
        inform(
          message = c(
            'Initializing Function',
            i = str_wrap(
              glue(
                'Object `{obj_nm}`, element `{obj_ele}` already in global 
                 environment, but overwrite parameter `TRUE`. Overwriting 
                 object.'
              )
            )
          )
        )
      } else {
        # Error out if no function provided.
        
        cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
        abort(
          'error_missing_function',
          message = c(
            'Cannot Overwrite',
            i = str_wrap(
              glue(
                'Cannot overwrite object `{obj_nm}` because no function
                 provided to recreate object.'
              )
            )
          ), 
          obj = obj_nm,
          overwrite = overwrite,
          fun_nm = fun_nm
        )
      }
    }
  } else {
    # Object or element not already loaded in global environment. Check working 
    # directory.
    
    # Get path by matching object name to full paths of all files in working 
    # directory.
    obj_path <- grep(
      pattern = paste0('/', obj_nm, '(?i)(\\.Rdata|\\.Rda|\\.rds)$'),
      x = list.files(dir, full.names = T, recursive = T), obj_nm,
      value = TRUE
    )
    
    # Proceed to loading object.
    
    if (length(obj_path) > 1) {
      # More than one object path found for object name, error out.
      
      cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
      abort(
        'error_duplicate_objects',
        message = c(
          'Duplicate Objects',
          i = str_wrap(
            glue(
              'Multilple objects with name \"{obj_nm}\" found in {dir}:'
            )
          ),
          set_names(obj_path, nm = rep('x', length(obj_path)))
        ),
        obj = obj_nm,
        overwrite = overwrite,
        dir = dir,
        obj_path = obj_path
      )
    } else if (length(obj_path) == 1) {
      # A distinct path was found for object. Load object with appropriate 
      # function (differs between .Rdata or .rds files).
      if (grepl('\\.(?i)(rds)$', obj_path)) {
        x <- readRDS(obj_path)
      } else if (grepl('\\.(?i)(Rda|Rdata)$', obj_path)) {
        x <- get(load(obj_path))
      }
      
      # Now the object is loaded and assigned to `x`. If `x` is a list, we need
      # to see if the element is part of that list.
      
      if (!is.null(obj_ele)) {
        # An element name was declared, so the object is a list.
        if (is.null(do.call('[', list(x, obj_ele)))) {
          # The element is not found in the list. Proceed to flag indicator 
          # variable to create, depending on if function provided.
          if (!is.null(fun)) {
            # Constructor was provided. Set indicator variable to TRUE.
            run_fn <- TRUE 
            cat('\n'); cat(crayon::silver(Sys.time()))
            inform(
              message = c(
                'Initializing Function',
                i = str_wrap(
                  glue(
                    'Object `{obj_nm}` found but element `{obj_ele}` 
                     missing. Initializing function to create element.'
                  )
                )
              )
            )
          } else {
            # Constructor was not provided, so error out.
            cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
            abort(
              'error_missing_function',
              message = c(
                'Cannot Create',
                x = str_wrap(
                  glue(
                    'Object `{obj_nm}` found with element `{obj_ele}` 
                     missing, but no function provided to create element.'
                  )
                )
              ), 
              obj = obj_nm,
              overwrite = overwrite,
              fun_nm = fun_nm
            )
          }
        } else if (overwrite) {
          # Element is found in list. Overwrite argument passed as TRUE, so flag
          # indicator variable to recreate, depending on if function provided.
          if (!is.null(fun)) {
            # Constructor was provided. Set indicator variable to TRUE.
            run_fn <- TRUE 
            
            cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
            inform(
              message = c(
                'Initializing Function',
                i = str_wrap(
                  glue(
                    'Object `{obj_nm}` found in working directory with element
                     `{obj_ele}` included. Overwrite indicated, so initializing 
                     function to recreate element.'
                  )
                )
              )
            )
          } else {
            # Constructor was not provided, so throw warning but return loaded 
            # object.
            cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
            warn(
              'warning_missing_function',
              message = c(
                'Cannot Rereate',
                i = str_wrap(
                  glue(
                    'Object `{obj_nm}` found with element `{obj_ele}` 
                     included. Overwrite indicated, but no function provided to 
                     recreate. Returning loaded object.' 
                  )
                )
              ), 
              obj_nm = obj_nm,
              obj_ele = obj_ele,
              overwrite = overwrite,
              fun_nm = fun_nm
            )
          }
        } else if (!is.null(fun)) {
          # Element is found in list, function provided, but overwrite argument
          # passed as FALSE. Do not run function to construct.
          cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
          inform(
            message = c(
              'Loaded from Working Directory',
              i = str_wrap(
                glue(
                  'Function provided but object `{obj_nm}`, element 
                   `{obj_ele}` found in working directory with overwrite 
                   parameter `FALSE`. Did not call function.'
                )
              )
            )
          )
        }
      } else if (overwrite) {
        # Object loaded from working directory, object element name not declared, 
        # (thus is not a list), overwrite argument passed as TRUE. Flag indicator
        # variable to recreate, depending on if function provided.
        
        if (!is.null(fun)) {
          # Constructor provided, set indicator variable to TRUE.
          run_fn <- TRUE
          
          cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
          inform(
            message = c(
              'Initializing Function',
              i = str_wrap(
                glue(
                  'Object `{obj_nm}` found in working directory and loaded.
                   Overwrite indicated, so initializing function to recreate 
                   object.'
                )
              )
            )
          )
        } else {
          # Constructor was not provided, so throw warning but return loaded object.
          cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
          warn(
            'warning_missing_function',
            message = c(
              'Did Not Create',
              i = str_wrap(
                glue(
                  'Did not create object `{obj_nm}` because no function 
                   provided.'
                )
              )
            ), 
            obj_nm = obj_nm,
            overwrite = overwrite,
            fun_nm = fun_nm,
            obj_path = obj_path
          )
        }
      } else if (!is.null(fun)) {
        # Function provided, but did not indicate overwrite and element is 
        # already in list. Do not run function to construct.
        cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
        inform(
          message = c(
            'Loaded from Working Directory',
            i = str_wrap(
              glue(
                'Function provided but object `{obj_nm}` found in working 
                 directory with overwrite parameter `FALSE`. Did not call 
                 function.'
              )
            )
          )
        )
      }
    } else if (!is.null(fun_nm)) {
      # Object path equals a zero-length character vector, thus object was not
      # found in working directory. Will create if function was provided.
      cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
      inform(
        message = c(
          'Initializing Function',
          i = str_wrap(
            glue(
              'Object `{obj_nm}` not found in working directory. 
               Initializing function to create object.'
            )
          )
        )
      )
      run_fn <- TRUE
    } else {
      # Error out if no function provided.
      cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
      abort(
        'error_missing_function',
        message = c(
          'Cannot Create',
          i = str_wrap(
            glue(
              'Cannot create object `{obj_nm}` because no function provided.'
            )
          )
        ), 
        obj_nm = obj_nm,
        overwrite = overwrite,
        fun_nm = fun_nm,
        obj_path = obj_path
      )
    }
  }
  # Create and save new object/element if needed.
  if (run_fn) {
    start_tm <- Sys.time()
    args <- enexprs(...)
    
    if (!is.null(obj_ele)) {
      x[[obj_ele]] <- do.call(fun_nm, args) 
    } else {
      x <-  do.call(fun_nm, args)
    }
    if (save) saveRDS(x, file = paste0(dir, save_nm %||% obj_nm, '.rds'))
    runtime <- Sys.time() - start_tm
    cat('\n'); cat(crayon::silver(Sys.time())); cat('\n')
    inform(
      message = c(
        'Complete!',
        i = str_wrap(
          glue(
            'Time elapsed:  {round(as.numeric(runtime), digits = 3)} 
             {attr(runtime, "units")}'
          )
        )
      )
    )
  }
  
  # Assign object/element to global environment.
  assign(obj_nm, x, envir = .GlobalEnv)
  return(invisible(x))
}