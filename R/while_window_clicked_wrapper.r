#' Execute a Function While Waiting for a GUI Click
#'
#' This function creates a graphical user interface (GUI) window with a canvas widget
#' that waits for a mouse click event. While waiting, it executes a specified function
#' in parallel multiple times based on user-defined parameters. The results are collected
#' and returned after the window is clicked or a maximum number of iterations is reached.
#'
#' Parallel_envir_setup() must be called before this function to set up the parallel environment and
#' to load the necessary packages and helper functions.
#'
#' @param OUT_GEN_FUN A function that generates an output dataframe. This function should
#'                    take an argument `n` and return a dataframe with at least `n` rows.
#' @param FUN A function to be applied to each row of the dataframe generated by
#'            `OUT_GEN_FUN`. It should process a dataframe and return the processed dataframe.
#' @param iter_per_core Numeric. The number of iterations per core. Default is 1.
#' @param packages Character vector. The names of packages that should be loaded in each
#'                  parallel worker. Default is `c("dplyr")`.
#' @param max_iters Numeric. The maximum number of iterations allowed before stopping.
#'                  Default is 10,000,000. The function will stop if this limit is reached
#'                  or if the GUI canvas is clicked.
#'
#' @details
#' The function creates a Tkinter GUI window with a canvas. It binds a click event on
#' the canvas to a variable that signals when the canvas has been clicked. In a loop,
#' it generates data using `OUT_GEN_FUN`, processes the data using `FUN` in parallel,
#' and appends the results to a cumulative dataframe. The loop continues until either
#' the canvas is clicked or the maximum number of iterations is reached. The function
#' then closes the GUI window and returns the accumulated results.
#'
#' @return A dataframe containing the results of applying `FUN` to the data generated
#'         by `OUT_GEN_FUN` over multiple iterations.
#'
#' @examples
#' # Example functions
#' generate_data <- function(n) {
#'   data.frame(x = rnorm(n), y = rnorm(n))
#' }
#'
#' process_data <- function(df) {
#'   df %>% dplyr::mutate(z = x + y)
#' }
#'
#' # Run the wrapper function
#' results <- While_window_clicked_wrapper(
#'   OUT_GEN_FUN = generate_data,
#'   FUN = process_data,
#'   iter_per_core = 2,
#'   packages = c("dplyr"),
#'   max_iters = 10000
#' )
#'
#' @export
While_window_clicked_wrapper <- function(OUT_GEN_FUN, FUN,
                                         iter_per_core = 1,
                                         packages = c("dplyr"),
                                         max_iters = Inf) {


  # Initialize variable which breaks loop if TRUE
  buttonClicked <- FALSE
  assign("buttonClicked", buttonClicked, envir = .GlobalEnv)

  # Create main window
  win <- tktoplevel()

  # Create a canvas widget
  canvas <- tkcanvas(win, width = 200, height = 200)
  tkpack(canvas)

  # Bind a left mouse button click on the canvas to the onClick function
  tkbind(canvas, "<Button-1>", onClick)
  results_binded <- OUT_GEN_FUN(n = 1) %>% .[0,] # empty dataframe


  while (!buttonClicked) {

    # Number of iterations
    no_iter <- n.cores * 3
    # Create a dataframe to be filled with results
    out <- OUT_GEN_FUN(n = no_iter * iter_per_core)

    # Split the dataframe into parts, get the lines appropriate
    borders <- floor(seq(1, nrow(out), length.out = no_iter + 1))

    results <- foreach(
      i = 1:no_iter,
      .verbose = TRUE,
      .combine = 'rbind',
      .export = c("out", "FUN"),
      .errorhandling = 'pass',
      .inorder = FALSE,
      .packages = packages
    ) %dopar% {

      out_act <- out[borders[i]:borders[i + 1],]

      out_act[1:nrow(out_act), ] <- FUN(out_act[1:nrow(out_act), ])

      return(out_act)
    }

    results_binded <- bind_rows(results_binded, results)

    # Check the status of buttonClicked
    if (get("buttonClicked", envir = globalenv())) {
      print("The canvas was clicked.")
      break
    } else if (nrow(results_binded) > max_iters) {
      print("Max. iterations reached.")
      break
    }

  }

  # Destroy the window
  tkdestroy(win)

  return(results_binded)
}
