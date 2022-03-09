library(tercen)
library(dplyr)
library(readr)
library(stringr)
library(progressr)
library("future.apply")

# plan(multisession) # works in R studio
no_cores <- availableCores() - 1
plan(multicore, workers = no_cores) ## don't work on R studio

handler_tercen <- function(ctx, ...) {

  env <- new.env()
  assign("ctx", ctx, envir = env)

  reporter <- local({
    list(
      update = function(config, state, progression, ...) {
        evt = TaskProgressEvent$new()
        evt$taskId = ctx$task$id
        evt$total = config$max_steps
        evt$actual = state$step
        evt$message = paste0(state$message , " : ",  state$step, "/", config$max_steps)
        ctx$client$eventService$sendChannel(ctx$task$channelId, evt)
      }
    )
  }, envir = env)

  progressr::make_progression_handler("tercen", reporter, intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "gui", interval = 1, ...)
}


ctx <- tercenCtx()

options(progressr.enable = TRUE)
progressr::handlers(handler_tercen(ctx))


# Define input and output paths
input_path <- "/var/lib/tercen/share/write/trimmed_fastqs"

if( dir.exists(input_path) == FALSE) {

  stop("ERROR: trimmed_fastqs folder does not exist in project write folder.")

}

if (length(dir(input_path)) == 0) {
  stop("ERROR: trimmed_fastqs folder is empty.")
}

output_path <- "/var/lib/tercen/share/write/tracer_output"

r1_files <- list.files(input_path, "_R1.*q.gz$",
                       full.names = TRUE)

if (length(r1_files) == 0) stop("ERROR: No R1 FastQ files found in trimmed_fastqs folder.")


samples = progressr::with_progress({
  progress = progressr::progressor(along = r1_files)
  tracer = function(r1_file) {
    r2_file <- str_replace(r1_file, "_R1", "_R2")
    r2_file <- str_replace(r2_file, "_val_1", "_val_2")

    sample_name <- str_split(basename(r1_file),
                             "_R1")[[1]][[1]]

  cmd = '/tracer/tracer'
  args = paste('assemble',
               '--ncores', parallel::detectCores(),
               '--config_file /tercen_tracer.conf',
               '-s Hsap',
               r1_file, r2_file,
               sample_name, output_path,
               sep = ' ')

    exitCode =   system(paste(cmd, args), ignore.stdout = TRUE, ignore.stderr = TRUE)

    if (exitCode != 0) {
      stop("ERROR: tracer failed for sample: ", sample_name)
    }

     progress("TraCeR")

     sample_name

     }

 future_lapply(r1_files, FUN=tracer)

})


tibble(.ci = 1,
        n_cores_detected = parallel::detectCores()) %>%
  ctx$addNamespace() %>%
  ctx$save()
