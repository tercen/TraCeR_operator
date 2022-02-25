library(tercen)
library(dplyr)
library(readr)
library(stringr)

ctx <- tercenCtx()


# Define input and output paths
input_path <- "/var/lib/tercen/share/write/trimmed_fastqs"

if( dir.exists(input_path) == FALSE) {

  stop("ERROR: trimmed_fastqs folder does not exist in project write folder.")

}

if (length(dir(input_path)) == 0) {
  stop("ERROR: trimmed_fastqs folder is empty.")
}

output_path <- "/var/lib/tercen/share/write/tracer_output"

r1_files <- list.files(input_path, "R1.fastq",
                       full.names = TRUE)

if (length(r1_files) == 0) stop("ERROR: No R1 FastQ files found in trimmed_fastqs folder.")


for (i in 1:length(r1_files)) {
  
  r1_file <- r1_files[[i]]
  r2_file <- str_replace(r1_file, "R1", "R2")

  sample_name <- str_split(basename(r1_file),
                         "_R1.fastq")[[1]][[1]]

  
  cmd = '/tracer/tracer'
  args = paste('assemble',
               '--ncores', parallel::detectCores(),
               '--config_file /tercen_tracer.conf',
               '-s Hsap',
               r1_file, r2_file,
               sample_name, output_path,
               sep = ' ')
  
  system2(cmd, args)
  
}


tibble(.ci = 1,
        n_cores_detected = parallel::detectCores()) %>%
  ctx$addNamespace() %>%
  ctx$save()
