#...............................................................................
#                                                                              .
#  rocker image specific functions and wrappers                                .
#                                                                              .
#...............................................................................

#' Pull Rocker Images
#'
#' @param name Rocker image name from the [data_rocker_table]
#' @param tag Version tag of rocker image. Will default to `latest`
#'
#' @export
rocker_pull <- function(name, tag = NULL){

  # Check for image name
  data_rocker_table <- ContainR::data_rocker_table
  if(!(name %in% data_rocker_table$name)){
    cli::cli_warn("{.arg name} must be one of: {.emph {data_rocker_table$name}}")
    print(data_rocker_table)
  }

  # Set tag
  if(is.null(tag)){
    tag <- "latest"
  } else {
    tag <- tag
  }

  exec_sys_cmd <- switch(name,
                         rstudio = paste0("docker pull rocker/rstudio:", tag),
                         tidyerse = paste0("docker pull rocker/tidyverse:", tag),
                         verse = paste0("docker pull rocker/verse:", tag),
                         geospatial = paste0("docker pull rocker/geospatial:", tag),
                         binder = paste0("docker pull rocker/binder:", tag))

  system(exec_sys_cmd)
}

#
# rocker_run <- function(image = "testing",
#                        tag = NULL,
#                        DISABLE_AUTH = TRUE,
#                        use_local = TRUE){
#
#
#   # Container Name from active project
#   project_name = tolower(basename(rstudioapi::getActiveProject()))
#
#   data_rocker_table <- ContainR::data_rocker_table
#
#   # Set tag
#   if(is.null(tag)){
#     tag <- "latest"
#   } else {
#     tag <- tolower(tag)
#   }
#
#   image <- check_images(image = image, tag = tag)
#
#   processed_args <- rocker_args(DISABLE_AUTH, use_local, image)
#
#   docker_run(proc_args = processed_args, name = project_name)
#
#   # dockered <- processx::process$new(command = "docker",
#   #                                   args = proc_args,
#   #                                   pty = TRUE) # currently only supported on Unix systems
#   if(isTRUE(dockered$is_alive())){
#     cli::cli_h1("Running: {.val {image}} as container: {.val {project_name}}")
#     cli::cli_alert_info("Launching {.path {image}} in you browser...")
#     #utils::browseURL("http://localhost:8787")
#   }
#
# }

# TODO set a healthcheck on the container to check its running



 docker$new(process = "docker", command = "blah", options = "ls")
  docker$new(process = "docker", command = "image", options = "nope")
  docker$new(process = "docker", command = "image", options = "ls")
cont <- docker$new(process = "docker", command = "image", options = "ls")
cont
print(cont)

cont_obj <- containr$new(image = "testing", name = "ContainR", DISABLE_AUTH = TRUE,
  use_local = FALSE)

cont_obj
cont_obj$docker_check() # public is good
cont_obj$status()
cont_obj$info()

cont_obj$stop()
cont_obj$start()
cont_obj$info()
cont_obj$stop()
cont_obj$launch_browser()



# get pid
# docker inspect --format '{{.State.Pid}}' container

containr_name <- processx::run(command = "docker",
  args = c("inspect",
    "--format",
    "'{{.Name}}'",
    "ContainR"),
  error_on_status = TRUE)$stdout

containr_pid <- processx::run(command = "docker",
  args = c("inspect",
    "--format",
    "'{{.State.Pid}}'",
    "ContainR"),
  error_on_status = TRUE)$stdout

containr_image <- processx::run(command = "docker",
  args = c("inspect",
    "--format",
    "'{{.Config.Image}}'",
    "ContainR"),
  error_on_status = TRUE)$stdout



# additional outputs
processx::run(command = "docker",
  args = c("inspect",
    "--format",
    "'{{.State.Running}}'",
    "ContainR"),
  error_on_status = TRUE)$stdout

containr_ports <- processx::run(command = "docker",
  args = c("inspect",
    "--format",
    "'{{.Config.ExposedPorts}}'",
    "ContainR"),
  error_on_status = TRUE)$stdout




json <- processx::run(command = "docker",
  args = c("inspect",
    "--format",
    "'{{json .Mounts}}'",
    "ContainR"),
  error_on_status = TRUE)$stdout


cat(paste("<", cli::style_bold("Process:"), ps::ps_name(proc),
  "|", cli::style_bold("ID:"), "docker_id",
  "|", cli::style_bold("Port:"), "port",
  "|", cli::style_bold("Docker Port:"), "docker_port",
  "|", cli::style_bold("Pid:"), ps::ps_pid(proc),
  "|", cli::style_bold("Status:"), cli::col_green("status"), # ifelse red then green
  "|", cli::style_bold("Image:"), "image",
  ">"))

logged <- processx::run(command = "docker",
  args = c("logs",
    "--tail=5",
    "ContainR"),
  error_on_status = TRUE)$stdout

logged[grepl("\\[services.d\\]", logged)]












