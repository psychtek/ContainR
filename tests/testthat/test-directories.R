# # Test structure for baseline environment
# test_that("Creates emptry dir and retursn path", {
# temp_proj_dir <- dynutils::safe_tempdir("project")
# usethis::local_project(temp_proj_dir, force = TRUE, setwd = TRUE)
# })

test_that("Docker folders created", {
  temp_proj_dir <- dynutils::safe_tempdir("project")
  usethis::local_project(temp_proj_dir, force = TRUE, setwd = TRUE)

  docker_folder_setup(print_tree = FALSE)
  expect_true(fs::is_dir(temp_proj_dir))
  expect_true(fs::dir_exists("docker"))
  expect_true(fs::dir_exists("docker/scripts"))
})


test_that("Warn when no scripts directory exists", {
  expect_warning(get_extdata("install_additional.sh"))
  expect_warning(get_extdata("install_python.sh"))
  expect_warning(get_extdata("install_additional.sh"))
})

test_that("Scripts are created and in correct place", {
  temp_proj_dir <- dynutils::safe_tempdir("project")
  usethis::local_project(temp_proj_dir, force = TRUE, setwd = TRUE)

  docker_folder_setup(print_tree = FALSE)
  expect_true(fs::file_exists("docker/Dockerfile"))
  expect_true(fs::file_exists("docker/scripts/install_libs_local.sh"))
  expect_true(fs::file_exists("docker/scripts/install_python.sh"))
  expect_true(fs::file_exists("docker/scripts/install_pyenv.sh"))
  expect_true(fs::file_exists("docker/scripts/install_additional.sh"))
})

# Config files
test_that("Temp config file", {
  temp_proj_dir <- dynutils::safe_tempdir("project")
  usethis::local_project(temp_proj_dir, force = TRUE, setwd = TRUE)

  file_obj <- create_config_file(project_name = "test")
  expect_true(fs::file_exists(file_obj))

})


test_that("Libs local scripts in the right place", {
  temp_proj_dir <- dynutils::safe_tempdir("project")
  usethis::local_project(temp_proj_dir, force = TRUE, setwd = TRUE)

  docker_folder_setup(print_tree = FALSE)

  packages <- sessioninfo::package_info()
  bash_file <- write_install_bash_file(packages)

  link_string <- paste0(fs::path_wd(), "/docker/scripts/install_libs_local.sh")
  created_file <- as.character(fs::path_wd(bash_file))

  expect_true(fs::file_exists(bash_file))
  expect_equal(created_file, link_string)

})

test_that("Error testing `docker_file` ", {
  temp_proj_dir <- dynutils::safe_tempdir("project")
  usethis::local_project(temp_proj_dir, force = TRUE, setwd = TRUE)

  expect_error(docker_file(dockerfile = "docker/Dockerfile",
                           which_pkgs = "xxx",
                           name = "tidyverse",
                           include_python = TRUE))

  expect_error(docker_file(dockerfile = "docker/Dockerfile",
                           which_pkgs = "none",
                           name = "some_name",
                           include_python = TRUE))

  expect_message(docker_file(dockerfile = "docker/Dockerfile",
                             which_pkgs = "loaded",
                             name = "tidyverse",
                             include_python = TRUE))

})

test_that("Docker Images returns a list", {
  temp_proj_dir <- dynutils::safe_tempdir("project")
  usethis::local_project(temp_proj_dir, force = TRUE, setwd = TRUE)

  dock_images <- docker_images()
  expect_type(dock_images, "list")

})
