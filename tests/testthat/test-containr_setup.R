test_that("Setup dockerfile build ", {

  withr::with_tempdir({
  temp_dockerfile <- "containr/Dockerfile"

  docker_file <- containr$new(name = "testing",
    image = "rstudio",
    #dockerfile = "containr/Dockerfile",
    packages = "none",
    tag = "latest",
    build = FALSE)

  # overwrite with temp dockerfile for build
  docker_cmds <- paste(c(
    paste("FROM alpine"),
    paste(""),
    paste("CMD [\"/bin/sh\", \"-c\", \"echo \'It works!\'\"] "),
    paste("")),
    collapse = "\n")

  writeLines(docker_cmds, con = temp_dockerfile, sep = "")

  expect_false(docker_file$build)
  expect_message(docker_file$build_image(TRUE), "Success!")
  expect_equal(docker_file$status(), "Not Running")
  expect_true(docker_file$build)
  expect_equal(docker_file$name, "testing")
  expect_equal(docker_file$packages, "none")
  expect_equal(docker_file$image, "rocker/rstudio:latest")

  expect_false(docker_file$inc_py)
  expect_false(docker_file$inc_pyenv)
  expect_false(docker_file$inc_tensor)
  expect_false(docker_file$inc_geo)
  expect_false(docker_file$inc_quarto)
  expect_false(docker_file$inc_tex)
  expect_false(docker_file$inc_julia)
  expect_false(docker_file$inc_jupyter)
  expect_false(docker_file$inc_tidy)
  expect_false(docker_file$inc_verse)
  expect_false(docker_file$inc_pandoc)
  expect_false(docker_file$inc_shiny)

  # remove testing image
  system("docker rmi testing")
},
  clean = TRUE)

})

test_that("Creates Dockerfiles and folders", {

  withr::with_tempdir({
    docker_file <- containr$new(name = "testing",
      image = "rstudio",
      #dockerfile = "containr/Dockerfile",
      packages = "none",
      tag = "latest",
      build = FALSE)
    expect_true(fs::dir_exists("containr"))
    expect_true(fs::file_exists("containr/Dockerfile"))
  },
    clean = TRUE)
})

#
# test_that("Set config flags for docker build", {
#
#   withr::with_tempdir({
#     docker_file <- containr$new(name = "testing",
#       image = "rstudio",
#       dockerfile = "docker/Dockerfile",
#       packages = "none",
#       tag = "latest",
#       build = FALSE)
#
#     expect_true(docker_file$set_python(TRUE))
#     expect_true(docker_file$set_login(TRUE))
#     expect_true(docker_file$set_local(TRUE))
#
#   },
#     clean = TRUE)
# })


test_that("Docker commands work", {

  withr::with_tempdir({

    expect_type(docker$new(process = "docker",
      commands = "info")$show_output(),
      "list")

    expect_s3_class(docker$new(process = "docker",
      commands = "search",
      options = "rstudio")$show_output(),
      "tbl_df")

    expect_type(docker$new(process = "docker",
      commands = "info") $show_json(),
      "character")

  },
    clean = TRUE)
})

test_that("Change containr settings", {

  withr::with_tempdir({
    docker_file <- containr$new(name = "testing",
      image = "rstudio",
      #dockerfile = "containr/Dockerfile",
      packages = "none",
      tag = "latest",
      build = FALSE)

    expect_equal(docker_file$set_image("verse"), "rocker/verse:latest")
    expect_equal(docker_file$set_name("projname"), "projname")

  },
    clean = TRUE)
})

test_that("Errors on non rocker image", {

  withr::with_tempdir({
    expect_error(containr$new(name = "testing",
      image = "none",
      #dockerfile = "containr/Dockerfile",
      packages = "none",
      tag = "latest",
      build = FALSE))
  },
    clean = TRUE)
})
