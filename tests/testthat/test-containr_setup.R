

test_that("Setup dockerfile build ", {

  withr::with_tempdir({
  temp_dockerfile <- "docker/Dockerfile"

  docker_file <- dockerfile$new(name = "testing",
    rocker_image = "rstudio",
    dockerfile = "docker/Dockerfile",
    packages = "none",
    tag = "latest",
    include_python = FALSE,
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
  system("docker rmi testing")
},
  clean = TRUE)

})

test_that("Creates Dockerfiles and folders", {

  withr::with_tempdir({
    docker_file <- dockerfile$new(name = "testing",
      rocker_image = "rstudio",
      dockerfile = "docker/Dockerfile",
      packages = "loaded",
      tag = "latest",
      include_python = TRUE,
      build = FALSE)
    expect_true(fs::dir_exists("docker"))
    expect_true(fs::dir_exists("docker/scripts"))
    expect_true(fs::file_exists("docker/Dockerfile"))
    expect_true(fs::file_exists("docker/scripts/install_python.sh"))
    expect_true(fs::file_exists("docker/scripts/install_pyenv.sh"))
    expect_true(fs::file_exists("docker/scripts/install_additional.sh"))
  },
    clean = TRUE)
})


test_that("Set config flags for docker build", {

  withr::with_tempdir({
    docker_file <- dockerfile$new(name = "testing",
      rocker_image = "rstudio",
      dockerfile = "docker/Dockerfile",
      packages = "none",
      tag = "latest",
      include_python = FALSE,
      build = FALSE)

    expect_true(docker_file$set_python(TRUE))
    expect_true(docker_file$set_login(TRUE))
    expect_true(docker_file$set_local(TRUE))

  },
    clean = TRUE)
})


test_that("Docker commands return a tibble", {

  withr::with_tempdir({
    expect_type(docker_images(), "list")
    expect_s3_class(docker_images(), "tbl_df")
    expect_type(docker_search("rstudio"), "list")
    expect_type(docker_containers(), "list")

  },
    clean = TRUE)
})

test_that("Change containr settings", {

  withr::with_tempdir({
    docker_file <- dockerfile$new(name = "testing",
      rocker_image = "rstudio",
      dockerfile = "docker/Dockerfile",
      packages = "none",
      tag = "latest",
      include_python = FALSE,
      build = FALSE)

    expect_equal(docker_file$set_image("newname"), "newname")
    expect_equal(docker_file$set_name("projname"), "projname")

  },
    clean = TRUE)
})

test_that("Errors on non rocker image", {

  withr::with_tempdir({
    expect_error(dockerfile$new(name = "testing",
      rocker_image = "none",
      dockerfile = "docker/Dockerfile",
      packages = "none",
      tag = "latest",
      include_python = FALSE,
      build = FALSE))
  },
    clean = TRUE)
})
