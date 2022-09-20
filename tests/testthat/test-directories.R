
# test_that("dockerfile class folders created", {
#
#   withr::with_dir(tempdir(),
#     {
#       dockerfile$new(name = "testing",
#         rocker_image = "rstudio",
#         dockerfile = "docker/Dockerfile",
#         packages = "loaded",
#         tag = "latest",
#         include_python = FALSE,
#         build = FALSE)
#       expect_true(fs::is_dir("docker"))
#       expect_true(fs::dir_exists("docker"))
#       expect_true(fs::dir_exists("docker/scripts"))
#       expect_true(fs::file_exists("docker/Dockerfile"))
#       expect_true(fs::file_exists("docker/scripts/install_libs_local.sh"))
#       expect_true(fs::file_exists("docker/scripts/install_python.sh"))
#       expect_true(fs::file_exists("docker/scripts/install_pyenv.sh"))
#       expect_true(fs::file_exists("docker/scripts/install_additional.sh"))
#     })
# })

test_that("Docker functions output tibbles", {

  withr::with_dir(tempdir(),
    {
      expect_type(docker_images(), "list")
      expect_type(docker_search("rstudio"), "list")
       expect_type(docker_containers(), "list")
    })
})
