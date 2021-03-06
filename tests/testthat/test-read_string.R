context("read_string")

test_that("Reading string", {
  library(mochiutils)
  x <- read_string(
"年  月 売上高   稼働全店数  稼働FC店数  既存店数  既存FC店数

 9  4  6666667   000         66          662       66
 9  5  6666667   662         66          662       66
 9  6  6666667   662         66          662       66
 9  7  6666667   662         66          662       66
 9  8  6666667   662         66          662       66
 9  9  6666667   662         66          662       66
 9 66  6666667   662         66          662       66
 9 66  6666667   662         66          662       66
 9 66  6666667   662         66          662       66
 66  1  6666667   662         66          662       66
 66  2  6666667   662         66          662       66
 66  3  6666667   662         66          662       66
 
 66  4  6666667   662         662         662       66
 66  5  6666667   662         662         662       66
 66  6  6666667   662         662         662       66
 66  7  6666667   662         662         662       66
 66  8  6666667   662         662         662       66
 66  9  6666667   662         662         662       66
 66  66 6666667   662         662         662       66
 66  66 6666667   662         662         662       66
 66  66 6666667   662         662         662       66
 66  1  6666667   662         662         662       66
 66  2  6666667   662         662         662       66
 66  3  6666667   662         662         662       66
")

  expect_equal(x[1,4],"000")
})