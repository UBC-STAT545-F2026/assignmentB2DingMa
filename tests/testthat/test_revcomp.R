simple_seq <- "Atggccggcaacgtgaagaagagctctggg"
test_that("Default arguments return the upper case complement", {
  expect_equal(complement_dna(simple_seq), "TACCGGCCGTTGCACTTCTTCTCGAGACCC")
})
test_that("Setting reverse to TRUE and upper to FALSE returns reverse complement in lower case", {
  expect_equal(complement_dna(simple_seq, reverse = TRUE, upper = FALSE), "cccagagctcttcttcacgttgccggccat")
})

messy_seq <- "1    Atggccggca acgtgaagaa gagctctggg gccgggggcg gcagcggctc cgggggctcg
61   ggttcgggtG Gcctgattgg gctcatgaag gacgccttcc Agccgcacca Ccaccaccac
121  caccacctca gcccccaccc gccggggacg gtggacaaga agatggtgga gaagtgctgg
"

# these tests are grouped together since the idea behind them is the same, but the the output is different enough to warrant multiple tests. Both warnings and outputs are checked.
test_that("Inputting seq with invalid characters shows a warning and evaluates seq with invalid characters stripped", {
  expect_warning(complement_dna(messy_seq))
  suppressWarnings({
    expect_equal(complement_dna(messy_seq), "TACCGGCCGTTGCACTTCTTCTCGAGACCCCGGCCCCCGCCGTCGCCGAGGCCCCCGAGCCCAAGCCCACCGGACTAACCCGAGTACTTCCTGCGGAAGGTCGGCGTGGTGGTGGTGGTGGTGGTGGAGTCGGGGGTGGGCGGCCCCTGCCACCTGTTCTTCTACCACCTCTTCACGACC")
  })

  expect_warning(complement_dna("Hello World"))
  suppressWarnings({
    expect_equal(complement_dna("Hello World"), "")
  })

  expect_warning(complement_dna("The importance of warnings"))
  suppressWarnings({
    expect_equal(complement_dna("The importance of warnings"), "AATGTC")
  })
})

test_that("Inputting a non-character seq returns an error", {
  expect_error(complement_dna(123))
})

test_that("Inputting a vector with length >1 outputs a warning and evaluates only the first element", {
  expect_warning(complement_dna(c("Atggccggca", "acgtgaagaa")))
  suppressWarnings({
    expect_equal(complement_dna(c("Atggccggca", "acgtgaagaa")), "TACCGGCCGT")
  })
})
