test_that("日期、术语和标识符转换符合预期", {
  template <- load_mapping_template()
  dm <- read_raw_csv(trace_path("data", "raw", "dm_raw.csv"))
  mappings <- template$domains$DM$mappings

  date_mapping <- mappings[[which(vapply(mappings, function(x) x$mapping_id == "DM005", logical(1)))]]
  sex_mapping <- mappings[[which(vapply(mappings, function(x) x$mapping_id == "DM009", logical(1)))]]
  id_mapping <- mappings[[which(vapply(mappings, function(x) x$mapping_id == "DM003", logical(1)))]]

  expect_equal(to_iso8601_date(dm, date_mapping)[1], "2025-01-05")
  expect_equal(map_controlled_term(dm, sex_mapping)[1:2], c("F", "M"))
  expect_equal(derive_usubjid(dm, id_mapping)[1], "TRACE001-701-1001")
})

test_that("无效日期和未知自定义转换被拒绝", {
  raw <- data.frame(DATE = "not-a-date", VALUE = "x")
  date_mapping <- list(mapping_id = "T001", source_variables = "DATE", parameters = list(formats = "y-m-d"))
  custom_mapping <- list(mapping_id = "T002", source_variables = "VALUE", parameters = list(name = "not_registered"))
  expect_error(to_iso8601_date(raw, date_mapping), "无法转换")
  expect_error(custom_transform(raw, custom_mapping), "未登记")
})

test_that("序号按受试者从一开始且可重复", {
  data <- tibble::tibble(
    STUDYID = c("S", "S", "S"),
    USUBJID = c("S-1", "S-1", "S-2"),
    AESTDTC = c("2025-01-02", "2025-01-01", "2025-01-03"),
    AETERM = c("B", "A", "C"),
    AEDECOD = c("B", "A", "C"),
    .SOURCE_ROW = 1:3
  )
  mapping <- list(
    mapping_id = "AESEQ",
    target_variable = "AESEQ",
    parameters = list(record_variables = c("STUDYID", "USUBJID", "AESTDTC", "AETERM", "AEDECOD", ".SOURCE_ROW"))
  )
  first <- derive_sequence(data, mapping)
  second <- derive_sequence(data, mapping)
  expect_identical(first, second)
  expect_equal(first$AESEQ[first$USUBJID == "S-1"], 1:2)
})

