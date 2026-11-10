if (requireNamespace("tinytest", quietly = TRUE)) {
    # Routine state resolves under tools::R_user_dir() when the tests
    # don't override it, so point the three roots at the check's tempdir:
    # a check run must not read or write a real user's cerebro state.
    Sys.setenv(R_USER_CACHE_DIR = tempfile("cerebro_cache_"),
               R_USER_DATA_DIR = tempfile("cerebro_data_"),
               R_USER_CONFIG_DIR = tempfile("cerebro_config_"))
    tinytest::test_package("cerebro")
}
