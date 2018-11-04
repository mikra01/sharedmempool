import ospaths, strutils

task sharedmempool_tests, "run sharedmempool tests":
  withDir thisDir():
    switch("threads","on")
    switch("run")
    setCommand "c", "tests/sharedmempooltest.nim"
