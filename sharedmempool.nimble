# Package
version = "0.1.0"
author = "Michael Krauter"
description = "sharedmemorypool implementation in Nim"
license = "MIT"
skipDirs = @["tests"]

# Dependencies
requires "nim >= 0.19.0"

task test, "running tests":
  exec "nim sharedmempool_tests"