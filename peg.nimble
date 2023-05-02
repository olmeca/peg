# Package

version       = "0.1.0"
author        = "olmeca"
description   = "A tool for search and extraction on text files, based on PEG."
license       = "MIT"
srcDir        = "src"
bin           = @["peg"]


# Dependencies

requires "nim >= 1.6.10"
requires "grammarian >= 0.2.0"
