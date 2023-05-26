#
#
#           PEG Utility
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
## =====================================================
## Rudi Angela's adaptation of the Nim Grep Utility. https://nim-lang.org/

## Copyright (C) 2006-2017 Andreas Rumpf. All rights reserved.

## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:

## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.

## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
## THE SOFTWARE.

## [ MIT license: http://www.opensource.org/licenses/mit-license.php ]

import
  os, strutils, parseopt, pegs, terminal, sequtils, logging, streams, grammarian

const
  Version = "0.2"
  Usage = "peg - PEG Utility Version " & Version & """

  (c) 2017 Rudi Angela
Usage:
  peg [options] [pattern] [replacement] (target file/directory)*
Options:
  --find, -f          find the pattern (default), as opposed to 'modify'.
  --peg -p            the literal PEG expression to use for matching
  --pegfile, -P       pattern argument is the path to the file containing the PEG.
                      In case of a relative path three possible containing
                      directories are searched:
                        - The current directory
                        - If defined, the path indicated by $PEGS_DIR environment
                          variable.
                        - If present, the subdirectory named 'pegs' of the directory
                          containing the peg binary.
  --symbols, -s       comma separated list of key-value pairs, where the keys
                      refer to symbols in the pegfile that will be replaced
                      by the corresponding values prior to compiling the PEG
  --replace, -r       replace the pattern by pattern specified in this expression
  --replacefile, -R   replace the pattern by pattern specified in this file
  --modify, -m        modify the file in place
  --stdin             instructs peg to take its input from stdin instead of a file.
  --input, -i         specifies a literal input value for processing
                      instead of an input file. Mainly for testing.
  --line, -l          Line by line processing, as opposed to processing the
                      contents of the file as a whole.
  --recursive         process directories recursively
  --ignorecase, -c    be case insensitive
  --name, -n          when target is a directory, only search the files with the
                      given name (including extension)
  --ext:EX1|EX2|...   when target is a directory, only search the files with the 
                      given extension(s)
  --verbose           be verbose: list every processed file
  --help, -h          shows this help
  --version, -v       shows the version

This is an adaptation of the original nimgrep tool that is distributed as
part of the Nim language compiler.
"""

const
  NIL = ""
  cPegsDirKey = "PEGS_DIR"

type
  TOption = enum
    optFind, optReplace, optPeg, optRecursive, optConfirm, optStdin,
    optWord, optIgnoreCase, optIgnoreStyle, optVerbose, optFilePeg, optModify, optLiteral, optByLine
  TOptions = set[TOption]
  TConfirmEnum = enum
    ceAbort, ceYes, ceAll, ceNo, ceNone
  TKeyValuePair = (string, string)
  KeyValueError = object of ValueError
  FileNotFound = object of ValueError

let
  cMatchItemSeparator = "---"
  cPegsDir = "pegs"
  cKeyValuePairPattern = peg"""
  Pattern <- ^ Pair !.
  Pair <- {Name} '=' {Value}
  Name <- \ident
  Value <- .+
  """

var
  filenames: seq[string] = @[]
  pattern = NIL
  patternVariant = NIL
  patternArguments: seq[string] = @[]
  captureTargets: seq[string] = @[]
  rootPatternName = "Pattern"
  replacement = NIL
  replacementFile: string = NIL
  extensions: seq[string] = @[]
  options: TOptions = {optPeg}
  useWriteStyled = true
  argName = NIL
  argPatternFile: string = NIL
  pegParamString: string = NIL
  gProcessingSingleFile = true
  lineSeparator = "\l"
  argLiteral = NIL

proc enableLogging() =
   let filePath = joinPath(getAppDir(), "peg.log")
   var fileLogger = newFileLogger(filePath, fmtStr = verboseFmtStr)
   addHandler(fileLogger)
   setLogFilter(lvlDebug)

proc readKeyValuePair(source: string): TKeyValuePair =
  if source =~ cKeyValuePairPattern:
    let symbol = "$" & "{$#}" % [matches[0]]
    let value = matches[1]
    result = (symbol, value)
  else:
    raise newException(KeyValueError, "Invalid value specification: '$#'" % [source])

proc ask(msg: string): string =
  stdout.write(msg)
  stdout.flushFile()
  result = stdin.readLine()

proc confirm: TConfirmEnum =
  while true:
    case normalize(ask("     [a]bort; [y]es, a[l]l, [n]o, non[e]: "))
    of "a", "abort": return ceAbort
    of "y", "yes": return ceYes
    of "l", "all": return ceAll
    of "n", "no": return ceNo
    of "e", "none": return ceNone
    else: discard

proc countLines(s: string, first, last: int): int =
  var i = first
  while i <= last:
    if s[i] == '\13':
      inc result
      if i < last and s[i+1] == '\10': inc(i)
    elif s[i] == '\10':
      inc result
    inc i

proc beforePattern(s: string, first: int): int =
  result = first-1
  while result >= 0:
    if s[result] in NewLines: break
    dec(result)
  inc(result)

proc afterPattern(s: string, last: int): int =
  result = last+1
  while result < s.len:
    if s[result] in NewLines: break
    inc(result)
  dec(result)

proc writeColored(s: string) =
  if useWriteStyled:
    terminal.writeStyled(s, {styleUnderscore, styleBright})
  else:
    stdout.write(s)

proc highlight(source, match, repl: string, range: tuple[first, last: int],
               line: int, showRepl: bool) =
  const alignment = 6
  debug("highlight called")
  #stdout.write(line.`$`.align(alignment), ": ")
  var x = beforePattern(source, range.first)
  var y = afterPattern(source, range.last)
  for i in x .. range.first-1: stdout.write(source[i])
  writeColored(match)
  for i in range.last+1 .. y: stdout.write(source[i])
  stdout.write("\n")
  stdout.flushFile()
  if showRepl:
    stdout.write(spaces(alignment-1), "-> ")
    for i in x .. range.first-1: stdout.write(source[i])
    writeColored(repl)
    for i in range.last+1 .. y: stdout.write(source[i])
    stdout.write("\n")
    stdout.flushFile()

proc determineLineSeparator(source: string): string =
  if source.contains('\r'): "\r\l" else: "\l"


proc writeResultToFile(result: string, filename: string) =
    var f: File
    if open(f, filename, fmWrite):
        f.write(result)
        f.close()
    else:
        raise newException(IOError, "cannot open file for overwriting: " & filename)


proc processString(source: string): string =
  var pegp: Peg
  var output: string

  let grammar = newGrammar(pattern)
  let pegstring = pegString(grammar, rootPatternName, captureTargets, patternVariant, patternArguments)
  pegp = peg(pegstring)

  if optReplace in options:
    output = newStringOfCap(source.len)

  var lineNr = 1
  var indexUnprocessed = 0
  # Create and initialize a match results array
  var matches: array[0..MaxSubpatterns-1, string]
  for j in 0..high(matches): matches[j] = ""

  while indexUnprocessed < source.len:
        var matchedBounds: tuple[first, last: int]
        matchedBounds = findBounds(source, pegp, matches, indexUnprocessed)
        if matchedBounds.first < 0: break
        inc(lineNr, countLines(source, indexUnprocessed, matchedBounds.first - 1))

        var wholeMatch = source.substr(matchedBounds.first, matchedBounds.last)

        if optReplace in options:
          # Perform replacement
          let updatedMatchedString = replace(wholeMatch, pegp, replacement % matches)

          # Send result to output
          if optModify in options:
            output.add(source.substr(indexUnprocessed, matchedBounds.first - 1))
            output.add(updatedMatchedString)
          else:
            stdout.write(updatedMatchedString)
            stdout.write(lineSeparator)
        else:
          # Just send found match to output
          stdout.write(wholeMatch)
          # Append a separator line
          # stdout.write(lineSeparator & cMatchItemSeparator & lineSeparator)
          stdout.flushFile()

        inc(lineNr, countLines(source, matchedBounds.first, matchedBounds.last))
        indexUnprocessed = matchedBounds.last + 1
  stdout.write(lineSeparator)
  stdout.flushFile()

  if {optReplace, optModify} <= options:
        output.add(substr(source, indexUnprocessed))

  result = output

proc processStdin() =
  let input = newFileStream(stdin).readAll()
  discard processString(input)

proc processFileByLine(filename: string) =
    let input = newFileStream(filename, fmRead)
    var line: string = NIL
    while input.readLine(line):
        echo processString(line)
    input.close()


proc processFile(filename: string) =
  var filenameShown = gProcessingSingleFile

  template printFileName =
    if not filenameShown and optVerbose notin options:
      stdout.write(filename & lineSeparator)
      stdout.flushFile()
      filenameShown = true

  if optVerbose in options:
    stdout.writeLine(filename)
    stdout.flushFile()

  var buffer: string
  try:
    buffer = system.readFile(filename)
  except IOError:
    echo "cannot open file: ", filename
    return

  printFileName()
  let output = processString(buffer)
  # If applicable, write output to file
  if {optReplace, optModify} <= options:
        writeResultToFile(output, filename)


proc hasRightExt(filename: string, exts: seq[string]): bool =
  var y = splitFile(filename).ext.substr(1) # skip leading '.'
  for x in items(exts):
    if os.cmpPaths(x, y) == 0: return true

proc styleInsensitive(s: string): string =
  template addx =
    result.add(s[i])
    inc(i)
  result = ""
  var i = 0
  var brackets = 0
  while i < s.len:
    case s[i]
    of 'A'..'Z', 'a'..'z', '0'..'9':
      addx()
      if brackets == 0: result.add("_?")
    of '_':
      addx()
      result.add('?')
    of '[':
      addx()
      inc(brackets)
    of ']':
      addx()
      if brackets > 0: dec(brackets)
    of '?':
      addx()
      if s[i] == '<':
        addx()
        while s[i] != '>' and s[i] != '\0': addx()
    of '\\':
      addx()
      if s[i] in strutils.Digits:
        while s[i] in strutils.Digits: addx()
      else:
        addx()
    else: addx()

proc walker(dir: string) =
  for kind, path in walkDir(dir):
    case kind
    of pcFile:
      var file = splitFile(path)
      if extensions.len > 0:
        if path.hasRightExt(extensions): processFile(path)
        else: discard
      elif argName.len > 0:
        # only if filename matches the value of the 'name' argument
        if os.cmpPaths(file.name & file.ext, argName) == 0: processFile(path)
        else: discard
      else:
        processFile(path)
    of pcDir:
      if optRecursive in options:
        walker(path)
    else: discard

proc writeHelp() =
  stdout.write(Usage)
  stdout.flushFile()
  quit(0)

proc writeVersion() =
  stdout.write(Version & "\n")
  stdout.flushFile()
  quit(0)

proc checkExclusiveOptions(subset: TOptions, a, b: string) =
  if subset <= options:
    quit("cannot specify both '$#' and '$#'" % [a, b])

proc checkInclusiveOptions(subset: TOptions, a, b: string) =
  if not (subset <= options):
    quit("You must specify both '$#' and '$#'" % [a, b])


proc processFileNode(fileNodePath: string) =
  if dirExists(fileNodePath):
    gProcessingSingleFile = false
    walker(fileNodePath)
  elif fileExists(fileNodePath):
    processFile(fileNodePath)
  else:
    echo "$# not found." % [fileNodePath]


proc processFiles() =
  gProcessingSingleFile = len(filenames) == 1
  for f in items(filenames):
      processFileNode(f)


proc processFilesByLine() =
  gProcessingSingleFile = len(filenames) < 2
  for f in items(filenames):
      processFileByLine(f)


proc processLiteral() =
  echo processString(argLiteral)

proc stdPegsDir(): string =
  joinPath(getAppDir(), cPegsDir)

proc readPatternFile(filePath: string): string =
  assert(filePath != NIL)
  debug("readPatternFile: '$#'" % [filePath])
  if fileExists(absolutePath(filePath)):
    result = system.readFile(absolutePath(filePath))
  else:
    let pegsDir = getEnv(cPegsDirKey, stdPegsDir())
    let stdPatFile = joinPath(pegsDir, filePath)
    debug("readPatternFile: '$#'" % [stdPatFile])
    if fileExists stdPatFile:
      result = system.readFile(stdPatFile)
    else:
      raise newException(FileNotFound, "File not found: '$#'" % [filePath])


proc applyOptions(pattern: string): string =
  if optIgnoreStyle in options:
    result = "\\y " & pattern
  elif optIgnoreCase in options:
    result = "\\i " & pattern
  else:
    result = pattern


debug("Start.")

for kind, key, val in getopt():
  case kind
  of cmdLongoption, cmdShortOption:
    case key
    of "find", "f": incl(options, optFind)
    of "name", "n":
      argName = val
    of "symbols", "s":
      pegParamString = val
    of "targets", "t":
      captureTargets = val.split(",")
    of "variant", "v":
      patternVariant = val
    of "arguments", "a":
      patternArguments = val.split(",")
    of "replace", "r":
      incl(options, optReplace)
      replacement = val
      debug("global: opt r: '$#'" % [replacement])
    of "input", "i":
      incl(options, optLiteral)
      argLiteral = val
      debug("global: opt l: '$#'" % [val])
    of "replacementfile", "R":
      incl(options, optReplace)
      replacementFile = val
      debug("global: opt R: " & replacementFile)
    of "peg", "p":
      excl(options, optFilePeg)
      incl(options, optPeg)
      pattern = val
    of "pegfile", "P":
      incl(options, optFilePeg)
      argPatternFile = val
      excl(options, optPeg)
    of "recursive": incl(options, optRecursive)
    of "modify": incl(options, optModify)
    of "confirm": incl(options, optConfirm)
    of "stdin": incl(options, optStdin)
    of "word", "w": incl(options, optWord)
    of "ignorecase", "c": incl(options, optIgnoreCase)
    of "line", "l": incl(options, optByLine)
    of "ignorestyle", "y": incl(options, optIgnoreStyle)
    of "ext": extensions.add val.split('|')
    of "nocolor": useWriteStyled = false
    of "verbose": incl(options, optVerbose)
    of "help", "h": writeHelp()
    of "debug", "d":
      enableLogging()
    else: writeHelp()
  of cmdArgument:
    if options.contains(optStdin):
      echo "You may not specify any file names if option --stdin is given."
    else:
     debug("global: arg: " & key)
     filenames.add(key)
  of cmdEnd: assert(false) # cannot happen

debug("global: done processing options")

when defined(posix):
  useWriteStyled = terminal.isatty(stdout)

checkExclusiveOptions({optFind, optReplace}, "find", "replace")
checkExclusiveOptions({optFilePeg, optPeg}, "pegfile", "peg")
checkExclusiveOptions({optIgnoreCase, optIgnoreStyle}, "ignore_case", "ignore_style")


if replacementFile != NIL:
  replacement = system.readFile(replacementFile)
  debug("global: replacement: " & replacement)

# Either a filename or a literal value must have been specified
if filenames.len == 0 and optLiteral notin options and optStdin notin options:
  writeHelp()
  quit()

# If the following also applies to optFilePeg then use intersection (*)
# if {optPeg, optFilePeg} * options != {}:
if optFilePeg in options:
  pattern = readPatternFile(argPatternFile)

if pegParamString != NIL:
  let kvSpecs = pegParamString.split(",")
  let kvPairs = kvSpecs.map(readKeyValuePair)
  pattern = multiReplace(pattern, kvPairs)

if optLiteral in options:
  processLiteral()
elif optStdin in options:
  processStdin()
else:
  if optByLine in options:
    processFilesByLine()
  else:
    processFiles()
