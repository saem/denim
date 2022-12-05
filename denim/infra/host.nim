## provide an interface for denim to the compiler host, allowing for platform
## agnostic "file system" and other such access. This should eventually allow
## for running in the browser for example.

import
  std/uri

type
  DenimHost* = object
    discard

proc initHost*(): DenimHost =
  discard

proc readFile(host: DenimHost, file: Uri): string =
  discard