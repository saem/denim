## idiomatic code and common definitions

const DenimFileExt* = "dnm"

func invalidModuleName*(name: string): bool =
  ## `true` if `name` is not a valid module name, otherwise false
  false # TODO implement me