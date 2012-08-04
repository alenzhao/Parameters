setClass ('Parameters',
            representation(lists='list'),
            prototype(lists=list())
         )

setGeneric ('getAll', signature='obj', function (obj) standardGeneric ('getAll'))
setGeneric ('value',  signature='obj', function (obj, category, name) standardGeneric ('value'))
setGeneric('value<-', signature='obj', function (obj, category, name, value) standardGeneric("value<-"))

setMethod ('value',  'Parameters',
  function (obj, category, name) { 
    if (!category %in% names (obj@lists))
      return (NA)
    current.list = obj@lists [[category]]
    if (!name %in% names (current.list))
      return (NA)
    return (current.list [[name]])
    })

setReplaceMethod ('value',  'Parameters',
  function (obj, category, name, value) { 
    if (!category %in% names (obj@lists))
      obj@lists [[category]] = list ()
    obj@lists [[category]][[name]] = value
    obj
    })

setGeneric ('categories',  signature='obj', function (obj) standardGeneric ('categories'))
setMethod ('categories',  'Parameters',
  function (obj) { 
    names (obj@lists)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAll',  'Parameters',
  function (obj) { 
    obj@lists
    })
#------------------------------------------------------------------------------------------------------------------------
# 4 possible values for 'arg':
#   NULL:  return an empty but otherwise fully functional Parameters object
#   is.character:  a package name, with a 'defaultParams.yml' file in its extdata directory.  load those into obj and return obj
#   is.character:  a yaml filename containing configuration parameters.  load assignments from that yaml file, return obj
#   is.list:  a bunch of hand-specified parameters.  might be a list of lists, in standard yaml format.  load them all.
Parameters = function (arg=NULL)
{
  obj = new ('Parameters')

  if (is.null (arg))
    return (obj)

  if (is.character (arg)) {
       # is it an explicitly named file?
    if (file.exists (arg)) {
       obj@lists = yaml.load_file (arg)
       return (obj)
       }
      # was not a filename. is it a package name?  if so, look for extdata/defaultParams.yml
    packagePath = find.package (arg, quiet=TRUE)
    if (length (packagePath) == 0) {  # could not find package.  not an explicit file.  indicate failure, return NULL
      message (sprintf ('Parameters could not find file or installed package with name\n    %s', arg))
      return (NULL)
      }

    if (file.exists (packagePath)) {
      filename.in.installed.package <- file.path (packagePath, 'extdata', 'defaultParams.yml')
      if (file.exists (filename.in.installed.package)) {
        obj@lists = yaml.load_file (filename.in.installed.package)
        return (obj)
        }
     } # found installed package

    message (sprintf ('Parameters constructor failed to load "%s" as filename or package name', arg))
    } # is.character (arg)  

  if (is.list (arg)) {
    message ('list ctor for Parameters not implemented yet')
    }

  return (obj)

} # Parameters
#------------------------------------------------------------------------------------------------------------------------
