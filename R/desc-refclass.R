## ----RCDescription-------------------------------------------------------
#' Reference class representing R package description objects
#'
#' The RCDesc reference class provides fields and methods that
#' can be used to read, to write and to extend DESCRIPTION files
#' of a package. The format of the DESCRIPTION files is explained
#' in the help files for \code{read.dcf}.
#'
RCDesc <- setRefClass(Class = "RCDesc",
                      fields = list(
                        title = "character",
                        version = "character",
                        author = "DcfAuthorRefClass",
                        maintainer = "DcfMaintainerRefClass",
                        description = "character",
                        depends = "character",
                        licence = "character",
                        LazyData = "character",
                        sDcfFileName = "character",
                        mExistingDescription = "matrix"
                      ),
                      methods = list(
                        # setters
                        setTitle = function(pstitle){ title <<- pstitle },
                        setVersion = function(psversion){ version <<- psversion },
                        setAuthor = function(poauthor){ author <<- poauthor },
                        setMaintainer = function(pomaintainer){maintainer <<- pomaintainer},
                        setDescription = function(psdescription){ description <<- psdescription },
                        setDepends = function(psdepends){ depends <<- psdepends },
                        setLicence = function(pslicence){ licence <<- pslicence },
                        setLazyData = function(psLazyData){ LazyData <<- psLazyData },
                        setSDcfFileName = function(pssDcfFileName){ sDcfFileName <<- pssDcfFileName },
                        # getters
                        getTitle = function() { return(title) },
                        getVersion = function() { return(version) },
                        getAuthor = function() { return(author) },
                        getMaintainer = function() { return(maintainer)},
                        getDescription = function() { return(description) },
                        getDepends = function() { return(depends) },
                        getLicence = function() { return(licence) },
                        getLazyData = function() { return(LazyData) },
                        getSDcfFileName = function() { return(sDcfFileName) },
                        # convenience methods for author
                        setGivenName = function(psgivenName){ author$setGivenName(psgivenName) },
                        setFamilyName = function(psfamilyName){ author$setFamilyName(psfamilyName) },
                        setEmailAddress = function(psemailAddress){ author$setEmailAddress(psemailAddress) },
                        setRole = function(psrole){ author$setRole(psrole) },
                        getGivenName = function() { return(author$getGivenName()) },
                        getFamilyName = function() { return(author$getFamilyName()) },
                        getEmailAddress = function() { return(author$getEmailAddress()) },
                        getRole = function() { return(author$getRole()) },
                        # show info
                        show = function() {
                          "Showing current conent of DCF object"
                          cat(" * Title:       ", title, "\n",
                              " * Version:     ", version, "\n",
                              " * Author:      ", author$toDcfString(), "\n",
                              " * Maintainer:  ", maintainer$toDcfString(), "\n",
                              " * Description: ", description, "\n",
                              " * Depends:     ", depends, "\n",
                              " * Licence:     ", licence, "\n",
                              " * LazyData:    ", LazyData, "\n",
                              " * DcfFile:     ", sDcfFileName, "\n"
                          )
                        },
                        # reading an existing description file
                        readDcf = function() {
                          "Reading a description file"
                          # check whether file exists
                          if(file.exists(sDcfFileName)) {
                            mExistingDescription <<- read.dcf(file = sDcfFileName)
                          } else {
                            cat(" * WARNING: Dcf file: ", sDcfFileName, " NOT FOUND\n")
                          }
                        },
                        # writing the info
                        writeDcf = function() {
                          "Write DCF information to a file"
                          write.dcf(mExistingDescription, file = sDcfFileName)
                        },
                        # add info to existing dcf
                        addToDcf = function() {
                          "Add DCF information to existing description"
                          if(!is.null(title))         mExistingDescription[1,"Title"] <<- title
                          if(length(version) > 0)     mExistingDescription[1,"Version"] <<- version
                          if(length(author) > 0)      mExistingDescription[1,"Author"] <<- author$toDcfString()
                          if(length(maintainer) > 0)  mExistingDescription[1,"Maintainer"] <<- maintainer$toDcfString()
                          if(length(description) > 0) mExistingDescription[1,"Description"] <<- description
                          if(length(depends) > 0)     mExistingDescription[1,"Depends"] <<- depends
                          if(length(licence) > 0)     mExistingDescription[1,"License"] <<- licence
                          if(length(LazyData) > 0)    mExistingDescription[1,"LazyData"] <<- LazyData
                        },
                        # combine read, add and write into one method
                        writeToExDcf = function() {
                          "read a description file, add existing information and write back to desc file"
                          readDcf()
                          addToDcf()
                          writeDcf()
                        }
                      ))

