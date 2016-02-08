#' Representation of document sources
#'
#' This reference class characterizes the sources for a given document.
#' Documents are either generated from LaTeX- or Markdown-sources. The
#' necessary infomration that characterize the source of a certain document
#' are contained in this reference class.
DocumentSourceRefClass <- setRefClass(Class = "DocumentSourceRefClass",
                                      fields = list(
                                        sSourceFile   = "character",
                                        sSourcePath   = "character",
                                        sSourceFormat = "character",
                                        sEngine       = "character",
                                        sOutFormat    = "character",
                                        sTemplateFile = "character",
                                        sTemplatePath = "character"),
                                      methods = list(
                                        setSourceFile   = function(psSourceFile){sSourceFile <<- psSourceFile},
                                        getSourceFile   = function(){return(sSourceFile)},
                                        setSourcePath   = function(psSourcePath){sSourcePath <<- psSourcePath},
                                        getSourcePath   = function(){return(sSourcePath)},
                                        setSourceFormat = function(psSourceFormat){sSourceFormat <<- psSourceFormat},
                                        getSourceFormat = function(){return(sSourceFormat)},
                                        setEngine       = function(psEngine){sEngine <<- psEngine},
                                        getEngine       = function(){return(sEngine)},
                                        setOutFormat    = function(psOutFormat){sOutFormat <<- psOutFormat},
                                        getOutFormat    = function(){return(sOutFormat)},
                                        setTemplateFile = function(psTemplateFile){sTemplateFile <<- psTemplateFile},
                                        getTemplateFile = function(){return(sTemplateFile)},
                                        setTemplatePath = function(psTemplatePath){sTemplatePath <<- psTemplatePath},
                                        getTemplatePath = function(){return(sTemplatePath)}))

#' Common attributes of documents are grouped in this reference class
#'
#' Documents are either in latex or in markdown format. The common
#' characteristics of such documents are contained in this reference
#' class.
GenericDocumentRefClass <- setRefClass(Class = "GenericDocumentRefClass",
                                       fields = list(
                                         roSource   = "DocumentSourceRefClass",
                                         sTitle     = "character",
                                         sAuthor    = "character",
                                         dDate      = "Date",
                                         sEngine    = "character") ,
                                       methods = list(
                                         setSource  = function(proSource){roSource <<- proSource},
                                         getSource  = function(){return(roSource)},
                                         setTitle   = function(psTitle){sTitle <<- psTitle},
                                         getTitle   = function(){return(sTitle)},
                                         setAuthor  = function(psAuthor){sAuthor <<- psAuthor},
                                         getAuthor  = function(){return(sAuthor)},
                                         setDate    = function(pdDate){dDate <<- pdDate},
                                         getDate    = function(){return(dDate)},
                                         setEngine  = function(psEnginge){sEngine <<- psEnginge},
                                         getEngine  = function(){return(sEngine)}))

#' Reference Class CourseWebsiteRefClass implements a concrete representation of a website
#'
#' All attributes and methods of "GenericDocumentRefClass" are inherited. The main method
#' in this reference class is to initialize a new website
CourseWebsiteRefClass <- setRefClass(Class = "CourseWebsiteRefClass",
                                     fields = list(),
                                     contains = c("GenericDocumentRefClass"),
                                     methods = list(
                                       initWebSite = function(psTemplate, psPath){
                                         'Initialize a new website'
                                         # check whether template file and path are taken from
                                         #  arguments or from object fields
                                         if (is.null(psTemplate)){
                                           sTemplate <- roSource$sTemplateFile
                                         } else {
                                           sTemplate <- psTemplate
                                         }
                                         if (is.null(psPath)){
                                           sPath <- roSource$sTemplatePath
                                         } else {
                                           sPath <- psPath
                                         }

                                         vWebSiteTemplate <- readLines(file.path(sPath,sTemplate))

                                       }
                                     ))
