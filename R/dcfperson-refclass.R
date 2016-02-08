## ----AuthorRefClass------------------------------------------------------
#' Abstract reference class for a person in a description file
#'
#' Reference objects from class \code{DcfPersonRefClass} can be used
#' for authors or maintainers
DcfPersonRefClass <- setRefClass(Class = "DcfPersonRefClass",
                                 fields = list(
                                   givenName = "character",
                                   familyName = "character",
                                   emailAddress = "character"
                                 ),
                                 methods = list(
                                   setGivenName    = function(psGivenName){givenName <<- psGivenName},
                                   getGivenName    = function(){return(givenName)},
                                   setFamilyName   = function(psFamilyName){familyName <<- psFamilyName},
                                   getFamilyName   = function(){return(familyName)},
                                   setEmailAddress = function(psEmailAddress){emailAddress <<- psEmailAddress},
                                   getEmailAddress = function(){return(emailAddress)}
                                 ))


#' Reference class to represent an author in a description file. This refclass is derived from DcfPersonRefClass
#'
#' @inheritParams from DcfPersonRefClass
DcfAuthorRefClass <- setRefClass(Class = "DcfAuthorRefClass",
                         fields = list(role = "character"),
                         contains = "DcfPersonRefClass",
                         methods = list(
                           setRole = function(psRole){role <<- psRole},
                           getRole = function(){return(role)},
                           addRole = function(psRole){
                             "adding a role to current list of roles"
                             role <<- c(role, psRole)
                           },
                           toDcfString = function(){
                             "writing author information to dcf-formatted string"
                             return(paste("\"", givenName, " ", familyName,
                                          " <", emailAddress, "> [",
                                          stringi::stri_flatten(role, collapse = ", "),
                                          "]\"", sep = "", collapse = ""))
                           }
                         ))


#' Reference Class to represent a maintainer in a dcf file, derived from DcfPersonRefClass
#'
DcfMaintainerRefClass <- setRefClass(Class = "DcfMaintainerRefClass",
                                     contains = "DcfPersonRefClass",
                                     methods = list(
                                       toDcfString = function(){
                                         "writing maintainer information to dcf-formatted string"
                                         return(paste0("\"", givenName, " ", familyName,
                                                      " <", emailAddress, ">\"", collapse = ""))
                                       }
                                     ))
