rm(list = ls())
devtools::load_all()
rCourseDesc <- RCDesc$new()
rCourseDesc$setAuthor(DcfAuthorRefClass$new())
rCourseDesc$setGivenName("Peter")
rCourseDesc$setFamilyName("von Rohr")
rCourseDesc$setEmailAddress("peter.vonrohr@gmail.com")
rCourseDesc$setRole(c("aut","cre"))

dcfMaintRefObj <- DcfMaintainerRefClass$new()
dcfMaintRefObj$setGivenName("Peter")
dcfMaintRefObj$setFamilyName("von Rohr")
dcfMaintRefObj$setEmailAddress("peter.vonrohr@gmail.com")
rCourseDesc$setMaintainer(dcfMaintRefObj)

rCourseDesc$setLicence("LGPL-3")
rCourseDesc$setTitle("An Introduction To R")
rCourseDesc$setDescription("Introductory course to the programming language R")
sCoursePath <- "."
rCourseDesc$setSDcfFileName(pssDcfFileName = file.path(sCoursePath, "DESCRIPTION"))
rCourseDesc$show()
rCourseDesc$writeToExDcf()

# test
curDesc <- read.dcf(file = rCourseDesc$sDcfFileName)
curDesc
