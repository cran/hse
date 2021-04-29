.onAttach <- function(lib, pkg) {
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        packageStartupMessage(paste(pkg, ver))
        msg <- paste("\n     This package, \"hse\" is now deprecated.   Users",
                     "\n     should install and use its successor \"dbd\".\n")
        packageStartupMessage(msg)
}
