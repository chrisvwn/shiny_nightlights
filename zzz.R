.onLoad <- function(libname, pkgname)
{
  ns <- getNamespace(pkgname);
  pkg <- Package(pkgname);
  assign(pkgname, pkg, envir=ns);
  
  # Setup the cache root path, possibly by prompting the user.
  setupRootPath();
}

.onAttach <- function(libname, pkgname)
{
  pkg <- get(pkgname, envir=getNamespace(pkgname));
  startupMessage(pkg);
}

.onLoad <- function(libname, pkgname)
{
  ns <- getNamespace(pkgname);
  pkg <- Package(pkgname);
  assign(pkgname, pkg, envir=ns);
  
  #cleanup by removing any global vars created etc
  cleanupNtLts();
}
