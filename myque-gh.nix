# Generated from myque-gh.cabal and parameterised over the filtered source.
{
  mkDerivation,
  lib,
  src,
  aeson,
  base,
  bytestring,
  containers,
  directory,
  filelock,
  filepath,
  hspec,
  git,
  myque,
  optparse-applicative,
  process,
  temporary,
  text,
  typed-process,
}:
mkDerivation {
  pname = "myque-gh";
  version = "0.1.0.0";
  inherit src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    containers
    directory
    filelock
    filepath
    myque
    optparse-applicative
    temporary
    text
    typed-process
  ];
  testHaskellDepends = [ aeson base bytestring containers directory filepath hspec myque process temporary text ];
  testToolDepends = [ git ];
  executableHaskellDepends = [ base containers myque process temporary text ];
  description = "Project canonical myque work items into GitHub";
  license = lib.licenses.bsd3;
  mainProgram = "myque-gh";
}
