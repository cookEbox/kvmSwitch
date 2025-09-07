{ mkDerivation, base, containers, directory, filepath, lib
, libgpiod, text, yaml
}:
mkDerivation {
  pname = "kvmSwitch";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers text yaml ];
  libraryPkgconfigDepends = [ libgpiod ];
  executableHaskellDepends = [
    base containers directory filepath yaml
  ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "kvmSwitch";
}
