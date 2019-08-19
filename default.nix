{ mkDerivation, base, microlens, reflex, reflex-dom-core, stdenv
, text
}:
mkDerivation {
  pname = "reflex-firebase";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base microlens reflex reflex-dom-core text
  ];
  license = stdenv.lib.licenses.bsd3;
}
