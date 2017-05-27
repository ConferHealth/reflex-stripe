{ mkDerivation, base, containers, data-default, ghcjs-base, jsaddle
, lens, mtl, reflex-dom, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-stripe";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default ghcjs-base jsaddle lens mtl reflex-dom
    text time
  ];
  executableHaskellDepends = [
    base containers data-default ghcjs-base jsaddle lens mtl reflex-dom
    text time
  ];
  homepage = "https://github.com/ConferHealth/reflex-stripe#readme";
  description = "Reflex integration for the Stripe client library";
  license = stdenv.lib.licenses.bsd3;
}
