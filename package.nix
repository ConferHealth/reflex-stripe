{ mkDerivation, base, data-default, jsaddle, lens, mtl, reflex-dom
, stdenv, text
}:
mkDerivation {
  pname = "reflex-stripe";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base data-default jsaddle lens mtl reflex-dom text
  ];
  homepage = "https://github.com/ConferHealth/reflex-stripe#readme";
  description = "Reflex integration for the Stripe client library";
  license = stdenv.lib.licenses.bsd3;
}
