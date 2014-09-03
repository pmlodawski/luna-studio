{ stdenv, fetchurl, pkgconfig, zlib, ctl, ilmbase }:

stdenv.mkDerivation rec {
  name = "openexr-2.1.0";

  src = fetchurl {
    url = "mirror://savannah/openexr/${name}.tar.gz";
    sha256 = "054a47yr6l2hh5934xl1ydigxzhgfc7wz4lqmssxrhbk812nnj2l";
  };

  buildInputs = [ pkgconfig ];

  propagatedBuildInputs = [ zlib ilmbase ];

  configureFlags = "--enable-imfexamples";
}

