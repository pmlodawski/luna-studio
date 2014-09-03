{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
    name = "ilmbase-2.1.0";

    src = fetchurl {
        url = "mirror://savannah/openexr/${name}.tar.gz";
        sha256 = "1d0rd3y4xjhcbvsdxslr1zzfjbyjka8fc9d10w5r8qapkkc1m10y";
    };
}
