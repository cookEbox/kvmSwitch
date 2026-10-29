{ lib
, stdenv
, fetchFromGitHub
, cmake
}:

stdenv.mkDerivation {
  pname = "rpi-ws281x";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "jgarff";
    repo = "rpi_ws281x";
    rev = "v1.0.0";
    hash = "sha256-Push5DMNNoTUHudMZxI7OYhaKSqbckWj5v1Jnf0ltms=";
  };

  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    "-DBUILD_SHARED=OFF"
    "-DBUILD_TEST=OFF"
  ];

  postInstall = ''
    install -Dm444 "$src/LICENSE" "$out/share/licenses/rpi-ws281x/LICENSE"
  '';

  meta = {
    description = "Userspace Raspberry Pi library for WS281X LEDs";
    homepage = "https://github.com/jgarff/rpi_ws281x";
    license = lib.licenses.bsd2;
    platforms = lib.platforms.linux;
  };
}
