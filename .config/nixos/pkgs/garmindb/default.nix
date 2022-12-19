{ pkgs ? import <nixpkgs> }:


with pkgs;
buildPythonPackage rec {
  pname = "GarminDB";
  version = "3.2.6";

  src = fetchPypi {
    inherit pname version;
    sha256 = lib.fakeSha256;
  };

  meta = with stdenv.lib; {
    description = "Download and parse data from Garmin Connect or a Garmin watch, FitBit CSV, and MS Health CSV files into and analyze data in Sqlite serverless databases with Jupyter notebooks.";
    homepage = "https://github.com/tcgoetz/GarminDB";
    maintainers = with maintainers; [ jdbaldry ];
    license = licenses.gpl2;
  };
}
