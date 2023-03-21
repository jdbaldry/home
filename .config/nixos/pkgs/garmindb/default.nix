{ pkgs ? import <nixpkgs> }:


with pkgs;
let
  python-dateutil = python3Packages.buildPythonPackage
    rec {
      pname = "python-dateutil";
      version = "2.8.2";

      src = python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "sha256-ASPKzBYnrhnd88J6XeW9Z+5FhvvdZEDZdI+Ku0g9PoY=";
      };

      meta = with lib; {
        description = "TODO";
        homepage = "TODO";
        maintainers = with maintainers; [ jdbaldry ];
      };
    };
in
let
  idbutils = python3Packages.buildPythonPackage
    rec {
      pname = "idbutils";
      version = "1.0.6";

      buildInputs = [ python-dateutil ];
      src = python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "sha256-dU5YfJ7ae6v186e9uII/68AkLIsUnd7UvGCB2nKDp6E=";
      };

      meta = with lib; {
        description = "TODO";
        homepage = "TODO";
        maintainers = with maintainers; [ jdbaldry ];
      };
    };
  fitfile = python3Packages.buildPythonPackage
    rec {
      pname = "fitfile";
      version = "1.1.3";

      src = python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "sha256-KfupwZ8KsaxCLEB6e7Zve8HJA9ewmDAKSulNwPaSouE=";
      };

      meta = with lib; {
        description = "TODO";
        homepage = "https://github.com/tcgoetz/fitfile";
        maintainers = with maintainers; [ jdbaldry ];
        license = licenses.gpl2;
      };
    };
in
python3Packages.buildPythonPackage
rec {
  pname = "garmindb";
  version = "3.2.6";

  buildInputs = [ fitfile idbutils ];
  src = python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "sha256-o1yRjNr6FgKw+Vc+9FXG9EuwkvlV6NE356CsctKIn4U=";
  };

  meta = with lib; {
    description = "Download and parse data from Garmin Connect or a Garmin watch, FitBit CSV, and MS Health CSV files into and analyze data in Sqlite serverless databases with Jupyter notebooks.";
    homepage = "https://github.com/tcgoetz/GarminDB";
    maintainers = with maintainers; [ jdbaldry ];
    license = licenses.gpl2;
  };
}
