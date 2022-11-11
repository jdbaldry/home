{ stdenv, buildGoModule }:

buildGoModule rec {
  pname = "build-pipeline";
  version = "0.4.9";

  src = builtins.fetchGit {
    url = "git@github.com:grafana/${pname}.git";
    rev = "b99a7d03c4733643d62940c7b6f0ff145e30e420"; # v0.4.9
    # sha256 = "01p7vqjd0izwh6d4x5hl4d9lgb9jp5h4d7hrvplj6wgrrxqgksxk";
  };

  modSha256 = "1y5d5z3a118j6bh6rgvaln9xvi6fa0lrwy5w681wg0g0s5wqpjyz";

  meta = with stdenv.lib; {
    description = "Consolidated Grafana build pipeline tool";
  };
}
