# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.
with import<nixpkgs>{};
import (builtins.fetchGit {
    url = "git://github.com/NixOS/nixpkgs";
    rev = "afdb5675a180f347bfa8ae909d4e419fb8b151bd";
  })
