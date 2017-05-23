
.PHONY: update-build

update-build:
	nix-shell update-build-shell.nix --run ./update-build.sh
