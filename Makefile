.PHONY : neovim neovide zsh asdf rust
install : neovim neovide zsh asdf rust
install-mac : neovim zsh asdf

clean : neovim-delete zsh-delete asdf-delete rust-delete
clean-mac : neovim-delete zsh-delete asdf-delete

neovim :
	stow -S neovim

zsh :
	stow -S zsh

neovim-delete :
	-stow -D neovim 2> /dev/null

zsh-delete :
	-stow -D zsh 2> /dev/null

asdf :
	zsh ./deps/install_asdf.zsh

asdf-delete :
	rm -rf "${HOME}/.asdf"

neovide :
	zsh ./deps/install_neovide.zsh

neovide-delete :
	rm -rf ${HOME}/.local/bin/neovide

rust :
	zsh ./deps/install_rust.zsh

rust-delete :
	rm -rf ${HOME}/.cargo

