.PHONY : neovim zsh
install : neovim zsh

clean : neovim-delete zsh-delete

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
	rm -rf "$HOME/.asdf"
