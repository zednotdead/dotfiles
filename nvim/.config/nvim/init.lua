vim.g.mapleader = "<Space>"

require("config.lazy")

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.opt.termguicolors = true

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "number"

vim.opt.fillchars = { eob = " " }

vim.cmd("colorscheme gruvbox")
vim.diagnostic.config({ virtual_lines = true })

require("config.keymaps")
require("config.autocmd")
