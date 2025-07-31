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

vim.opt.undofile = true

local undodir = vim.fn.stdpath("data") .. "/undodir"
if not vim.fn.isdirectory(undodir) then
  ---@diagnostic disable-next-line: param-type-mismatch
  vim.call(vim.fn.mkdir(vim.fn.expand(undodir), "p"))
end

vim.opt.undodir = undodir

require("config.autocmd")
