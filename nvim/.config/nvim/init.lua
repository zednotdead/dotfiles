-- Proudly stolen from Mark McDonnell
-- https://github.com/Integralist/nvim

-- Omnisharp
vim.g.OmniSharp_server_use_net6 = 1
vim.g.OmniSharp_highlighting = 0

-- Set up leader
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Set up tab width
vim.o.expandtab = true
vim.o.shiftwidth = 4

-- Set up line numbers
vim.o.number = true
vim.o.relativenumber = true

-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true

-- set GUI font
local font = "PragmataPro Liga"

local os = vim.uv.os_uname().sysname
if os == "Darwin" then
	vim.opt.guifont = { font, ":h16" }
elseif os == "Linux" then
	vim.opt.guifont = { font, ":h14" }
end

-- set up folds
vim.o.foldcolumn = "1" -- '0' is not bad
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

-- Set up Lazy.nvim
require("config.lazy")

vim.opt.undofile = true
local undodir = vim.fn.stdpath("data") .. "/undodir"
if not vim.fn.isdirectory(undodir) then
	vim.call(vim.fn.mkdir(vim.fn.expand(undodir), "p"))
end
vim.opt.undodir = undodir

vim.loader.enable()
vim.o.background = "dark"

vim.diagnostic.config({ virtual_lines = true })
vim.o.backupcopy = "yes"

require("mappings")
