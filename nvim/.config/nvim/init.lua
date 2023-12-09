-- Proudly stolen from Mark McDonnell
-- https://github.com/Integralist/nvim

-- Set up leader
vim.g.mapleader = " "

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

local os = vim.loop.os_uname().sysname
if os == "Darwin" then
    vim.opt.guifont = { font, ":h16" }
elseif os == "Linux" then
    vim.opt.guifont = { font, ":h14" }
end

vim.g.python3_host_prog = "/home/zed/.local/share/rtx/installs/python/3.10.11/bin/python3"

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)
vim.lsp.buf.format({
    filter = function(client)
        return client.name ~= "tsserver"
    end,
})

vim.opt.undofile = true
local undodir = vim.fn.stdpath("data") .. "/undodir"
if not vim.fn.isdirectory(undodir) then
    vim.call(vim.fn.mkdir(vim.fn.expand(undodir), "p"))
end
vim.opt.undodir = undodir

vim.loader.enable()

require("lazy").setup("plugins")
require("mappings")
