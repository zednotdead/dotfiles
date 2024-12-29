local cp_loaded, _ = pcall(require, "catppuccin")
local tn_loaded, _ = pcall(require, "tokyonight")
local kg_loaded, _ = pcall(require, "kanagawa")

if cp_loaded then
  vim.cmd.colorscheme("catppuccin")
elseif tn_loaded then
  vim.cmd.colorscheme("tokyonight-night")
elseif kg_loaded then
  vim.cmd.colorscheme("kanagawa")
end

