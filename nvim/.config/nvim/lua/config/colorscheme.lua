local cp_loaded, _ = pcall(require, "catppuccin")
local tn_loaded, _ = pcall(require, "tokyonight")

if cp_loaded then
  vim.cmd.colorscheme("catppuccin")
elseif tn_loaded then
  vim.cmd.colorscheme("tokyonight-night")
end

