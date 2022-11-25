local color_scheme = "gruvbox-dark-soft"

local ok, _ = pcall(vim.cmd, string.format("colorscheme base16-%s", color_scheme))
if not ok then
	vim.cmd("colorscheme default") -- if the above fails, then use default
end
