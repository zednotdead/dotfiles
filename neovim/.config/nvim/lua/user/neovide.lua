local set = vim.opt

if vim.fn.has("macunix") then
	set.guifont = "Iosevka Nerd Font:h15"
else
	set.guifont = "Iosevka:h15"
end
