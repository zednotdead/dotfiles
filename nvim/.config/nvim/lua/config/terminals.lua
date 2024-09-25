local tt_loaded, tt = pcall(require, "toggleterm.terminal")
if not tt_loaded then
	return
end

local gitui = tt.Terminal:new({
	cmd = "gitui",
	hidden = true,
	direction = "float",
	close_on_exit = true,
	float_opts = { width = 120, height = 30 },
})

---@diagnostic disable-next-line: lowercase-global
function _gitui_toggle()
	gitui:toggle()
end

vim.keymap.set("n", "<Leader>gg", _gitui_toggle, { desc = "Toggle GitUI" })
