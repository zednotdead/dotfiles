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

local floating = tt.Terminal:new({
	hidden = true,
	direction = "horizontal",
	close_on_exit = true,
})

---@diagnostic disable-next-line: lowercase-global
function _gitui_toggle()
	gitui:toggle()
end

---@diagnostic disable-next-line: lowercase-global
function _floating_toggle()
  floating:toggle()
end

vim.keymap.set("n", "<Leader>gg", _gitui_toggle, { desc = "Toggle GitUI" })
