function _G.terminal_keymaps()
	local opts = { buffer = 0 }
	vim.keymap.set("t", "<C-esc>", [[<C-\><C-n>]], opts)
	vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], opts)
	vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], opts)
	vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], opts)
	vim.keymap.set("t", "<C-l>", [[<Cmd>wincmd l<CR>]], opts)
end

vim.cmd("autocmd! TermOpen term://* lua terminal_keymaps()")
-- Neotree BEGIN
local nt_loaded, nt_cmd = pcall(require, "neo-tree.command")
if nt_loaded then
	local function toggle_filetree()
		nt_cmd.execute({ toggle = true })
	end

	vim.keymap.set("n", "<Leader><Tab>", toggle_filetree, { desc = "Open neotree" })
end
-- Neotree END

-- nvim-tree BEGIN
local nvt_loaded, nvt = pcall(require, "nvim-tree.api")
if nvt_loaded then
	local function toggle_filetree()
		nvt.tree.toggle()
	end

	vim.keymap.set("n", "<Leader><Tab>", toggle_filetree, { desc = "Open neotree" })
end
-- nvim-tree END

-- Sessions BEGIN
local possession_loaded, pos = pcall(require, "possession")
if possession_loaded then
	vim.keymap.set("n", "<Leader>sl", function()
		require("telescope").extensions.possession.list()
	end, { desc = "Load session" })

	vim.keymap.set("n", "<Leader>ss", function()
		local session_name = require("possession.session").get_session_name()
		if session_name == nil then
			vim.ui.input({ prompt = "Input name of session: " }, function(input)
				pos.save(input, { no_confirm = true })
			end)
		else
			pos.save(session_name, { no_confirm = true })
		end
	end, { desc = "Save current session" })
end

local persisted_loaded, pers = pcall(require, "persisted")
if persisted_loaded then
	vim.keymap.set("n", "<Leader>sl", "<Cmd>Telescope persisted<CR>", { desc = "Load session" })

	vim.keymap.set("n", "<Leader>ss", function()
		pers.save()
	end, { desc = "Save current session" })
end

local as_loaded, _ = pcall(require, "auto-session")
if as_loaded then
	vim.keymap.set("n", "<Leader>sl", "<cmd>SessionSearch<CR>", { desc = "Load session" })

	vim.keymap.set("n", "<Leader>ss", "<cmd>SessionSave<CR>", { desc = "Save session" })
end
-- Sessions END

-- LSP BEGIN
-- Functions BEGIN
local conform_loaded, conform = pcall(require, "conform")
local format_fn

if conform_loaded then
	format_fn = function()
		conform.format()
	end
else
	format_fn = function()
		vim.lsp.buf.format()
	end
end

local ap_loaded, ap = pcall(require, "actions-preview")

local code_action_fn = function()
	if ap_loaded then
		ap.code_actions()
	else
		vim.lsp.buf.code_action()
	end
end

local glances_loaded, glances = pcall(require, "glance")

local show_diagnostics_fn = function()
	vim.diagnostic.open_float()
end
local goto_definition_fn = function()
	if glances_loaded then
		glances.open("definitions")
	else
		vim.lsp.buf.definition()
	end
end
local goto_references_fn = function()
	if glances_loaded then
		glances.open("references")
	else
		vim.lsp.buf.references()
	end
end
local hover_fn = function()
	vim.lsp.buf.hover()
end

local rename_fn = function()
	vim.lsp.buf.rename()
end

local increname_loaded, _ = pcall(require, "inc_rename")
if increname_loaded then
	rename_fn = function()
		return ":IncRename " .. vim.fn.expand("<cword>")
	end
end

-- END

-- Bindings BEGIN
vim.keymap.set("n", "<Leader>lf", format_fn, { desc = "Format" })
vim.keymap.set("n", "<Leader>la", code_action_fn, { desc = "Code action" })
vim.keymap.set("n", "ga", code_action_fn, { desc = "Code action" })
vim.keymap.set("n", "<Leader>ld", show_diagnostics_fn, { desc = "Show diagnostics" })
vim.keymap.set("n", "gd", goto_definition_fn, { desc = "Go to definition" })
vim.keymap.set("n", "gD", goto_references_fn, { desc = "Show references" })
vim.keymap.set("n", "K", hover_fn, { desc = "Show information" })
vim.keymap.set("n", "<Leader>lr", rename_fn, { desc = "Rename", expr = increname_loaded })
vim.keymap.set("n", "gr", rename_fn, { desc = "Rename", expr = increname_loaded })
-- Bindings END
-- LSP END

-- Telescope BEGIN
local telescope_loaded, _ = pcall(require, "telescope")
if telescope_loaded then
	-- Functions BEGIN

	local function telescope_find_files()
		local frecency_loaded, frecency = pcall(require, "telescope-frecency")
		local smartopen_loaded = pcall(require, "smart-open")
		if smartopen_loaded then
			require("telescope").extensions.smart_open.smart_open({
				cwd_only = true,
			})
		elseif frecency_loaded then
			frecency.start({ workspace = "CWD" })
		else
			require("telescope.builtin").find_files()
		end
	end

	-- SOURCE: https://github.com/nvim-telescope/telescope.nvim/wiki/Configuration-Recipes#live-grep-from-project-git-root-with-fallback
	local function telescope_live_grep_no_gitignore()
		local function is_git_repo()
			vim.fn.system("git rev-parse --is-inside-work-tree")

			return vim.v.shell_error == 0
		end

		local function get_git_root()
			local dot_git_path = vim.fn.finddir(".git", ".;")
			return vim.fn.fnamemodify(dot_git_path, ":h")
		end

		local opts = {}

		if is_git_repo() then
			opts = {
				cwd = get_git_root(),
			}
		end

		require("telescope.builtin").live_grep(opts)
	end

	local function telescope_live_grep()
		local opts = {}

		require("telescope.builtin").live_grep(opts)
	end

	local function telescope_diagnostics()
		require("telescope.builtin").diagnostics()
	end
	-- Functions END

	-- Bindings BEGIN
	vim.keymap.set("n", "<Leader>fi", telescope_find_files, { desc = "Find files" })
	vim.keymap.set("n", "<C-S-f>", telescope_live_grep_no_gitignore, { desc = "Live grep" })
	-- Unset, something conflicts
	vim.keymap.set("n", "<Leader>ff", "<Nop>", { desc = "Live grep" })
	vim.keymap.set("n", "<Leader>ff", telescope_live_grep, { desc = "Live grep" })
	vim.keymap.set("n", "<Leader>fF", telescope_live_grep_no_gitignore, { desc = "Live grep" })
	vim.keymap.set("n", "<Leader>fd", telescope_diagnostics, { desc = "Diagnostics" })
	vim.keymap.set("n", "<Leader>lD", telescope_diagnostics, { desc = "Diagnostics" })
	-- Bindings END
end
-- Telescope END

-- Find and replace BEGIN

local grug_loaded, grug = pcall(require, "grug-far")

if grug_loaded then
	local function find_and_replace_toggle()
		local ext = vim.bo.buftype == "" and vim.fn.expand("%:e")
		grug.open({
			transient = true,
			prefills = {
				filesFilter = ext and ext ~= "" and "*." .. ext or nil,
			},
		})
	end

	vim.keymap.set("n", "<Leader>fr", find_and_replace_toggle, { desc = "Find and replace" })
end

-- Find and replace END

-- Noice BEGIN
vim.keymap.set("n", "<Leader><Esc>", function()
	require("noice").cmd("dismiss")
	vim.cmd([[cclose]])
end, { desc = "Dismiss everything" })
-- Noice END

-- Tabs BEGIN
local barbar_loaded, bb = pcall(require, "barbar.api")
if barbar_loaded then
	vim.keymap.set("n", "gt", function()
		vim.cmd([[BufferNext]])
	end, { desc = "Next tab" })
	vim.keymap.set("n", "gT", function()
		vim.cmd([[BufferPrevious]])
	end, { desc = "Previous tab" })
	vim.keymap.set("n", "<Leader>t.", function()
		vim.cmd([[BufferNext]])
	end, { desc = "Next tab" })
	vim.keymap.set("n", "<Leader>t,", function()
		vim.cmd([[BufferPrevious]])
	end, { desc = "Previous tab" })

	vim.keymap.set("n", "<Leader>tt", bb.pick_buffer, { desc = "Pick tab" })
	vim.keymap.set("n", "<Leader>td", function()
		bb.pick_buffer_delete(1, true)
	end, { desc = "Pick tab to delete" })

	for var = 1, 9 do
		vim.keymap.set("n", "<A-" .. var .. ">", function()
			bb.goto_buffer(var)
		end, { desc = "Go to tab " .. var })
	end

	vim.keymap.set("n", "<A-0>", function()
		bb.goto_buffer(-1)
	end, { desc = "Go to last tab" })

	vim.keymap.set("n", "<Leader>to", bb.close_all_but_current_or_pinned, { desc = "Close all but last tab" })
end

local bufferline_loaded, bfl = pcall(require, "bufferline")
if bufferline_loaded then
	local function next_tab()
		bfl.cycle(1)
	end
	local function prev_tab()
		bfl.cycle(-1)
	end

	vim.keymap.set("n", "gt", next_tab, { desc = "Next tab" })
	vim.keymap.set("n", "gT", prev_tab, { desc = "Previous tab" })
	vim.keymap.set("n", "<Leader>t.", next_tab, { desc = "Next tab" })
	vim.keymap.set("n", "<Leader>t,", prev_tab, { desc = "Previous tab" })
	vim.keymap.set("n", "<C-]>", next_tab, { desc = "Next tab" })
	vim.keymap.set("n", "<C-[>", prev_tab, { desc = "Previous tab" })

	vim.keymap.set("n", "<Leader>tt", bfl.pick, { desc = "Pick tab" })
	vim.keymap.set("n", "<Leader>td", bfl.close_with_pick, { desc = "Pick tab to delete" })

	vim.keymap.set("n", "<Leader>to", bfl.close_others, { desc = "Close all but last tab" })
end
-- Tabs END

vim.keymap.set("n", "<Leader>gb", "<Cmd>GitBlameToggle<CR>", { desc = "Toggle blame" })

local refactor_loaded, _ = pcall(require, "refactoring")
if refactor_loaded then
	vim.keymap.set({ "n", "x" }, "<leader>r", function()
		require("telescope").extensions.refactoring.refactors()
	end, { desc = "Refactoring" })
end

local zen_loaded, zen = pcall(require, "zen-mode")
if zen_loaded then
	-- Bindings BEGIN
	vim.keymap.set("n", "<Leader>z", function()
		zen.toggle({
			window = {
				width = 0.85,
			},
		})
	end, { desc = "Zen mode" })
	-- Bindings END
end

local wk_loaded, wk = pcall(require, "which-key")
-- Load prefix names
if wk_loaded then
	wk.add({
		{ "<Leader>s", group = "sessions" },
		{ "<Leader>f", group = "find" },
		{ "<Leader>l", group = "LSP" },
		{ "<Leader>g", group = "git" },
		{ "<Leader>t", group = "tabs" },
		{ "<Leader><Leader>", group = "comment" },
	})
end

local spider_loaded, spider = pcall(require, "spider")
if spider_loaded then
	vim.keymap.set({ "n", "o", "x" }, "w", function()
		spider.motion("w")
	end, { desc = "Spider-w" })
	vim.keymap.set({ "n", "o", "x" }, "e", function()
		spider.motion("e")
	end, { desc = "Spider-e" })
	vim.keymap.set({ "n", "o", "x" }, "b", function()
		spider.motion("b")
	end, { desc = "Spider-b" })
end

local luasnip_loaded, ls = pcall(require, "luasnip")
if luasnip_loaded then
	vim.keymap.set({ "i" }, "<C-K>", function()
		ls.expand()
	end, { silent = true })
	vim.keymap.set({ "i", "s" }, "<C-.>", function()
		ls.jump(1)
	end, { silent = true })
	vim.keymap.set({ "i", "s" }, "<C-,>", function()
		ls.jump(-1)
	end, { silent = true })
end

local ufo_loaded, ufo = pcall(require, "ufo")
if ufo_loaded then
	vim.keymap.set("n", "zR", ufo.openAllFolds)
	vim.keymap.set("n", "zM", ufo.closeAllFolds)
end

local tt_loaded, _ = pcall(require, "toggleterm")
if tt_loaded then
	vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })
	vim.keymap.set("n", "<leader>x", _floating_toggle)
end

local dial_loaded, dial = pcall(require, "dial.map")
if dial_loaded then
	vim.keymap.set("n", "<C-a>", function()
		dial.manipulate("increment", "normal")
	end)
	vim.keymap.set("n", "<C-x>", function()
		dial.manipulate("decrement", "normal")
	end)
	vim.keymap.set("n", "g<C-a>", function()
		dial.manipulate("increment", "gnormal")
	end)
	vim.keymap.set("n", "g<C-x>", function()
		dial.manipulate("decrement", "gnormal")
	end)
	vim.keymap.set("v", "<C-a>", function()
		dial.manipulate("increment", "visual")
	end)
	vim.keymap.set("v", "<C-x>", function()
		dial.manipulate("decrement", "visual")
	end)
	vim.keymap.set("v", "g<C-a>", function()
		dial.manipulate("increment", "gvisual")
	end)
	vim.keymap.set("v", "g<C-x>", function()
		dial.manipulate("decrement", "gvisual")
	end)
end

vim.keymap.set("n", "ZZ", "<Cmd>w | qa<CR>", { desc = "quick quit" })
