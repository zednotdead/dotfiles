return {
	{
		"folke/edgy.nvim",
		event = "VeryLazy",
		init = function()
			vim.opt.laststatus = 3
			vim.opt.splitkeep = "screen"
		end,
		keys = {
			{
				"<leader>u",
				desc = "Window layout",
			},
			{
				"<leader>ue",
				function()
					require("edgy").toggle()
				end,
				desc = "Edgy Toggle",
			},
			{
				"<leader>uE",
				function()
					require("edgy").select()
				end,
				desc = "Edgy Select Window",
			},
		},
		---@module "edgy"
		---@type Edgy.Config
		opts = {
			keys = {
				-- increase width
				["<c-Right>"] = function(win)
					win:resize("width", 2)
				end,
				-- decrease width
				["<c-Left>"] = function(win)
					win:resize("width", -2)
				end,
				-- increase height
				["<c-Up>"] = function(win)
					win:resize("height", 2)
				end,
				-- decrease height
				["<c-Down>"] = function(win)
					win:resize("height", -2)
				end,
			},
			bottom = {
				{
					ft = "toggleterm",
					size = { height = 0.1 },
					---@diagnostic disable-next-line: unused-local
					filter = function(buf, win)
						return vim.api.nvim_win_get_config(win).relative == ""
					end,
				},
				{ ft = "qf", title = "QuickFix" },
				{
					ft = "help",
					size = { height = 20 },
					-- only show help buffers
					filter = function(buf)
						return vim.bo[buf].buftype == "help"
					end,
				},
				{ ft = "grug-far", size = { height = 0.4 } },
			},
			left = {
				{
					title = "Neo-tree [Filesystem]",
					ft = "neo-tree",
					open = function()
						vim.cmd(("Neotree show filesystem position=%s"):format("left"))
					end,
					filter = function(buf)
						return vim.b[buf].neo_tree_source == "filesystem"
					end,
					pinned = true,
					collapsed = false,
				},
			},
		},
		config = true,
	},
}
