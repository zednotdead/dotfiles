return {
	{
		"folke/edgy.nvim",
		event = "VeryLazy",
		init = function()
			vim.opt.laststatus = 3
			vim.opt.splitkeep = "screen"
		end,
		---@module "edgy"
		---@type Edgy.Config
		opts = {
			bottom = {
				-- toggleterm / lazyterm at the bottom with a height of 40% of the screen
				{
					ft = "toggleterm",
					size = { height = 0.1 },
					-- exclude floating windows
					pinned = true,
					collapsed = false,
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
					ft = "neo-tree",
					open = function()
						vim.cmd(("Neotree show filesystem position=%s"):format("left"))
					end,
					pinned = true,
					collapsed = false,
				},
			},
		},
		config = true,
	},
}
