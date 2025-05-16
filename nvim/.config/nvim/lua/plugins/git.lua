return {
	{
		"lewis6991/gitsigns.nvim",
		config = true,
		event = "BufEnter",
	},
	{
		"f-person/git-blame.nvim",
		name = "gitblame",
		event = "BufEnter",
		opts = {
			enabled = true,
			ignored_filetypes = { "gitcommit" },
		},
	},
	{ "akinsho/git-conflict.nvim", version = "*", config = true },
	{
		"luukvbaal/statuscol.nvim",
		dependencies = {
			"mfussenegger/nvim-dap",
			"lewis6991/gitsigns.nvim",
		},
		config = function()
			local builtin = require("statuscol.builtin")
			require("statuscol").setup({
				clickhandlers = { -- builtin click handlers, keys are pattern matched
					Lnum = builtin.lnum_click,
					FoldClose = builtin.foldclose_click,
					FoldOpen = builtin.foldopen_click,
					FoldOther = builtin.foldother_click,
					DapBreakpointRejected = builtin.toggle_breakpoint,
					DapBreakpoint = builtin.toggle_breakpoint,
					DapBreakpointCondition = builtin.toggle_breakpoint,
					["diagnostic/signs"] = builtin.diagnostic_click,
					gitsigns = builtin.gitsigns_click,
				},
			})
		end,
	},
}
