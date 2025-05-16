return {
	{
		"nvim-treesitter/nvim-treesitter",
		config = function()
			vim.o.foldmethod = "expr"
			vim.o.foldexpr = "nvim_treesitter#foldexpr()"
			vim.o.foldenable = false

			require("nvim-treesitter.configs").setup({
				sync_install = false,
				auto_install = true,
				ignore_install = {},
				modules = {},
				ensure_installed = "all",
				highlight = {
					enable = true,
				},
				indent = {
					enable = false,
				},
				textobjects = {
					select = {
						enable = true,
						lookahead = true,
						keymaps = {
							-- You can use the capture groups defined in textobjects.scm
							["af"] = "@function.outer",
							["if"] = "@function.inner",
							["ac"] = "@class.outer",
							["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
							-- You can also use captures from other query groups like `locals.scm`
							["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
							["ap"] = "@parameter.outer",
							["ip"] = "@parameter.inner",
						},
						-- If you set this to `true` (default is `false`) then any textobject is
						-- extended to include preceding or succeeding whitespace. Succeeding
						-- whitespace has priority in order to act similarly to eg the built-in
						-- `ap`.
						include_surrounding_whitespace = true,
					},
					move = {
						enable = true,
						set_jumps = true, -- whether to set jumps in the jumplist

						goto_next_start = {
							["]m"] = "@function.outer",
							["]p"] = "@parameter.outer",
						},
						goto_previous_start = {
							["[m"] = "@function.outer",
							["[p"] = "@parameter.outer",
						},
						goto_next_end = {
							["]M"] = "@function.outer",
							["]P"] = "@parameter.outer",
						},
						goto_previous_end = {
							["[M"] = "@function.outer",
							["[P"] = "@parameter.outer",
						},
					},
				},
				selection_modes = {
					["@parameter.outer"] = "v", -- charwise
					["@function.outer"] = "V", -- linewise
					["@class.outer"] = "<c-v>", -- blockwise
				},
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "vv",
						node_incremental = "+",
						node_decremental = "=",
					},
				},
			})
		end,
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"IndianBoy42/tree-sitter-just",
		},
	},
	{
		"IndianBoy42/tree-sitter-just",
		config = true,
	},
	{
		"nvim-treesitter/nvim-treesitter-context",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		lazy = false,
		---@module "treesitter-context"
		---@type TSContext.UserConfig
		opts = {
			enable = true,
		},
	},
}
