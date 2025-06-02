local ignore = {
	"node_modules",
	".git",
	".venv",
	"lazy-lock.json",
	"package-lock.json",
	"yarn.lock",
	"dist",
	"target",
	"build",
	".gradle",
}

return {
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.3",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		opts = {
			pickers = {
				live_grep = {
					theme = "ivy",
					file_ignore_patterns = ignore,
					additional_args = function(_)
						return { "--hidden" }
					end,
				},
				find_files = {
					file_ignore_patterns = ignore,
					find_command = { "fd", "-tf", "--hidden" },
				},
			},
			extensions = {
				smart_open = {
					show_scores = true,
				},
			},
		},
	},
	{
		"nvim-telescope/telescope-frecency.nvim",
		enabled = true,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
		},
		config = function()
			require("telescope").load_extension("frecency")
		end,
	},
	{
		"danielfalk/smart-open.nvim",
    enabled = false,
		branch = "0.2.x",
		config = function()
			require("telescope").load_extension("smart_open")
		end,
		dependencies = {
			"kkharji/sqlite.lua",
			-- Only required if using match_algorithm fzf
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			-- Optional.  If installed, native fzy will be used when match_algorithm is fzy
			{ "nvim-telescope/telescope-fzy-native.nvim" },
		},
	},
}
