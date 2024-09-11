local ignore = {
	"node_modules",
	".git",
	".venv",
	"lazy-lock.json",
	"package-lock.json",
	"yarn.lock",
	"dist",
	"target"
}

return {
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.3",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {
			pickers = {
				live_grep = {
					theme = "ivy",
					file_ignore_patterns = ignore,
					additional_args = function(_)
						return { "-uuu" }
					end,
				},
				find_files = {
					file_ignore_patterns = ignore,
					find_command = { "fd", "-tf", "-u" },
				},
			},
		},
	},
}
