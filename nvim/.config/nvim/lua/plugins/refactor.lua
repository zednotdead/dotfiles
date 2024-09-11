return {
	{
		"ThePrimeagen/refactoring.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"nvim-telescope/telescope.nvim",
		},
		opts = {},
		config = function(opts)
			require("refactoring").setup(opts)
			require("telescope").load_extension("refactoring")
		end,
	},
}
