return {
	{
		"lewis6991/gitsigns.nvim",
		config = true,
	},
	{
		"f-person/git-blame.nvim",
		opts = {
			enabled = false,
		},
		config = function(opts)
			require("gitblame").setup(opts)
		end,
	},
}
