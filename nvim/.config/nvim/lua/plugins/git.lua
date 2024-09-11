return {
	{
		"lewis6991/gitsigns.nvim",
		config = true,
    event = "BufEnter"
	},
	{
		"f-person/git-blame.nvim",
    name = "gitblame",
		event = "BufEnter",
		opts = {
			enabled = false,
		},
	},
}
