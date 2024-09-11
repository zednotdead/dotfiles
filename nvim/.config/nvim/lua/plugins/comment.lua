return {
	{
		"folke/todo-comments.nvim",
		event = "BufEnter",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {
			keywords = {
				TODO = { alt = { "todo", "unimplemented" } },
			},
			highlight = {
				pattern = {
					[[.*<(KEYWORDS)\s*:]],
					[[.*<(KEYWORDS)\s*!\(]],
				},
				comments_only = false,
			},
			search = {
				pattern = [[\b(KEYWORDS)(:|!\()]],
			},
		},
	},
	{
		"numToStr/Comment.nvim",
		event = "BufEnter",
		opts = {
			toggler = {
				line = "<leader><leader><leader>",
			},
			opleader = {
				line = "<leader><leader><leader>",
				block = "<leader><leader>b",
			},
		},
		config = true,
	},
}

