return {
	{
		"kylechui/nvim-surround",
		version = "*",
		event = "VeryLazy",
		config = true,
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-treesitter/nvim-treesitter-textobjects",
		},
	},
	{
		"pest-parser/pest.vim",
		ft = "pest",
		config = true,
	},
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {},
	},
	{
		"windwp/nvim-ts-autotag",
		event = "InsertEnter",
		opts = {},
	},
	{
		"smjonas/inc-rename.nvim",
		cmd = "IncRename",
		opts = {},
	},
}
