return {
	{
		"catppuccin/nvim",
		name = "catppuccin",
		enabled = false,
		priority = 1000,
		lazy = false,
		opts = {
			flavour = "mocha",
		},
		config = true,
	},
	{
		"folke/tokyonight.nvim",
		enabled = false,
		lazy = false,
		priority = 1000,
	},

	{
		"folke/tokyonight.nvim",
		enabled = false,
		lazy = false,
		priority = 1000,
	},
	{
		"ellisonleao/gruvbox.nvim",
		enabled = false,
		lazy = false,
		priority = 1000,
		config = function()
			require("gruvbox").setup({
				contrast = "hard",
			})
			vim.cmd.colorscheme("gruvbox")
		end,
	},
	{
		"neanias/everforest-nvim",
		version = false,
		lazy = false,
    enabled = false,
		priority = 1000, -- make sure to load this before all the other start plugins
		-- Optional; default configuration will be used if setup isn't called.
		config = function()
			require("everforest").setup({
        background = "hard",
			})
      vim.cmd.colorscheme("everforest")
		end,
	},
	{
		"olimorris/onedarkpro.nvim",
    lazy = false,
		config = function ()
			vim.cmd.colorscheme("onedark")
		end
	},
}
