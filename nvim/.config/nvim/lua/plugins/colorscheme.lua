return {
	{
		"catppuccin/nvim",
		name = "catppuccin",
		enabled = false,
		priority = 1000,
		lazy = false,
		config = function()
			require("catppuccin").setup({
				flavour = "mocha",
			})

			require("config.colorscheme")
		end,
	},
	{
		"folke/tokyonight.nvim",
		enabled = false,
		lazy = false,
		priority = 1000,
		config = function()
			require("config.colorscheme")
		end,
	},
	{
		"rebelot/kanagawa.nvim",
		enabled = true,
		lazy = false,
		priority = 1000,
		config = function()
			require("kanagawa").setup({
				theme = "wave"
			})

			require("config.colorscheme")
		end,
	},
}
