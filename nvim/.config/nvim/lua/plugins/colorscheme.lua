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
		enabled = true,
		lazy = false,
		priority = 1000,
    config = function ()
      require('gruvbox').setup({
        contrast = "hard"
      })
      vim.cmd.colorscheme("gruvbox")
    end
	},
}
