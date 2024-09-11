return {
	{
		"nvim-lualine/lualine.nvim",
		lazy = false,
		enabled = true,
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("lualine").setup({})
		end,
	},
}
