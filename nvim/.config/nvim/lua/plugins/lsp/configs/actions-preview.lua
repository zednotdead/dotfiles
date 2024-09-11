return {
	{
		"aznhe21/actions-preview.nvim",
		dependencies = { "nvim-telescope/telescope.nvim" },
		config = function()
			require("actions-preview").setup({
				backend = { "telescope" },
				telescope = vim.tbl_extend("force", require("telescope.themes").get_ivy(), {}),
			})
		end,
	},
}
