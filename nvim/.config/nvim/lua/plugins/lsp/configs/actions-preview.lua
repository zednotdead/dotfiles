return {
	{
		"aznhe21/actions-preview.nvim",
		dependencies = { "nvim-telescope/telescope.nvim" },
		config = function()
			local hl = require("actions-preview.highlight")
			require("actions-preview").setup({
				highlight_command = { hl.delta() },
				backend = { "telescope" },
				telescope = vim.tbl_extend("force", require("telescope.themes").get_ivy(), {}),
			})
		end,
	},
}
