return {
	{
		"jedrzejboczar/possession.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
		opts = {
			autosave = {
				current = true,
				on_quit = true,
			},
		},
		config = function(_, opts)
			require("possession").setup(opts)
			require("telescope").load_extension("possession")
		end,
	},
}
