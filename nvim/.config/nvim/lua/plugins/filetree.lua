return {
	{
		"nvim-neo-tree/neo-tree.nvim",
		enabled = true,
		branch = "v3.x",
		cmd = { "Neotree" },
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
			"MunifTanjim/nui.nvim",
			"3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
		},
		opts = {
			close_if_last_window = true,
			filesystem = {
				follow_current_file = {
					enabled = true,
					leave_dirs_open = true,
				},
				filtered_items = {
					hide_dotfiles = false,
					visible = true,
				},
			},
			auto_clean_after_session_restore = true,
		},
	},
	{
		"nvim-tree/nvim-tree.lua",
		version = "*",
		lazy = false,
    enabled = false,
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			require("nvim-tree").setup({})
		end,
	},
}
