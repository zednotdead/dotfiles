return {
	{
		"akinsho/nvim-toggleterm.lua",
		keys = {
			{ "<leader>x", desc = "Terminal" },
			{ "<leader>gg", _gitui_toggle, desc = "Gitui" },
		},
		opts = {
			size = 20,
			hide_numbers = true,
			shade_filetypes = {},
			shade_terminals = false,
			shading_factor = 0.1, -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
			start_in_insert = true,
			persist_size = true,
			direction = "horizontal",
			close_on_exit = true,
			float_opts = {
				border = { "┏", "━", "┓", "┃", "┛", "━", "┗", "┃" },
			},
g	},
    config = function (_, opts)
      require("toggleterm").setup(opts)
      require("config.terminals")
    end
	},
}
