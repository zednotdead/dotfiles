return {
	{
		"folke/noice.nvim",
		enabled = true,
		event = "VeryLazy",
		dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
		opts = {
			routes = {
				-- skip displaying message that file was written to.
				{
					filter = {
						event = "msg_show",
						kind = "",
						find = "written",
					},
					opts = { skip = true },
				},
				{
					filter = {
						event = "msg_show",
						kind = "",
						find = "more lines",
					},
					opts = { skip = true },
				},
				{
					filter = {
						event = "msg_show",
						kind = "",
						find = "fewer lines",
					},
					opts = { skip = true },
				},
				{
					filter = {
						event = "msg_show",
						kind = "",
						find = "lines yanked",
					},
					opts = { skip = true },
				},
				{
					view = "split",
					filter = { event = "msg_show", min_height = 10 },
				},
			},
			lsp = {
				progress = {
					enabled = false,
				},
				signature = {
					enabled = false,
				},
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true,
				},
			},
			presets = {
				bottom_search = true, -- use a classic bottom cmdline for search
				command_palette = true, -- position the cmdline and popupmenu together
				long_message_to_split = true, -- long messages will be sent to a split
				inc_rename = true, -- enables an input dialog for inc-rename.nvim
				lsp_doc_border = true, -- add a border to hover docs and signature help
			},
		},
	},
	{
		"stevearc/dressing.nvim",
		enabled = true,
		lazy = false,
	},
	{
		"rcarriga/nvim-notify",
		config = true,
		opts = {},
	},
	{
		"Bekaboo/dropbar.nvim",
		event = "BufEnter",
		dependencies = {
			"nvim-telescope/telescope-fzf-native.nvim",
		},
	},
	{
		"mvllow/modes.nvim",
		event = "BufEnter",
		config = function()
			require("modes").setup()
		end,
	},
  {
    "sindrets/diffview.nvim",
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory"
    },
    opts = {},
  },
  {
    "monaqa/dial.nvim",
    enabled = true,
    lazy = false,
  }
}
