return {
	"lukas-reineke/indent-blankline.nvim",
	"stevearc/dressing.nvim",
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.3",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {
			pickers = {
				live_grep = { theme = "ivy" },
			},
		},
	},
	{
		"rcarriga/nvim-notify",
		config = true,
		opts = {},
	},
	{
		"jedrzejboczar/possession.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
		config = function()
			require("possession").setup({})
			require("telescope").load_extension("possession")
		end,
	},
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
		opts = {},
	},
	{
		"nvim-tree/nvim-tree.lua",
		opts = {
			respect_buf_cwd = true,
			sync_root_with_cwd = true,
			update_focused_file = {
				enable = true,
			},
		},
		config = function(_, opts)
			require("nvim-tree").setup(opts)
		end,
		dependencies = {},
	},
	"lukas-reineke/indent-blankline.nvim",
	{
		"folke/noice.nvim",
		event = "VimEnter",
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
		},
	},
	{
		"lewis6991/gitsigns.nvim",
		config = true,
	},
	{
		"folke/flash.nvim",
		event = "VeryLazy",
		opts = {
			modes = {
				search = { enabled = false },
			},
		},
		keys = {
			{
				";",
				mode = { "n", "o", "x" },
				function()
					require("flash").jump()
				end,
			},
			{
				"r",
				mode = "o",
				function()
					require("flash").remote()
				end,
				desc = "Remote Flash",
			},
			{
				"R",
				mode = { "o", "x" },
				function()
					require("flash").treesitter_search()
				end,
				desc = "Treesitter Search",
			},
			{
				"<c-s>",
				mode = { "c" },
				function()
					require("flash").toggle()
				end,
				desc = "Toggle Flash Search",
			},
		},
	},
	{
		"f-person/git-blame.nvim",
		opts = {
			enabled = false,
		},
		config = function(opts)
			require("gitblame").setup(opts)
		end,
	},
	{ "kevinhwang91/nvim-bqf", config = true, opts = {} },
	{
		"chrisgrieser/nvim-spider",
		config = true,
		opts = { skipInsignificantPunctuation = false },
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {
			highlight = {
				pattern = [[.*<(KEYWORDS)\s*:]],
			},
		},
	},
	{
		"numToStr/Comment.nvim",
		opts = {
			toggler = {
				line = "<leader><leader><leader>",
			},
			opleader = {
				line = "<leader><leader><leader>",
				block = "<leader><leader>b",
			},
		},
		config = true,
	},
	{
		"ThePrimeagen/refactoring.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"nvim-telescope/telescope.nvim",
		},
		opts = {},
		config = function(opts)
			require("refactoring").setup(opts)
			require("telescope").load_extension("refactoring")
		end,
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("lualine").setup({})
		end,
	},
	{
		"folke/zen-mode.nvim",
		opts = {
			kitty = {
				enabled = false,
				font = "+4", -- font size increment
			},
		},
	},
	{
		"folke/twilight.nvim",
		config = true,
	},
}
