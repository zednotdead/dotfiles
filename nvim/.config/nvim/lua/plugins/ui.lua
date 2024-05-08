return {
	"lukas-reineke/indent-blankline.nvim",
	"stevearc/dressing.nvim",
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.3",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {
			pickers = {
				live_grep = {
					theme = "ivy",
					file_ignore_patterns = {
						"node_modules",
						".git",
						".venv",
						"lazy-lock.json",
						"package-lock.json",
						"yarn.lock",
					},
					additional_args = function(_)
						return { "-uuu" }
					end,
				},
				find_files = {
					file_ignore_patterns = {
						"node_modules",
						".git",
						".venv",
						"lazy-lock.json",
						"package-lock.json",
						"yarn.lock",
					},
					find_command = { "fd", "-tf", "-u" },
				},
			},
		},
	},
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
	{
		"rcarriga/nvim-notify",
		config = true,
		opts = {},
	},
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
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v3.x",
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
					enabled = false,
					leave_dirs_open = false,
				},
				filtered_items = {
					hide_dotfiles = false,
					visible = true,
				},
			},
		},
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
			keywords = {
				TODO = { alt = { "todo", "unimplemented" } },
			},
			highlight = {
				pattern = {
					[[.*<(KEYWORDS)\s*:]],
					[[.*<(KEYWORDS)\s*!\(]],
				},
				comments_only = false,
			},
			search = {
				pattern = [[\b(KEYWORDS)(:|!\()]],
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
