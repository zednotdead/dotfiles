return {
	-- LSP BEGIN {{{
	{
		"neovim/nvim-lspconfig",
		cmd = "LspInfo",
		enabled = true,
		config = function()
			local lspconfig = require("lspconfig")

			lspconfig.tailwindcss.setup({
				init_options = {
					eelixir = "html-eex",
					eruby = "erb",
					rust = "html",
				},
			})
		end,
		dependencies = {
			"saghen/blink.cmp",
			-- "hrsh7th/cmp-nvim-lsp",
			"b0o/schemastore.nvim",
		},
	},
	{
		"j-hui/fidget.nvim",
		branch = "legacy",
		event = "LspAttach",
		opts = {},
	},
	{
		"folke/lazydev.nvim",
		ft = "lua", -- only load on lua files
		opts = {
			library = {
				{ path = "luvit-meta/library", words = { "vim%.uv" } },
			},
		},
		dependencies = {
			"Bilal2453/luvit-meta",
		},
	},
	--- LSP END }}}
	-- AUTOINSTALL BEGIN {{{
	{
		"mason-org/mason.nvim",
		event = "VeryLazy",
		cmd = {
			"Mason",
			"MasonInstall",
			"MasonInstallAll",
			"MasonUpdate",
			"MasonUninstall",
			"MasonUninstallAll",
			"MasonLog",
		},
		dependencies = {
			"mason-org/mason-lspconfig.nvim",
			"saghen/blink.cmp",
		},
		config = function()
			local mason = require("mason")
			local mason_lspconfig = require("mason-lspconfig")

			local disabled_servers = {
				"jdtls",
				"rust_analyzer",
				"ts_ls",
			}

			mason.setup({
				log_level = vim.log.levels.DEBUG,
				ui = {
					border = vim.g.border_enabled and "rounded" or "none",
					-- Whether to automatically check for new versions when opening the :Mason window.
					check_outdated_packages_on_open = false,
					icons = {
						package_pending = " ",
						package_installed = " ",
						package_uninstalled = " ",
					},
				},
				-- install_root_dir = path.concat { vim.fn.stdpath "config", "/lua/custom/mason" },
			})

			mason_lspconfig.setup({
				ensure_installed = { "lua_ls" },
				automatic_enable = {
					exclude = disabled_servers,
				},
			})
		end,
	},
	-- END }}}
	-- TESTING & DEBUGGING {{{
	{
		"nvim-neotest/neotest",
		dependencies = {
			"nvim-neotest/nvim-nio",
			"nvim-lua/plenary.nvim",
			"antoinemadec/FixCursorHold.nvim",
			"nvim-treesitter/nvim-treesitter",
			"nvim-neotest/neotest-jest",
			"marilari88/neotest-vitest",
			"rouge8/neotest-rust",
		},
		config = function()
			---@diagnostic disable-next-line: missing-fields
			require("neotest").setup({
				adapters = {
					require("neotest-jest")({}),
					require("neotest-rust"),
					require("neotest-vitest"),
				},
			})
		end,
	},

	{
		"andythigpen/nvim-coverage",
		version = "*",
    cmd = {
      "Coverage",
      "CoverageLoad",
      "CoverageLoadLcov",
      "CoverageShow",
      "CoverageHide",
      "CoverageToggle",
      "CoverageClear",
      "CoverageSummary",
    },
		config = function()
			require("coverage").setup({
				auto_reload = true,
			})
		end,
	},
	-- }}}
}
