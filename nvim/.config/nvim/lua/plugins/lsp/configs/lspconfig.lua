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
			"hrsh7th/nvim-cmp",
			"hrsh7th/cmp-nvim-lsp",
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
		"williamboman/mason.nvim",
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
			"williamboman/mason-lspconfig.nvim",
		},
		config = function()
			local mason = require("mason")
			-- local path = require "mason-core.path"
			local mason_lspconfig = require("mason-lspconfig")
			-- local on_attach = require("plugins.lsp.opts").on_attach
			-- local on_init = require("plugins.lsp.opts").on_init

			local capabilities = require("cmp_nvim_lsp").default_capabilities()
			capabilities.textDocument.foldingRange = {
				dynamicRegistration = false,
				lineFoldingOnly = true,
			}

			local disabled_servers = {
				"jdtls",
				"rust_analyzer",
				"ts_ls",
			}

			mason.setup({
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

			mason_lspconfig.setup_handlers({
				function(server_name) -- default handler (optional)
					for _, name in pairs(disabled_servers) do
						if name == server_name then
							return
						end
					end

					local opts = {
						-- on_attach = on_attach,
						-- on_init = on_init,
						capabilities = capabilities,
					}

					local require_ok, server = pcall(require, "plugins.lsp.settings." .. server_name)
					if require_ok then
						opts = vim.tbl_deep_extend("force", server, opts)
					end

					require("lspconfig")[server_name].setup(opts)
				end,
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
			require("neotest").setup({
				adapters = {
					require("neotest-jest")({}),
					require("neotest-rust"),
					require("neotest-vitest"),
				},
			})
		end,
	},
	-- }}}
}
