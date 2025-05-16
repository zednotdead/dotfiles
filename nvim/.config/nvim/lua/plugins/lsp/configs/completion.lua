return {
	{
		"xzbdmw/colorful-menu.nvim",
		---@module 'colorful-menu'
		---@type ColorfulMenuConfig
		opts = {
			ls = {
				lua_ls = {
					arguments_hl = "@comment",
					align_type_to_right = true,
					extra_info_hl = "@comment",
				},
				ts_ls = {
					arguments_hl = "@comment",
					align_type_to_right = true,
					extra_info_hl = "@comment",
				},
			},
		},
	},
	{
		"saghen/blink.cmp",
		lazy = false, -- lazy loading handled internally
		-- optional: provides snippets for the snippet source
		dependencies = {
			"rafamadriz/friendly-snippets",
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",
			{ "saghen/blink.compat", opts = { impersonate_nvim_cmp = true } },
			"xzbdmw/colorful-menu.nvim",
		},

		-- use a release tag to download pre-built binaries
		version = "v1.3.1",

		config = function()
			---@module 'blink.cmp'
			---@type blink.cmp.Config
			local opts = {
				keymap = {
					preset = "super-tab",
				},
				fuzzy = { use_frecency = true, use_proximity = true, implementation = "prefer_rust" },
				snippets = { preset = "luasnip" },
				sources = {
					default = { "lsp", "path", "snippets", "buffer" },
				},
				completion = {
					menu = {
						auto_show = true,
						draw = {
							columns = { { "kind_icon" }, { "label", "label_description", gap = 1 } },
							components = {
								label = {
									text = function(ctx)
										return require("colorful-menu").blink_components_text(ctx)
									end,
									highlight = function(ctx)
										return require("colorful-menu").blink_components_highlight(ctx)
									end,
								},
							},
						},
					},
					ghost_text = {
						enabled = true,
					},
					documentation = {
						auto_show = true,
						auto_show_delay_ms = 500,
					},
					list = {
						selection = {
							preselect = true,
							auto_insert = false,
						},
					},
				},
			}

			require("blink-cmp").setup(opts)
		end,
		opts_extend = { "sources.completion.enabled_providers" },
	},
}
