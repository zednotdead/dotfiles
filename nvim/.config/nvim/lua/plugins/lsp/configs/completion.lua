return {
	{
		"saghen/blink.cmp",
		lazy = false, -- lazy loading handled internally
		-- optional: provides snippets for the snippet source
		dependencies = {
			"rafamadriz/friendly-snippets",
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",
			{ "saghen/blink.compat", opts = { impersonate_nvim_cmp = true } },
		},

		-- use a release tag to download pre-built binaries
		version = "v0.*",

		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			keymap = {
				preset = "enter",
				["<Tab>"] = {
					function(cmp)
						if cmp.is_in_snippet() then
							return cmp.accept()
						else
							return cmp.select_next()
						end
					end,
					"snippet_forward",
					"fallback",
				},
				["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
				["<C-,>"] = { "snippet_backward" },
				["<C-.>"] = { "snippet_forward" },
			},
			windows = {
				documentation = {
					border = vim.g.borderStyle,
					min_width = 15,
					max_width = 45, -- smaller, due to https://github.com/Saghen/blink.cmp/issues/194
					max_height = 10,
					auto_show = true,
					auto_show_delay_ms = 250,
				},
				autocomplete = {
					border = vim.g.borderStyle,
					min_width = 10, -- max_width controlled by draw-function
					max_height = 10,
					cycle = { from_top = false }, -- cycle at bottom, but not at the top
				},
			},
			highlight = {
				use_nvim_cmp_as_default = true,
			},
			nerd_font_variant = "normal",
			sources = {
				completion = {
					enabled_providers = { "lsp", "path", "snippets", "buffer", "luasnip" },
				},
				providers = {
					luasnip = {
						name = "luasnip",
						module = "blink.compat.source",

						score_offset = -3,

						opts = {
							use_show_condition = false,
							show_autosnippets = true,
						},
					},
				},
			},
			accept = {
				expand_snippet = function(snippet)
					require("luasnip").lsp_expand(snippet)
				end,
				auto_brackets = { enabled = true },
			},
			trigger = { signature_help = { enabled = true } },
		},
		opts_extend = { "sources.completion.enabled_providers" },
	},
}
