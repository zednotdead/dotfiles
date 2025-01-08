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
		version = "v0.10.0",

		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			keymap = {
				preset = "super-tab",
			},
      fuzzy = { use_frecency = true, use_proximity = true },
			snippets = { preset = 'luasnip' },
			sources = {
				default = { "lsp", "path", "snippets", "buffer" },
			},
			completion = {
				menu = {
					auto_show = true,
				},
				ghost_text = {
					enabled = true,
				},
				list = {
					selection = {
						preselect = true,
						auto_insert = false,
					},
				},
			},
		},
		opts_extend = { "sources.completion.enabled_providers" },
	},
}
