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
		version = "v0.8.2",

		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			keymap = {
				preset = "enter"
			},
			snippets = {
				expand = function(snippet) require('luasnip').lsp_expand(snippet) end,
				active = function(filter)
					if filter and filter.direction then
						return require('luasnip').jumpable(filter.direction)
					end
					return require('luasnip').in_snippet()
				end,
				jump = function(direction) require('luasnip').jump(direction) end,
			},
			sources = {
				default = { 'lsp', 'path', 'luasnip', 'buffer' },
			},
			completion = {
				list = {
					selection = "manual",
				},
			},
		},
		opts_extend = { "sources.completion.enabled_providers" },
	},
}
