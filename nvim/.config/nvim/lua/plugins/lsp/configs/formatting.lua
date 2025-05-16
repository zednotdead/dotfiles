---@param bufnr integer
---@param ... string
---@return string
local function first(bufnr, ...)
	local conform = require("conform")
	for i = 1, select("#", ...) do
		local formatter = select(i, ...)
		if conform.get_formatter_info(formatter, bufnr).available then
			return formatter
		end
	end
	return select(1, ...)
end

return {
	{
		"stevearc/conform.nvim",
		config = function()
			local es_formatters = { "biome", "eslint_d", "eslint", lsp_format = "fallback", stop_after_first = true }
			require("conform").setup({
				formatters_by_ft = {
					lua = { "stylua" },
					javascript = es_formatters,
					typescript = es_formatters,
					typescriptreact = es_formatters,
					astro = { "eslint_d", "eslint", lsp_format = "fallback", stop_after_first = true },
					json = function(bufnr)
						return { "jq", first(bufnr, "prettierd", "prettier") }
					end,
					rust = { "rustfmt" },
					bash = { "beautysh" },
					sh = { "beautysh" },
					zsh = { "beautysh" },
					sql = { "sqlfluff" },
					toml = { "taplo" },
					terraform = { "terraform_fmt" },
					go = { "goimports", "gofmt" },
				},
				formatters = {
					rustfmt = {
						command = "rustfmt",
						args = { "--edition=2021", "--emit=stdout" },
					},
					terraform_fmt = {
						command = "tofu",
						args = { "fmt", "-no-color", "-" },
					},
					biome = {
						cwd = require("conform.util").root_file({ "biome.json", "biome.jsonc" }),
						require_cwd = true,
					},
				},
			})
		end,
	},
}
