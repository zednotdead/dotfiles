local wk = require("which-key")
local builtin = require("telescope.builtin")
local conform = require("conform")

wk.add({
  { "<leader>l", group = "LSP" }
})

vim.keymap.set("n", "ga", function() vim.lsp.buf.code_action() end, { desc = "Code actions" })
vim.keymap.set("n", "<leader>la", function() vim.lsp.buf.code_action() end, { desc = "Code actions" })
vim.keymap.set("n", "<leader>lr", function() vim.lsp.buf.rename() end, { desc = "Rename" })
vim.keymap.set("n", "gd", function() builtin.lsp_definitions() end, { desc = "Definitions" })
vim.keymap.set("n", "gD", function() builtin.lsp_references() end, { desc = "References" })
vim.keymap.set('n', '<leader>ld', function() vim.diagnostic.open_float() end, { desc = 'Diagnostics' })
vim.keymap.set('n', '<leader>lD', function() builtin.diagnostics() end, { desc = 'All diagnostics' })
vim.keymap.set('n', '<leader>lf', function() conform.format() end, { desc = 'Format' })

vim.lsp.config('jsonls', {
  settings = {
    json = {
      schemas = require('schemastore').json.schemas(),
      validate = { enable = true },
    },
  },
})

vim.lsp.config('yamlls', {
  settings = {
    yaml = {
      schemaStore = {
        enable = false,
        url = "",
      },
      schemas = require('schemastore').yaml.schemas(),
    },
  }
})

require"lspconfig".nil_ls.setup{}
