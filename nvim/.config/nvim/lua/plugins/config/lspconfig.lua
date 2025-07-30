local wk = require("which-key")
local builtin = require("telescope.builtin")

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
vim.keymap.set('n', '<leader>lf', function() vim.lsp.buf.format() end, { desc = 'Format' })

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
        -- You must disable built-in schemaStore support if you want to use
        -- this plugin and its advanced options like `ignore`.
        enable = false,
        -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
        url = "",
      },
      schemas = require('schemastore').yaml.schemas(),
    },
  }
})
