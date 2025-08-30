local conform = require("conform")
local wk = require("which-key")

wk.add({
  { "<leader>l", group = "LSP" },
})

vim.keymap.set("n", "ga", function() vim.lsp.buf.code_action() end, { desc = "Code actions" })
vim.keymap.set("n", "<leader>la", function() vim.lsp.buf.code_action() end, { desc = "Code actions" })
vim.keymap.set("n", "<leader>lr", function() vim.lsp.buf.rename() end, { desc = "Rename" })
vim.keymap.set(
  "n",
  "gd",
  "<Cmd>Trouble lsp_definitions toggle focus=false win.position=bottom<CR>",
  { desc = "Definitions" }
)
vim.keymap.set(
  "n",
  "gD",
  "<Cmd>Trouble lsp_references toggle focus=false win.position=bottom<CR>",
  { desc = "References" }
)
vim.keymap.set("n", "<leader>ld", function() vim.diagnostic.open_float() end, { desc = "Diagnostics" })
vim.keymap.set(
  "n",
  "<leader>lD",
  "<Cmd>Trouble diagnostics toggle focus=false win.position=bottom<CR>",
  { desc = "All diagnostics" }
)
vim.keymap.set("n", "<leader>lf", function() conform.format() end, { desc = "Format" })

vim.lsp.config("jsonls", {
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
})

vim.lsp.config("yamlls", {
  settings = {
    yaml = {
      schemaStore = {
        enable = false,
        url = "",
      },
      schemas = require("schemastore").yaml.schemas(),
    },
  },
})

require("lspconfig").nil_ls.setup({})
