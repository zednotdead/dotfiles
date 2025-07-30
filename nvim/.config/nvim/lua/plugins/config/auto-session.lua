local wk = require("which-key")

---@module "auto-session"
---@type AutoSession.Config
local opts = {
  -- The following are already the default values, no need to provide them if these are already the settings you want.
  session_lens = {
    mappings = {
      -- Mode can be a string or a table, e.g. {"i", "n"} for both insert and normal mode
      delete_session = { "i", "<C-D>" },
      alternate_session = { "i", "<C-S>" },
      copy_session = { "i", "<C-Y>" },
    },
    picker_opts = {},
    load_on_setup = true,
  },
}

wk.add({
  { "<leader>p", group = "projects" }
})

vim.keymap.set("n", "<leader>pp", "<cmd>SessionSearch<CR>", { silent = true, desc = "Switch project" })
vim.keymap.set("n", "<leader>ps", "<cmd>SessionSave<CR>", { silent = true, desc = "Save project" })

require("auto-session").setup(opts)
