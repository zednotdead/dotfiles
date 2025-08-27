local wk = require("which-key")
local over = require("overseer")

over.setup({})

wk.add({
  { "<leader>r", group = "run" }
})

vim.keymap.set('n', '<leader>rr', "<Cmd>OverseerRun<CR>", { desc = 'Run task' })
vim.keymap.set('n', '<leader>ro', "<Cmd>OverseerToggle<CR>", { desc = 'Toggle task list' })
