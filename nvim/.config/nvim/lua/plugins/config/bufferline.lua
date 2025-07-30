local bufferline = require("bufferline")
local wk = require("which-key")

wk.add({
  { "<leader>t", group = "tabs" }
})

vim.keymap.set("n", "gt", function() bufferline.cycle(1) end, { silent = true, desc = "Next tab" })
vim.keymap.set("n", "gT", function() bufferline.cycle(-1) end, { silent = true, desc = "Previous tab" })
vim.keymap.set("n", "<leader>tt", function() bufferline.pick() end, { silent = true, desc = "Pick tab" })
vim.keymap.set("n", "<leader>td", function() bufferline.close_with_pick() end, { silent = true, desc = "Close tab" })
vim.keymap.set("n", "<leader>to", function() bufferline.close_others() end,
  { silent = true, desc = "Close all but current tab" })

bufferline.setup({})
