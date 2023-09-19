local wk_loaded, wk = pcall(require, 'which-key')

function _G.set_terminal_keymaps()
  local opts = { buffer = 0 }
  vim.keymap.set('t', '<C-esc>', [[<C-\><C-n>]], opts)
  vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
  vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
  vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
  vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
end

vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')

vim.keymap.set('n', '<Leader>sl', '<cmd>Telescope persisted<cr>', { desc = "List sessions" })
vim.keymap.set('n', '<Leader>ss', '<cmd>SessionSave<cr>', { desc = "Save session" })
vim.keymap.set('n', '<Leader>sd', '<cmd>SessionDelete<cr>', { desc = "Delete current session" })
vim.keymap.set('n', '<Leader>s<Space>', '<cmd>SessionLoadLast<cr>', { desc = "Load last session" })

-- Load prefix names
if wk_loaded then
  wk.register({
    s = { name = "sessions" },
  }, { prefix = "<Leader>"})
end
