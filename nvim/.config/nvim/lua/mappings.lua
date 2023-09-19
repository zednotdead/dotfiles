---@diagnostic disable-next-line: duplicate-set-field
function _G.terminal_keymaps()
  local opts = { buffer = 0 }
  vim.keymap.set('t', '<C-esc>', [[<C-\><C-n>]], opts)
  vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
  vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
  vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
  vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
end

vim.cmd('autocmd! TermOpen term://* lua terminal_keymaps()')
-- Neotree {{{
vim.keymap.set('n', '<Leader><Tab>', [[<Cmd>Neotree toggle<CR>]], { desc = "Open neotree" })
-- }}}
-- Sessions {{{
vim.keymap.set('n', '<Leader>sl', [[<Cmd>SessionManager load_session<CR>]], { desc = "Load session" })
vim.keymap.set('n', '<Leader>ss', [[<Cmd>SessionManager save_current_session<CR>]], { desc = "Save current session" })
-- }}}
-- LSP {{{
-- Functions {{{
local format_fn = function() vim.lsp.buf.format() end
local code_action_fn = function() vim.lsp.buf.code_action() end
local show_diagnostics_fn = function() vim.diagnostic.open_float() end
local goto_definition_fn = function() vim.lsp.buf.definition() end
local goto_references_fn = function() vim.lsp.buf.references() end
local hover_fn = function() vim.lsp.buf.hover() end

-- }}}
-- Bindings {{{
vim.keymap.set('n', '<Leader>lf', format_fn, { desc = "Format" })
vim.keymap.set('n', '<Leader>la', code_action_fn, { desc = "Code action" })
vim.keymap.set('n', '<Leader>ld', show_diagnostics_fn, { desc = "Show diagnostics" })
vim.keymap.set('n', 'ga', code_action_fn, { desc = "Code action" })
vim.keymap.set('n', 'gd', goto_definition_fn, { desc = "Go to definition" })
vim.keymap.set('n', 'gD', goto_references_fn, { desc = "Show references" })
vim.keymap.set('n', 'K', hover_fn, { desc = "Show information" })
-- }}}
-- }}}

local wk_loaded, wk = pcall(require, 'which-key')
-- Load prefix names
if wk_loaded then
  wk.register({
    s = { name = "sessions" },
  }, { prefix = "<Leader>" })
end
