return {
  {
    'TimUntersberger/neogit',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      vim.keymap.set('n', '<leader>gg', "<Cmd>Neogit<CR>", { desc = 'Open Neogit' })
    end
  }
}
