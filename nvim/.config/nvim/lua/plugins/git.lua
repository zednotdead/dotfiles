return {
  {
    'TimUntersberger/neogit',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = {
      disable_commit_confirmation = true
    },
    config = function(_, opts)
      require('neogit').setup(opts)
      vim.keymap.set('n', '<leader>gg', "<Cmd>Neogit<CR>", { desc = 'Open Neogit' })
    end
  }
}
