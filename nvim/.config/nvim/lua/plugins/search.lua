return {
  {
    -- FZF USED BY BETTER-QUICKFIX PLUGIN
    "junegunn/fzf",
    build = function() vim.fn["fzf#install"]() end
  },
  {
    -- FZF SORTER FOR TELESCOPE WRITTEN IN C
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make"
  },
  {
    -- SEARCH EMOJIS IN TELESCOPE
    "xiyaowong/telescope-emoji.nvim"
  },
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.1',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      local ts = require('telescope')

      ts.load_extension('projects')
      ts.load_extension('emoji')

      vim.keymap.set('n', '<leader>ff', "<Cmd>Telescope live_grep<CR>", { desc = 'Find in files' })
    end
  }
}
