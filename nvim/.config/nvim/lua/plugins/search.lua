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
    dependencies = { 'nvim-lua/plenary.nvim', 'ahmedkhalf/project.nvim' },
    cmd = { 'Telescope' },
    keys = {
      {
        "<leader>ff",
        function() require("telescope.builtin").live_grep { display_type = "minimal" } end,
        desc = "Find in files",
      },
      {
        "<leader>pp",
        function() require('custom_telescope.project').projects {} end,
        desc = "Projects",
      },
      {
        "<leader>pf",
        function() require("telescope.builtin").git_files() end,
        desc = "Projects",
      },
    },
    config = function(_, opts)
      local ts = require('telescope')

      ts.load_extension('emoji')

      ts.setup(opts)
    end
  }
}
