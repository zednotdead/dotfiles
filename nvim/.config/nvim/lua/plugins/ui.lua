return {
  "lukas-reineke/indent-blankline.nvim",
  'stevearc/dressing.nvim',
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.3',
    dependencies = { 'nvim-lua/plenary.nvim' },
  },
  {
    "rcarriga/nvim-notify",
    config = true,
    opts = {},
  },
  {
    "Shatur/neovim-session-manager",
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      local config = require('session_manager.config')

      require('session_manager').setup({
        autoload_mode = config.AutoloadMode.Disabled,
        autosave_ignore_filetypes = {
          'gitcommit',
          'gitrebase',
          'alpha',
        },
        autosave_ignore_dirs = {
          vim.fn.expand('$HOME')
        },
      })
    end,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = {}
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    opts = {
      enable_git_status = true,
      enable_diagnostics = true,
      default_component_configs = {
        container = {
          enable_character_fade = true
        }
      },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    }
  },
  "nvim-tree/nvim-web-devicons",
  "lukas-reineke/indent-blankline.nvim",
}
