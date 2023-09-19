return {
  "lukas-reineke/indent-blankline.nvim",
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
    "olimorris/persisted.nvim",
    opts = {
      autosave = true,
      autoload = true,
      follow_cwd = true,
      should_autosave = function()
        -- do not autosave if the alpha dashboard is the current filetype
        if vim.bo.filetype == "alpha" then
          return false
        end
        return true
      end,
      on_autoload_no_session = function()
        vim.notify("No existing session to load.")
      end
    },
    config = function(opts)
      require("persisted").setup(opts)
      require("telescope").load_extension("persisted")
    end,
    dependencies = {
      'nvim-telescope/telescope.nvim',
    },
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
    'goolord/alpha-nvim',
    config = function ()
      require'alpha'.setup(require'alpha.themes.dashboard'.config)
    end
  };
}
