return {
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    config = true,
    opts = {
      contrast = "hard",
    },
    setup = function(_, opts)
      require("gruvbox").setup(opts)
    end
  },
  {
    "folke/which-key.nvim",
    opts = {
      preset = "helix",
      debug = vim.uv.cwd():find("which%-key"),
      win = {},
      spec = {},
    },
  },
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    opts = {
      views = {
        cmdline_popup = {
          border = {
            style = "none",
            padding = { 2, 3 },
          },
          filter_options = {},
          win_options = {
            winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
          },
        },
      },
    },
    dependencies = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify",
    }
  },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      bigfile = { enabled = true },
      indent = { enabled = true },
      input = { enabled = true },
      lazygit = { enabled = true },
      quickfile = { enabled = true },
      picker = { enabled = true },
      rename = { enabled = true },
      scope = { enabled = true },
      scroll = { enabled = true },
      statuscolumn = { enabled = true },
      win = { enabled = true },
      words = { enabled = true },
    },
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require("plugins.config.lualine")
    end
  },
  {
    "lewis6991/gitsigns.nvim",
  },
  {
    "nvim-tree/nvim-tree.lua",
    config = function()
      require("plugins.config.nvim-tree")
    end
  },
  {
    'akinsho/bufferline.nvim',
    version = "*",
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      require("plugins.config.bufferline")
    end
  },
  {
    "kylechui/nvim-surround",
    version = "^3.0.0", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    opts = {},
  },
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      modes = {
        search = { enabled = false },
      },
    },
    keys = {
      {
        ";",
        mode = { "n", "o", "x" },
        function()
          require("flash").jump()
        end,
      },
      {
        "r",
        mode = "o",
        function()
          require("flash").remote()
        end,
        desc = "Remote Flash",
      },
      {
        "R",
        mode = { "o", "x" },
        function()
          require("flash").treesitter_search()
        end,
        desc = "Treesitter Search",
      },
      {
        "<c-s>",
        mode = { "c" },
        function()
          require("flash").toggle()
        end,
        desc = "Toggle Flash Search",
      },
    },
  },
}
