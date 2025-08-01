return {
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    config = true,
    opts = {},
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
    ---@type NoiceConfig
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
    },
    config = function(_, opts)
      local noice = require("noice")
      local cmd = require("noice.commands")

      noice.setup(opts)
      vim.keymap.set(
        "n",
        "<leader><Esc>",
        function()
          cmd.commands.dismiss()
          vim.cmd([[nohlsearch]])
        end,
        { desc = "Lazygit", remap = true }
      )
    end
  },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    ---@type snacks.Config
    opts = {
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
      terminal = { enabled = true },
    },
    config = function(_, opts)
      local wk = require("which-key")

      wk.add({
        { "<leader>r", group = "terminal" }
      })

      require("snacks").setup(opts)

      Snacks.terminal.opts = { auto_close = true }

      vim.keymap.set("n", "<leader>gg", function() Snacks.lazygit() end, { desc = "Lazygit", remap = true })
      vim.keymap.set("n", "<leader>rr", function()
        Snacks.terminal.toggle("$SHELL", { auto_close = true, win = { position = "bottom" } })
      end, { desc = "Terminal", remap = true })
    end
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
    opts = {},
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
  {
    'numToStr/Comment.nvim',
    opts = {
      toggler = {
        line = '<leader><leader><leader>',
        block = '<leader><leader>b',
      },
      opleader = {
        line = '<leader><leader>',
        block = '<leader>b',
      },
    }
  }
}
