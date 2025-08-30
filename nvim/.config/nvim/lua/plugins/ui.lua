return {
  {
    "ellisonleao/gruvbox.nvim",
    enabled = false,
    priority = 1000,
    config = true,
    opts = {},
  },
  {
    "nyoom-engineering/oxocarbon.nvim",
    priority = 1000,
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
      vim.keymap.set("n", "<leader><Esc>", function()
        cmd.commands.dismiss()
        vim.cmd([[nohlsearch]])
      end, { desc = "Lazygit", remap = true })
    end,
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
      styles = {
        lazygit = {
          wo = {
            winhighlight = "",
          },
        },
        terminal = {
          wo = {
            winhighlight = "",
          },
        },
      },
    },
    config = function(_, opts)
      require("snacks").setup(opts)

      Snacks.terminal.opts = { auto_close = true, win = { wo = { winhighlight = "" } } }

      vim.keymap.set("n", "<leader>gg", function() Snacks.lazygit() end, { desc = "Lazygit", remap = true })
      vim.keymap.set(
        "n",
        "<leader>gt",
        function() Snacks.terminal.toggle("$SHELL", { auto_close = true, win = { position = "bottom" } }) end,
        { desc = "Terminal", remap = true }
      )
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function() require("plugins.config.lualine") end,
  },
  {
    "lewis6991/gitsigns.nvim",
    opts = {},
  },
  {
    "nvim-tree/nvim-tree.lua",
    enabled = false,
    config = function() require("plugins.config.nvim-tree") end,
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    lazy = false,
    config = function() require("plugins.config.neo-tree") end,
  },
  {
    "akinsho/bufferline.nvim",
    version = "*",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function() require("plugins.config.bufferline") end,
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
        function() require("flash").jump() end,
      },
      {
        "r",
        mode = "o",
        function() require("flash").remote() end,
        desc = "Remote Flash",
      },
      {
        "R",
        mode = { "o", "x" },
        function() require("flash").treesitter_search() end,
        desc = "Treesitter Search",
      },
      {
        "<c-s>",
        mode = { "c" },
        function() require("flash").toggle() end,
        desc = "Toggle Flash Search",
      },
    },
  },
  {
    "numToStr/Comment.nvim",
    opts = {
      toggler = {
        line = "<leader><leader><leader>",
        block = "<leader><leader>b",
      },
      opleader = {
        line = "<leader><leader>",
        block = "<leader>b",
      },
    },
  },
  {
    "monaqa/dial.nvim",
    config = function()
      vim.keymap.set("n", "<C-a>", function() require("dial.map").manipulate("increment", "normal") end)
      vim.keymap.set("n", "<C-x>", function() require("dial.map").manipulate("decrement", "normal") end)
      vim.keymap.set("n", "g<C-a>", function() require("dial.map").manipulate("increment", "gnormal") end)
      vim.keymap.set("n", "g<C-x>", function() require("dial.map").manipulate("decrement", "gnormal") end)
      vim.keymap.set("v", "<C-a>", function() require("dial.map").manipulate("increment", "visual") end)
      vim.keymap.set("v", "<C-x>", function() require("dial.map").manipulate("decrement", "visual") end)
      vim.keymap.set("v", "g<C-a>", function() require("dial.map").manipulate("increment", "gvisual") end)
      vim.keymap.set("v", "g<C-x>", function() require("dial.map").manipulate("decrement", "gvisual") end)
    end,
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {},
  },
  {
    "RRethy/vim-illuminate",
    opts = {},
    config = function(_, opts) require("illuminate").configure(opts) end,
  },
  {
    "chrisgrieser/nvim-spider",
    lazy = true,
    ---@type Spider.config
    opts = {
      skipInsignificantPunctuation = true,
    },
    config = function(_, opts)
      local spider = require("spider")
      spider.setup(opts)

      vim.keymap.set({ "n", "o", "x" }, "w", function() spider.motion("w") end)
      vim.keymap.set({ "n", "o", "x" }, "e", function() spider.motion("e") end)
      vim.keymap.set({ "n", "o", "x" }, "b", function() spider.motion("b") end)
    end,
  },
  {
    "kevinhwang91/nvim-ufo",
    dependencies = { "kevinhwang91/promise-async" },
    opts = {},
  },
  {
    "hedyhli/outline.nvim",
    config = function()
      -- Example mapping to toggle outline
      vim.keymap.set("n", "<leader>o", "<cmd>Outline<CR>", { desc = "Toggle Outline" })

      require("outline").setup({})
    end,
  },
  {
    "folke/edgy.nvim",
    init = function()
      vim.opt.laststatus = 3
      vim.opt.splitkeep = "screen"
    end,
    event = "VeryLazy",
    config = function() require("plugins.config.edgy") end,
  },
  {
    "nvim-pack/nvim-spectre",
    ---@module "spectre"
    ---@class SpectreConfig
    opts = {},

    config = function(_, opts)
      local sp = require("spectre")

      sp.setup(opts)

      vim.keymap.set("n", "<leader>fr", function() sp.toggle() end, {
        desc = "Find and replace",
      })
    end,
  },
  {
    "stevearc/overseer.nvim",
    dependencies = { "akinsho/toggleterm.nvim" },
    config = function() require("plugins.config.overseer") end,
  },
  {
    "s1n7ax/nvim-window-picker",
    name = "window-picker",
    event = "VeryLazy",
    version = "2.*",
    opts = {
      hint = "floating-letter",
      show_prompt = false,
    },
    config = function(_, opts) require("window-picker").setup(opts) end,
  },
}
