return {
  "lukas-reineke/indent-blankline.nvim",
  'stevearc/dressing.nvim',
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.3',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = {
      pickers = {
        live_grep = { theme = "ivy" },
      },
    },
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
      local config_group = vim.api.nvim_create_augroup('NeovimSessionManagerGroup', {})

      vim.api.nvim_create_autocmd({ 'User' }, {
        pattern = "SessionLoadPost",
        group = config_group,
        callback = function()
          require('neo-tree.command').execute({ action = "show" })
        end,
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
      source_selector = {
        winbar = true,
      },
      default_component_configs = {
        container = {
          enable_character_fade = true
        }
      },
    },
    config = function(_, opts)
      require('neo-tree').setup(opts)
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    }
  },
  "lukas-reineke/indent-blankline.nvim",
  {
    "folke/noice.nvim",
    event = "VimEnter",
    dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
    opts = {
      routes = {
        -- skip displaying message that file was written to.
        {
          filter = {
            event = "msg_show",
            kind = "",
            find = "written"
          },
          opts = { skip = true }
        },
        {
          filter = {
            event = "msg_show",
            kind = "",
            find = "more lines"
          },
          opts = { skip = true }
        },
        {
          filter = {
            event = "msg_show",
            kind = "",
            find = "fewer lines"
          },
          opts = { skip = true }
        },
        {
          filter = {
            event = "msg_show",
            kind = "",
            find = "lines yanked"
          },
          opts = { skip = true }
        },
        {
          view = "split",
          filter = { event = "msg_show", min_height = 10 }
        }
      },
      lsp = {
        progress = {
          enabled = false
        },
        signature = {
          enabled = false,
        },
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"] = true,
          ["cmp.entry.get_documentation"] = true
        }
      }
    }
  },
  {
    "lewis6991/gitsigns.nvim",
    config = true
  },
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      modes = {
        search = { enabled = false }
      }
    },
    keys = {
      { "s", mode = { "n", "o", "x" }, function() require("flash").jump() end,       desc = "Flash" },
      { "S", mode = { "n", "o", "x" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
      { "r", mode = "o",               function() require("flash").remote() end,     desc = "Remote Flash" },
      {
        "R",
        mode = { "o", "x" },
        function() require("flash").treesitter_search() end,
        desc =
        "Treesitter Search"
      },
      {
        "<c-s>",
        mode = { "c" },
        function() require("flash").toggle() end,
        desc =
        "Toggle Flash Search"
      },
    },
  }
}
