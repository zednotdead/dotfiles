return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
    config = function()
      require("lualine").setup({
        options = {
          disabled_filetypes = {
            "alpha",
            "neo-tree"
          },
        },
        sections = {
          lualine_c = {
            {
              "filename",
              file_status = true,  -- displays file status (readonly status, modified status)
              path = 1,            -- relative path
              shorting_target = 40 -- Shortens path to leave 40 space in the window
            }
          }
        }
      })
    end
  },
  {
    "stevearc/dressing.nvim",
    config = true
  },
  {
    "folke/noice.nvim",
    dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
    opts = {
      presets = { inc_rename = true }
    },
    config = function(_, opts)
      require("noice").setup(opts)
    end
  },
  {
    'romgrk/barbar.nvim',
    dependencies = {
      'lewis6991/gitsigns.nvim',     -- OPTIONAL: for git status
      'nvim-tree/nvim-web-devicons', -- OPTIONAL: for file icons
    },
    init = function() vim.g.barbar_auto_setup = false end,
    opts = {
      -- lazy.nvim will automatically call setup for you. put your options here, anything missing will use the default:
      animation = false,
      insert_at_start = true,
      -- …etc.
    },
    version = '^1.0.0', -- optional: only update when a new 1.x version is released
  },
  {
    -- WINDOW BAR BREADCRUMBS
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    dependencies = {
      "neovim/nvim-lspconfig",
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons"
    },
    config = function()
      require("barbecue").setup({
        attach_navic = false -- prevent barbecue from automatically attaching nvim-navic
        -- this is so shared LSP attach handler can handle attaching only when LSP running
      })
    end
  },
  {
    -- SCROLLBAR
    "petertriho/nvim-scrollbar",
    config = true
  },
  {
    "catppuccin/nvim",
    as = "catppuccin",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme "catppuccin"
    end
  },
  {
    'lewis6991/gitsigns.nvim',
    config = true
  },
  {
    'gorbit99/codewindow.nvim',
    config = function()
      local codewindow = require('codewindow')
      codewindow.setup()
      codewindow.apply_default_keybinds()
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    opts = {
      space_char_blankline = " ",
      show_current_context = true,
      show_current_context_start = true,
      show_trailing_blankline = true,
    },
    config = function(_, opts)
      vim.opt.termguicolors = true

      require('indent_blankline').setup(opts)
    end
  },
  {
    "b0o/incline.nvim",
    config = function(_, opts)
      vim.o.laststatus = 3

      require('incline').setup(opts)
    end,
    opts = {}
  }
}
