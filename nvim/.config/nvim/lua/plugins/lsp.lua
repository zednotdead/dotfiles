return {
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {
      library = {
        -- See the configuration section for more details
        -- Load luvit types when the `vim.uv` word is found
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        { path = "snacks",             words = { "Snacks" } },
      },
    },
  },
  { -- optional cmp completion source for require statements and module annotations
    "hrsh7th/nvim-cmp",
    opts = function(_, opts)
      opts.sources = opts.sources or {}
      table.insert(opts.sources, {
        name = "lazydev",
        group_index = 0, -- set group index to 0 to skip loading LuaLS completions
      })
    end,
  },
  "folke/which-key.nvim",
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("plugins.config.lspconfig")
    end
  },
  {
    "mason-org/mason.nvim",
    opts = {
      ui = {
        border = "rounded",
        width = 0.5,
      },
    },
  },
  {
    "folke/neoconf.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      local_settings = ".neoconf.json",
      global_settings = "neoconf.json",
      import = {
        vscode = true,
      },
      live_reload = true,
      filetype_jsonc = true,
      plugins = {
        lspconfig = {
          enabled = true,
        },
        jsonls = {
          enabled = true,
          configured_servers_only = true,
        },
        lua_ls = {
          enabled_for_neovim_config = true,
          enabled = false,
        }
      }
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = {},
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      "neovim/nvim-lspconfig",
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    branch = 'master',
    lazy = false,
    build = ":TSUpdate",
    opts = {
      ensure_installed = { "lua", "markdown", "markdown_inline" },
      auto_install = true,
    },
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {},
  },
  {
    "windwp/nvim-ts-autotag",
    opts = {
      enable_close = true,
      enable_rename = true,
      enable_close_on_slash = true,
    },
  },
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    opts = {}
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
  },
  "b0o/schemastore.nvim",
}
