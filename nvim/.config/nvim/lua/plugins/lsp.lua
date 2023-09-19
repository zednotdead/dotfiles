return {
  -- LSP {{{
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require("lspconfig")
      local cmp_capabilities = require("cmp_nvim_lsp").default_capabilities()

      lspconfig.lua_ls.setup({
        capabilities = cmp_capabilities,
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim" },
            },
            workspace = {
              library = vim.api.nvim_get_runtime_file("", true),
              checkThirdParty = false,
            },
          },
        },
      })
      lspconfig.tsserver.setup({
        capabilities = cmp_capabilities,
        on_attach = function(client)
          client.resolved_capabilities.document_formatting = false
        end,
      })
      lspconfig.eslint.setup({
        capabilities = cmp_capabilities,
      })
    end,
    dependencies = {
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-nvim-lsp',
    },
  },
  --- LSP }}}
  -- AUTOINSTALL {{{
  {
    "williamboman/mason.nvim",
    config = true,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "tsserver",
          "lua_ls",
        },
      })
    end,
  },
  -- }}}
  -- COMPLETION {{{
  "hrsh7th/cmp-nvim-lsp",
  'L3MON4D3/LuaSnip',
  'saadparwaiz1/cmp_luasnip',
  {
    'hrsh7th/nvim-cmp',
    config = function()
      local cmp = require'cmp'

      cmp.setup({
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
            -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
          end,
        },
        window = {
          -- completion = cmp.config.window.bordered(),
          -- documentation = cmp.config.window.bordered(),
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm(), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
          ['<Tab>'] = cmp.mapping.confirm(),
        }),
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
        }, {
          { name = 'buffer' },
        })
      })
    end
  },
  -- COMPLETION }}}
  "nvim-tree/nvim-web-devicons",
  "lukas-reineke/indent-blankline.nvim",
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "lua",
        "tsx",
        "typescript",
        "jsx",
        "javascript",
      },
      auto_install = true,
    },
    config = true,
  },
  "nvim-treesitter/nvim-treesitter-textobjects",
  {
    "kylechui/nvim-surround",
    version = "*",
    event = "VeryLazy",
    config = true,
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
  },
}
