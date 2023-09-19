return {
  'neovim/nvim-lspconfig',
  'nvim-tree/nvim-web-devicons',
  "lukas-reineke/indent-blankline.nvim",
  {
    'nvim-treesitter/nvim-treesitter',
    opts = {
      ensure_installed = {
        "lua",
        "tsx", "typescript",
        "jsx", "javascript",
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
      'nvim-treesitter/nvim-treesitter',
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
  }
}
