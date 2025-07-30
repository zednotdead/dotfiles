return {
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require("plugins.config.telescope")
    end
  },
  {
    "nvim-telescope/telescope-frecency.nvim",
    -- install the latest stable version
    version = "*",
    config = function()
      require("telescope").load_extension "frecency"
    end,
  }
}
