return {
  {
    "akinsho/bufferline.nvim",
    version = "*",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
      local bufferline = require("bufferline")
      bufferline.setup({
        options = {
          diagnostics = "nvim_lsp",
          diagnostics_update_in_insert = true,
          numbers = "ordinal",
          hover = {
            enabled = true,
            delay = 200,
            reveal = { "close" },
          },
        },
      })
    end,
  },
}
