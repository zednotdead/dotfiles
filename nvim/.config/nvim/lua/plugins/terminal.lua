return {
  {
    -- TERMINAL
    "akinsho/toggleterm.nvim",
    version = "v2.*",
    config = function()
      require("toggleterm").setup()

      local Terminal = require('toggleterm.terminal').Terminal
      local htop = Terminal:new({
        cmd = "htop",
        hidden = true,
        direction = "float"
      })

      local lazygit = Terminal:new({
        cmd = "lazygit",
        hidden = true,
        direction = "float"
      })

      function Htop_toggle() htop:toggle() end

      function Lazygit_toggle() lazygit:toggle() end

      vim.api.nvim_set_keymap("n", "<leader>th",
        "<cmd>lua Htop_toggle()<CR>", {
          noremap = true,
          silent = true,
          desc = "toggle htop"
        })

      vim.api.nvim_set_keymap("n", "<leader>tg",
        "<cmd>lua Lazygit_toggle()<CR>",
        {
          noremap = true,
          silent = true,
          desc = "toggle lazygit"
        })

      vim.keymap.set(
        "n",
        "<leader>tf",
        "<Cmd>ToggleTerm direction=float<CR>",
        { desc = "toggle floating terminal" }
      )
    end
  }
}
