---@diagnostic disable: missing-fields

require'nvim-treesitter.configs'.setup {
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ["af"] = { query = "@function.outer", desc = "Select outer part of a function" },
        ["if"] = { query = "@function.inner", desc = "Select inner part of a function" },
        ["ac"] = { query = "@class.outer", desc = "Select outer part of a class" },
        ["ic"] = { query = "@class.inner", desc = "Select inner part of a class" },
        ["aP"] = { query = "@parameter.outer", desc = "Select outer part of a parameter" },
        ["iP"] = { query = "@parameter.inner", desc = "Select inner part of a parameter" },
      },
      include_surrounding_whitespace = true,
    },
  },
}
