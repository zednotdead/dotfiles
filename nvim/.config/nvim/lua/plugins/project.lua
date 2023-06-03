return {
  {
    "ahmedkhalf/project.nvim",
    opts = {
      show_hidden = true,
    },
    config = function(_, opts)
      require("project_nvim").setup(opts)
    end
  }
}
