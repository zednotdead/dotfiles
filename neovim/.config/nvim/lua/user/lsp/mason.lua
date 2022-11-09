local servers = {
  "sumneko_lua",
  "cssls",
  "html",
  "tsserver",
  "pyright",
  "bashls",
  "jsonls",
  "yamlls",
}

local settings = {
  ui = {
    border = "none",
    icons = {
      package_installed = "",
      package_pending = "",
      package_uninstalled = "",
    },
  },
  log_level = vim.log.levels.INFO,
  max_concurrent_installers = 4,
}

require("mason").setup(settings)
require("mason-lspconfig").setup({
  ensure_installed = servers,
  automatic_installation = true,
})

local lspconfig_status_ok, lspconfig = pcall(require, "lspconfig")
if not lspconfig_status_ok then
  return
end

local opts = {}

for _, server in pairs(servers) do

  opts = {
    on_attach = function(_, bufnr)
      local wk_status_ok, wk = pcall(require, "which-key")
      if not wk_status_ok then
        return
      end

      local wk_opts = {
        mode = "n",
        buffer = bufnr
      }

      wk.register({
        K = { "<cmd>lua vim.lsp.buf.hover()<CR>", "Hover" },
        g = {
          D = {"<cmd>lua vim.lsp.buf.declaration()<CR>", "Go to declaration"},
          d = {"<cmd>lua vim.lsp.buf.definition()<CR>", "Go to definition"},
        },
        ["<leader>"] = {
          l = {
            name = "+LSP",
            a = {"<cmd>lua vim.lsp.buf.code_action()<CR>", "Code action"},
            r = {"<cmd>lua vim.lsp.buf.rename()<CR>", "Rename symbol"},
            I = {"<cmd>Mason<CR>", "LSP Installer" },
          },
        },
        ["<C-.>"] = {"<cmd>lua vim.lsp.buf.code_action()<CR>", "Code action"},
      }, wk_opts)
    end
  }

  server = vim.split(server, "@")[1]

  local require_ok, conf_opts = pcall(require, "user.lsp.settings." .. server)
  if require_ok then
    opts = vim.tbl_deep_extend("force", conf_opts, opts)
  end

  lspconfig[server].setup(opts)
end
