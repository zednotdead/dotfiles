---@diagnostic disable-next-line: duplicate-set-field
local function contains(table, val)
  for i = 1, #table do
    if table[i] == val then
      return true
    end
  end
  return false
end

function _G.terminal_keymaps()
  local opts = { buffer = 0 }
  vim.keymap.set("t", "<C-esc>", [[<C-\><C-n>]], opts)
  vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], opts)
  vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], opts)
  vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], opts)
  vim.keymap.set("t", "<C-l>", [[<Cmd>wincmd l<CR>]], opts)
end

vim.cmd("autocmd! TermOpen term://* lua terminal_keymaps()")
-- Neotree BEGIN
vim.keymap.set("n", "<Leader><Tab>", [[<Cmd>NvimTreeToggle<CR>]], { desc = "Open neotree" })
-- Neotree END

-- Sessions BEGIN
vim.keymap.set("n", "<Leader>sl", [[<Cmd>SessionManager load_session<CR>]], { desc = "Load session" })
vim.keymap.set("n", "<Leader>ss", [[<Cmd>SessionManager save_current_session<CR>]], { desc = "Save current session" })
-- Sessions END

-- LSP BEGIN
-- Functions BEGIN
local format_fn
local conform_loaded, conform = pcall(require, "conform")

if conform_loaded then
  format_fn = function()
    conform.format()
  end
else
  format_fn = function()
    vim.lsp.buf.format()
  end
end

local code_action_fn = function()
  vim.lsp.buf.code_action()
end
local show_diagnostics_fn = function()
  vim.diagnostic.open_float()
end
local goto_definition_fn = function()
  vim.lsp.buf.definition()
end
local goto_references_fn = function()
  vim.lsp.buf.references()
end
local hover_fn = function()
  vim.lsp.buf.hover()
end
local rename_fn = function()
  vim.lsp.buf.rename()
end
-- END

-- Bindings BEGIN
vim.keymap.set("n", "<Leader>lf", format_fn, { desc = "Format" })
vim.keymap.set("n", "<Leader>la", code_action_fn, { desc = "Code action" })
vim.keymap.set("n", "ga", code_action_fn, { desc = "Code action" })
vim.keymap.set("n", "<Leader>ld", show_diagnostics_fn, { desc = "Show diagnostics" })
vim.keymap.set("n", "gd", goto_definition_fn, { desc = "Go to definition" })
vim.keymap.set("n", "gD", goto_references_fn, { desc = "Show references" })
vim.keymap.set("n", "K", hover_fn, { desc = "Show information" })
vim.keymap.set("n", "<Leader>lr", rename_fn, { desc = "Rename" })
vim.keymap.set("n", "gr", rename_fn, { desc = "Rename" })
-- Bindings END
-- LSP END

-- Telescope BEGIN
local telescope_loaded, _ = pcall(require, "telescope")
if telescope_loaded then
  -- Functions BEGIN
  local function telescope_find_files()
    require("telescope.builtin").find_files()
  end

  local function telescope_live_grep()
    require("telescope.builtin").live_grep()
  end

  local function telescope_diagnostics()
    require("telescope.builtin").diagnostics()
  end
  -- Functions END

  -- Bindings BEGIN
  vim.keymap.set("n", "<Leader>fi", telescope_find_files, { desc = "Find files" })
  vim.keymap.set("n", "<C-S-f>", telescope_live_grep, { desc = "Live grep" })
  -- Unset, something conflicts
  vim.keymap.set("n", "<Leader>ff", '<Nop>', { desc = "Live grep" })
  vim.keymap.set("n", "<Leader>ff", telescope_live_grep, { desc = "Live grep" })
  vim.keymap.set("n", "<Leader>fd", telescope_diagnostics, { desc = "Diagnostics" })
  vim.keymap.set("n", "<Leader>lD", telescope_diagnostics, { desc = "Diagnostics" })
  -- Bindings END
end
-- Telescope END

-- Noice BEGIN
vim.keymap.set("n", "<Leader><Esc>", function()
  require("noice").cmd("dismiss")
  vim.cmd([[cclose]])
end, { desc = "Dismiss everything" })
-- Noice END

local no_delete = {
  "NvimTree",
}

-- Tabs BEGIN
local cokeline_loaded, ckl = pcall(require, "cokeline.mappings")
if cokeline_loaded then
  vim.keymap.set("n", "gt", function()
    ckl.by_step("focus", 1)
  end, { desc = "Next tab" })
  vim.keymap.set("n", "gT", function()
    ckl.by_step("focus", -1)
  end, { desc = "Previous tab" })
  vim.keymap.set("n", "<Leader>t.", function()
    ckl.by_step("focus", 1)
  end, { desc = "Next tab" })
  vim.keymap.set("n", "<Leader>t,", function()
    ckl.by_step("focus", -1)
  end, { desc = "Previous tab" })
  vim.keymap.set("n", "<Leader>tt", function()
    ckl.pick("focus")
  end, { desc = "Pick tab" })
  vim.keymap.set("n", "<Leader>td", function()
    ckl.pick("close")
  end, { desc = "Delete tab" })

  vim.keymap.set("n", "<Leader>to", function()
    -- Get all buffers
    local bufs = vim.api.nvim_list_bufs()
    -- Get current buffer
    local current_buf = vim.api.nvim_get_current_buf()

    for _, i in ipairs(bufs) do
      ---@diagnostic disable-next-line: undefined-field
      local filetype = vim.bo[i].filetype
      -- If the filetype is on no delete list, skip it
      if contains(no_delete, filetype) then
        goto continue
      end

      -- If the buffer is the current one, skip it
      if i ~= current_buf then
        vim.api.nvim_buf_delete(i, {})
      end
      ::continue::
    end
  end, { desc = "Close all but current tab" })
end
-- Tabs END

vim.keymap.set("n", "<Leader>gb", "<Cmd>GitBlameToggle<CR>", { desc = "Toggle blame" })

local refactor_loaded, _ = pcall(require, "refactoring")
if refactor_loaded then
  vim.keymap.set({ "n", "x" }, "<leader>rr", function()
    require("telescope").extensions.refactoring.refactors()
  end)
end

local zen_loaded, zen = pcall(require, "zen-mode")
if zen_loaded then
  -- Bindings BEGIN
  vim.keymap.set("n", "<Leader>z", function()
    zen.toggle({
      window = {
        width = 0.85,
      },
    })
  end, { desc = "Live grep" })
  -- Bindings END
end

local wk_loaded, wk = pcall(require, "which-key")
-- Load prefix names
if wk_loaded then
  wk.register({
    s = { name = "sessions" },
    f = { name = "find" },
    l = { name = "LSP" },
    g = { name = "git" },
    t = { name = "tabs" },
    r = { name = "refactor" },
    ["<Leader>"] = { name = "comment" },
  }, { prefix = "<Leader>" })
end

local spider_loaded, spider = pcall(require, "spider")
if spider_loaded then
  vim.keymap.set(
    { "n", "o", "x" },
    "w",
    function() spider.motion('w') end,
    { desc = "Spider-w" }
  )
  vim.keymap.set(
    { "n", "o", "x" },
    "e",
    function() spider.motion('e') end,
    { desc = "Spider-e" }
  )
  vim.keymap.set(
    { "n", "o", "x" },
    "b",
    function() spider.motion('b') end,
    { desc = "Spider-b" }
  )
end
