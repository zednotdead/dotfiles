return {
	{
		"jedrzejboczar/possession.nvim",
		enabled = false,
		dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
		opts = {
			autosave = {
				current = true,
				on_quit = true,
			},
		},
		config = function(_, opts)
			require("possession").setup(opts)
			require("telescope").load_extension("possession")
		end,
	},
	{
		"olimorris/persisted.nvim",
		enabled = false,
		lazy = false, -- make sure the plugin is always loaded at startup
		dependencies = { "nvim-telescope/telescope.nvim" },
		config = function()
			local nt_loaded, nt = pcall(require, "neo-tree")

			vim.o.sessionoptions = "buffers,curdir,folds,globals,tabpages,winpos,winsize"
			require("persisted").setup({
				autoload = true,
				on_autoload_no_session = function()
					vim.notify("No existing session to load.")
				end,
				should_save = function()
					-- Do not save if the alpha dashboard is the current filetype
					return vim.bo.filetype ~= "alpha"
				end,
			})
			require("telescope").load_extension("persisted")

			vim.api.nvim_create_autocmd("User", {
				pattern = "PersistedTelescopeLoadPre",
				callback = function(_)
					if nt_loaded then
						nt.close_all()
					end

					-- Save the currently loaded session using the global variable
					require("persisted").save({ session = vim.g.persisted_loaded_session })

					-- Delete all of the open buffers
					vim.api.nvim_input("<ESC>:%bd!<CR>")
				end,
			})

			vim.api.nvim_create_autocmd("User", {
				pattern = "PersistedLoadPost",
				callback = function(_)
					if nt_loaded then
						local nt_cmd = require("neo-tree.command")

						nt_cmd.execute({ action = "show" })
					end
				end,
			})
		end,
	},
	{
		"rmagatti/auto-session",
		lazy = false,
		---enables autocomplete for opts
		---@module "auto-session"
		---@type AutoSession.Config
		opts = {
			suppressed_dirs = { "~/Downloads", "/", "~" },
			auto_save = true,
		},
	},
}
