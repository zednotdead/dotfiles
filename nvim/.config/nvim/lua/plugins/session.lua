return {
	{
		"rmagatti/auto-session",
		lazy = false,
		---enables autocomplete for opts
		---@module "auto-session"
		---@type AutoSession.Config
		opts = {
			suppressed_dirs = { "~/Downloads", "/", "~" },
			auto_save = true,
      session_lens = {
        load_on_setup = true,
      }
		},
	},
}
