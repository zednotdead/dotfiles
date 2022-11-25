local status_ok, alpha = pcall(require, "alpha")
if not status_ok then
	return
end

local dashboard = require("alpha.themes.dashboard")

dashboard.section.header.val = {
	[[                                                                                   ]],
	[[                                                                                   ]],
	[[                                                                                   ]],
	[[      ___           ___           ___                                     ___      ]],
	[[     /\  \         /\__\         /\  \          ___                      /\  \     ]],
	[[     \:\  \       /:/ _/_       /::\  \        /\  \        ___         |::\  \    ]],
	[[      \:\  \     /:/ /\__\     /:/\:\  \       \:\  \      /\__\        |:|:\  \   ]],
	[[  _____\:\  \   /:/ /:/ _/_   /:/  \:\  \       \:\  \    /:/__/      __|:|\:\  \  ]],
	[[ /::::::::\__\ /:/_/:/ /\__\ /:/__/ \:\__\  ___  \:\__\  /::\  \     /::::|_\:\__\ ]],
	[[ \:\~~\~~\/__/ \:\/:/ /:/  / \:\  \ /:/  / /\  \ |:|  |  \/\:\  \__  \:\~~\  \/__/ ]],
	[[  \:\  \        \::/_/:/  /   \:\  /:/  /  \:\  \|:|  |   ~~\:\/\__\  \:\  \       ]],
	[[   \:\  \        \:\/:/  /     \:\/:/  /    \:\__|:|__|      \::/  /   \:\  \      ]],
	[[    \:\__\        \::/  /       \::/  /      \::::/__/       /:/  /     \:\__\     ]],
	[[     \/__/         \/__/         \/__/        ~~~~           \/__/       \/__/     ]],
	[[                                                                                   ]],
	[[                                                                                   ]],
	[[                                                                                   ]],
}

dashboard.section.buttons.val = {
	dashboard.button("e", "  > New file", ":ene <BAR> startinsert <CR>"),
	dashboard.button("u", "  > Update packages", ":PackerSync<CR>"),
	dashboard.button("f", "  > Find files", ":Telescope find_files<CR>"),
	dashboard.button("l", "ﮦ  > Load last session", ":SessionManager load_last_session<CR>"),
	dashboard.button("r", "  > Show sessions", ":SessionManager load_session<CR>"),
	dashboard.button("<C-\\>", "  > Show terminal", ":ToggleTerm<CR>"),
	dashboard.button("q", "  > Quit", ":qa<CR>"),
}

alpha.setup(dashboard.opts)
