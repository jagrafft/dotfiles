local settings = require "settings"
local engines = settings.window.search_engines

engines.default = engines.duckduckgo

settings.undoclose.max_saved_tabs = 0
settings.window.home_page = "https://duckduckgo.com"
settings.window.new_tab_page = "https://duckduckgo.com"