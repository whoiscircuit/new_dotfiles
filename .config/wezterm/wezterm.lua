local wezterm = require "wezterm"

local config = wezterm.config_builder()

config.bidi_enabled = true

-- Finally, return the configuration to wezterm:
return config

