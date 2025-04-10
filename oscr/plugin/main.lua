local oscr = require("oscr")

local FMT_DBUG = "OscR DBUG: %s"
local FMT_INFO = "OscR INFO: %s"
local FMT_WARN = "OscR WARN: %s"

local IMPL = nil

function renoise_log(lvl, msg)
  if (lvl == oscr.LVL_DBUG) then
    renoise.app():show_status(FMT_DBUG:format(msg))
  elseif (lvl == oscr.LVL_INFO) then
    renoise.app():show_status(FMT_INFO:format(msg))
  else
    renoise.app():show_warning(FMT_WARN:format(msg))
  end
end

function on_message(socket, data)
  local recv, err = renoise.Osc.from_binary_data(data)
  if (recv) then
    if (IMPL == nil) then
      IMPL = oscr.init_impl(renoise_log)
    end
    oscr.interp(IMPL, recv)
  else
    renoise_log(oscr.LVL_WARN, ("Invalid message: %s"):format(err))
  end
end

function main()
  if _G.renoise == nil then
  else
    renoise_log(oscr.LVL_INFO, "Starting server")
    local server, err = renoise.Socket.create_server(
      "localhost", 8008, renoise.Socket.PROTOCOL_UDP)
    if (err) then
      renoise_log(oscr.LVL_WARN, ("Server failed: %s"):format(socket_error))
    else
      server:run { socket_message = on_message }
    end
  end
end

main()
