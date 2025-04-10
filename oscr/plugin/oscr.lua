local LVL_INFO = "INFO"
local LVL_DBUG = "DBUG"
local LVL_WARN = "WARN"

function init_impl(log)
  return { log = log }
end

function init_state()
  return { env = {} }
end

function interp(impl, recv)
  st = init_state()
  if (type(recv) == "Message") then
    step(impl, st, recv.pattern, recv.args)
  elseif (type(recv) == "Bundle") then
    for _, msg in ipairs(recv.elements) do
      step(impl, st, msg.pattern, msg.args)
    end
  else
    impl.log(LVL_WARN, ("Invalid OSC message type: %s"):format(type(recv)))
  end
end

function step(impl, st, pat, args)
  impl.log(LVL_DBUG, ("OSC message to %s"):format(pat))
end
