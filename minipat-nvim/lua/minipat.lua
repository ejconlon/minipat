local M = {}

local treesitter = require('vim.treesitter')

local DEFAULTS = {
  config = {
    minipat_cmd = 'minipat',
    file_ext = 'minipat',
    split = 'v',
  },
  keymaps = {
    send_line = '<C-L>',
    send_node = '<Leader>s',
    send_visual = '<C-L>',
    hush = '<C-H>',
  },
}

local KEYMAPS = {
  send_line = {
    mode = 'n',
    action = 'Vy<cmd>lua require(\'minipat\').send_reg()<CR><ESC>',
    description = 'send line to Minipat',
  },
  send_node = {
    mode = 'n',
    action = function()
      M.send_node()
    end,
    description = 'send treesitter node to Minipat',
  },
  send_visual = {
    mode = 'v',
    action = 'y<cmd>lua require(\'minipat\').send_reg()<CR>',
    description = 'send selection to Minipat',
  },
  hush = {
    mode = 'n',
    action = function()
      M.send('hush')
    end,
    description = 'send \'hush\' to Minipat',
  },
}

local state = {
  launched = false,
  minipat = nil,
  minipat_process = nil,
}

local function boot_minipat(args, extra_args)
  if state.minipat then
    local ok = pcall(vim.api.nvim_set_current_buf, state.minipat)
    if not ok then
      state.minipat = nil
      boot_minipat(args, extra_args)
      return
    end
  else
    state.minipat = vim.api.nvim_create_buf(false, false)
    boot_minipat(args, extra_args)
    return
  end
  local full_cmd = args.minipat_cmd
  if extra_args ~= nil then
    full_cmd = full_cmd .. " " .. extra_args
  end
  state.minipat_process = vim.fn.termopen(full_cmd, {
    on_exit = function()
      if #vim.fn.win_findbuf(state.minipat) > 0 then
        vim.api.nvim_win_close(vim.fn.win_findbuf(state.minipat)[1], true)
      end
      vim.api.nvim_buf_delete(state.minipat, { unload = true })
      state.minipat = nil
      state.minipat_process = nil
    end,
  })
end

local function launch_minipat(args, extra_args)
  local current_win = vim.api.nvim_get_current_win()
  if state.launched then
    return
  end
  vim.cmd(args.split == 'v' and 'vsplit' or 'split')
  boot_minipat(args, extra_args)
  vim.api.nvim_set_current_win(current_win)
  state.launched = true
end

local function exit_minipat()
  if not state.launched then
    return
  end
  if state.minipat_process then
    vim.fn.jobstop(state.minipat_process)
  end
  state.launched = false
end

local function key_map(key, mapping)
  vim.keymap.set(KEYMAPS[key].mode, mapping, KEYMAPS[key].action, {
    buffer = true,
    desc = KEYMAPS[key].description,
  })
end

function M.send(text)
  if not state.minipat_process then
    return
  end
  vim.api.nvim_chan_send(state.minipat_process, text .. '\n')
end

function M.send_reg(register)
  if not register then
    register = ''
  end
  local text = table.concat(vim.fn.getreg(register, 1, true), '\n')
  M.send(text)
end

function M.send_node()
  local node = treesitter.get_node_at_cursor(0)
  local root
  if node then
    root = treesitter.get_root_for_node(node)
  end
  if not root then
    return
  end
  local parent
  if node then
    parent = node:parent()
  end
  while node ~= nil and node ~= root do
    local t = node:type()
    if t == 'top_splice' then
      break
    end
    node = parent
    if node then
      parent = node:parent()
    end
  end
  if not node then
    return
  end
  local start_row, start_col, end_row, end_col = treesitter.get_node_range(node)
  local text = table.concat(vim.api.nvim_buf_get_text(0, start_row, start_col, end_row, end_col, {}), '\n')
  M.send(text)
end

function M.setup(args)
  args = vim.tbl_deep_extend('force', DEFAULTS, args)

  local launch_fn = function(fn_args)
    launch_minipat(args.config, fn_args['args'])
  end

  local enter_fn = function()
    vim.cmd('set ft=haskell')
    vim.api.nvim_buf_set_option(0, 'commentstring', '-- %s')
    for key, value in pairs(args.keymaps) do
      key_map(key, value)
    end
  end

  vim.api.nvim_create_user_command('MinipatLaunch', launch_fn, { desc = 'launches Minipat instance (can pass extra args)', nargs = '*' })
  vim.api.nvim_create_user_command('MinipatQuit', exit_minipat, { desc = 'quits Minipat instance' })
  vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWinEnter' }, {
    pattern = { '*.' .. args.config.file_ext },
    callback = enter_fn,
  })
end

return M
