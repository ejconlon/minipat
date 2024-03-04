# minipat-nvim

A Neovim plugin for minipat

Based on [tidal.nvim](https://github.com/ryleelyman/tidal.nvim) by Rylee Alanza Lyman,
which is MIT-licensed.

To use this, add it to your `lazy.nvim` plugins:

    {
      dir = 'minipat-nvim',
      lazy = true,
      ft = { 'minipat' },
      dependencies = {
        'nvim-treesitter/nvim-treesitter',
      },
      init = function()
        vim.filetype.add { extension = { minipat = 'minipat' } }
      end,
    },

The plugin will be lazy-loaded when you open a `*.minipat` file. Then you can start
`minipat` with `:MinipatLaunch` and stop with `:MinipatQuit`.

By default it will look for `minipat` on your `$PATH` - add it to your shell with

    export PATH="/path/to/minipat/bin:${PATH}"

Check the source (`lua/minipat.lua`) for configurable options and defaults.
