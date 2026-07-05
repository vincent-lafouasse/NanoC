# NanoC Vim Syntax Highlighting

## Installation

Copy the syntax file to your Vim runtime:

```bash
# For Vim
mkdir -p ~/.vim/syntax
cp syntax/nanoc.vim ~/.vim/syntax/

# For Neovim
mkdir -p ~/.config/nvim/syntax
cp syntax/nanoc.vim ~/.config/nvim/syntax/
```

Add to your `.vimrc` or `init.vim`:

```vim
" Detect .nc files as NanoC
autocmd BufRead,BufNewFile *.nc set filetype=nanoc
```

or to your `init.lua`

```lua
-- Detect .nc files as NanoC
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*.nc",
  callback = function()
    vim.bo.filetype = "nanoc"
  end,
})
```
