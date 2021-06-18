call plug#begin('~/.vim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'https://github.com/wesgibbs/vim-irblack.git'
Plug 'https://github.com/tpope/vim-commentary.git'
Plug 'itchyny/lightline.vim'
Plug 'dense-analysis/ale'
Plug 'jiangmiao/auto-pairs'
Plug 'michaeljsmith/vim-indent-object'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'lervag/vimtex'
call plug#end()

:set tabstop=8
:set shiftwidth=8
:set expandtab
:set tabstop=8

:colorscheme ir_black
:set foldcolumn=0
:set number relativenumber
:set nu rnu
:set colorcolumn=80
:set expandtab
:set noshowmode
:retab

:highlight ExtraWhitespace ctermbg=red guibg=red
:match ExtraWhitespace /\s\+$/
