set nocompatible              " be iMproved, required
filetype off                  " required
set hidden

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'fatih/molokai'
Plugin 'sjl/Gundo.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-fugitive'
Plugin 'majutsushi/tagbar'
"Plugin 'vim-pandoc/vim-pandoc'
"Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'plasticboy/vim-markdown'
Plugin 'lervag/vimtex'
Plugin 'vim-scripts/indentpython.vim'
Plugin 'scrooloose/syntastic'
Plugin 'nvie/vim-flake8'
Plugin 'scrooloose/nerdtree'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'fatih/vim-go'

" All of your Plugins must be added before the following line
call vundle#end()            " required

filetype plugin indent on    " required

" Molokai plugin
let g:molokai_original=1
colorscheme molokai

"python from powerline.vim import setup as powerline_setup
"python powerline_setup()
"python del powerline_setup

syntax enable           " enable syntax processing

let mapleader = " "

" Enable nice porweline symbols
let g:airline_powerline_fonts = 1

set laststatus=2        " displaying status line always

set tabstop=8           " number of visual spaces per TAB
set softtabstop=8       " number of spaces in tab when editing
set shiftwidth=8        " identation size
set expandtab           " tabs are spaces
set cino=g0             " do not indent labels such as public and private

set autoindent
set smartindent

set number              " show line numbers
set numberwidth=5

"set showcmd             " show command in bottom bar

set cursorline          " highlight current line

filetype indent on      " load filetype-specific indent files

set wildmenu            " visual autocomplete for command menu

set showmatch           " highlight matching [{()}]
set matchtime=2

set autochdir           " the working directory is always the same as the file
                        "    you are editing

set ignorecase          " case-insensitive when searching
set smartcase           " ignore case if search pattern is all lowercase,
                        "    case-sensitive otherwise

set incsearch           " search as characters are entered
set hlsearch            " highlight matches
" turn off search highlight with ,<space>
nnoremap <leader><space> :nohlsearch<CR>

set foldenable          " enable folding
set foldlevelstart=10   " open most folds by default
set foldnestmax=10      " 10 nested fold max
set foldmethod=indent
" Enable folding with the spacebar
nnoremap <space> za

" jk is escape
inoremap jk <esc>

" Graph your Vim undo tree in style.
nnoremap <F5> :GundoToggle<CR>

"nmap <F8> :TagbarToggle<CR>
":map <F8> :vertical wincmd f<CR>

nnoremap <F6> :tabe
nnoremap <F7> :tabp<CR>
nnoremap <F8> :tabn<CR>

" Display extra whitespace
set list listchars=tab:»·,trail:·,nbsp:·

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
    " Use Ag over Grep
    set grepprg=ag\ --nogroup\ --nocolor

    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
endif

" CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_working_path_mode = 'ra'

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Make it obvious where 100 characters is
set textwidth=100
set colorcolumn=+1

" Get off my lawn
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Always use vertical diffs
set diffopt+=vertical

augroup markdown

    " remove previous autocmds
    autocmd!

    autocmd BufRead,BufNewFile *.md set filetype=markdown

    " Enable spellchecking for Markdown
    autocmd FileType markdown setlocal spell

    " Automatically wrap at 100 characters for Markdown
    autocmd BufRead,BufNewFile *.md setlocal textwidth=100

augroup END

augroup tex
    autocmd!
    " Enable spellchecking for tex
    autocmd FileType tex setlocal spell
    " autocmd FileType tex setlocal spell spelllang=fr
augroup END

let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/examples/.ycm_extra_conf.py'
" ensures that the autocomplete window goes away when you’re done with it
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>

" Python
au BufNewFile,BufRead *.py
            \ set tabstop=4       |
            \ set softtabstop=4   |
            \ set shiftwidth=4    |
            \ set textwidth=79    |
            \ set expandtab       |
            \ set autoindent      |
            \ set fileformat=unix

let python_highlight_all=1
syntax on

let NERDTreeIgnore=['\.pyc$', '\~$'] " ignore files in NERDTree
