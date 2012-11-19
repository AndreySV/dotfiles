" ----------------------------------------------
"  ~/.vimrc             
"                       Skvortsov Andrey 
"                       19.09.2012 
" ----------------------------------------------


"-----------------------------------------------
"               Plugins management
"-----------------------------------------------


"let loaded_nerd_comments=1         " disable NERD Commenter
"let g:bufexplorer_version=1        " disable BufExplorer
"let loaded_taglist=1               " disable CTagList


"-----------------------------------------------
"               Backup settings
"-----------------------------------------------

set backup            " keep a backup file
set swapfile          " make swap files

function InitBackupDir()
    let separator = "."
    let parent = $HOME .'/' . separator . 'vim/'
    let backup = parent . 'backup/'
    let tmp    = parent . 'tmp/'

    if exists("*mkdir")
        if !isdirectory(parent)
            call mkdir(parent)
        endif
        if !isdirectory(backup)
            call mkdir(backup)
        endif
        if !isdirectory(tmp)
            call mkdir(tmp)
        endif
    endif

    let missing_dir = 0
    if isdirectory(tmp)
        execute 'set backupdir=' . escape(backup, " ") . "/,."
    else
        let missing_dir = 1
    endif
    if isdirectory(backup)
        execute 'set directory=' . escape(tmp, " ") . "/,."
    else
        let missing_dir = 1
    endif

    if missing_dir
        echo "Warning: Unable to create backup directories: " . backup ." and " . tmp
        echo "Try: mkdir -p " . backup
        echo "and: mkdir -p " . tmp
        set backupdir=.                 
        set directory=.
    endif
endfunction          

call InitBackupDir()



function! GetBufferList()
  redir =>buflist
  silent! ls
  redir END
  return buflist
endfunction

function! ToggleList(bufname, pfx)
  let buflist = GetBufferList()
  for bufnum in map(filter(split(buflist, '\n'), 'v:val =~ "'.a:bufname.'"'), 'str2nr(matchstr(v:val, "\\d\\+"))')
    if bufwinnr(bufnum) != -1
      exec(a:pfx.'close')
      return
    endif
  endfor
  if a:pfx == 'l' && len(getloclist(0)) == 0
      echohl ErrorMsg
      echo "Location List is Empty."
      return
  endif
  let winnr = winnr()
  exec(a:pfx.'open')
  if winnr() != winnr
    wincmd p
  endif
endfunction

"-----------------------------------------------
"               Common settings
"-----------------------------------------------

filetype plugin on              " load filetype plugins
set nocompatible                " get out of horrible vi-compatible mode



set history=200
set undolevels=400	            " undo 


set encoding=UTF-8
set fileencoding=UTF-8
set fileencodings=UTF-8,CP1251,CP866
set ffs=unix,mac,dos            " support all three, in this order

if has("win32") 
set fileformat=dos
else
set fileformat=unix
endif



set so=10                       " Keep 10 lines (top/bottom) for scope
colorscheme elflord             " colors ( dark )
"set guifont=Lucida_console:h10
set guifont=Lucida_console:h9


try
    lang mes en
catch
endtry



"-----------------------------------------------
"              Searching  
"-----------------------------------------------


set incsearch                   " show search results, wenn typing
set nohlsearch                  " disable highlighting other search results
set ignorecase                  " select case-insenitiv search
set wrapscan                    " begin search at top when EOF reached
set smartcase                   " When searching try to be smart about cases 


" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

set hidden                      " don't unload buffer, when I change current file. I can edit simualtaneously many files without "write" command


set statusline=%<%f%h%m%r\ %b\ %{&encoding}\ ---\ %l,%c%V\ %P  " status line format 
set laststatus=2




"--------------------------------------------------------
"              Programming features 
"--------------------------------------------------------


set foldmethod=syntax           " folding is disabled by default 
set foldlevel=9999              " disable folding by default. All folds are open by new opened file.

filetype on                     " detect the type of file
syntax on                       " syntax highlighting on

set showmatch                   " show matching brackets
set cindent                     " do c-style indenting
set autoindent                  " autoindent
set smartindent                 " on smartindent



set expandtab                   " spaces instead of tab 
set tabstop=4                   " tab spacing (settings below are just to unify it)
set softtabstop=4               " unify
set shiftwidth=4                " unify 


set nowrap                          " do not wrap lines
set listchars=precedes:<,extends:>  " show symbols > or < then there are leftside or rightside more symbols
"set smarttab                       " use tabs at the start of a line, spaces elsewhere

" Remember info about open buffers on close
set viminfo^=%

let g:NERD_cpp_alt_style=1      " using /* for comment instead of //
let NERDShutUp=1                " do not tell me about unknown filetype




"--------------------------------------------------------
"               File Explorer
"--------------------------------------------------------
let g:explVertical=1 " should I split verticially
let g:explWinSize=35 " width of 35 pixels




"--------------------------------------------------------
"              GUI Options 
"--------------------------------------------------------


"if has("win32") and has
"    au  GUIEnter * :simalt ~mx    " run in Maximized Mode (english windows)
"endif

try
    au  GUIEnter *  :lang mes   en    " set english locale
catch
endtry

set guioptions-=T               " without toolbar 
set guioptions-=m               " without menu bar
set guioptions-=r               " without right-hand scrollbar




if has("gui_running")
  "GUI is running or is about to start.
  "Maximize gvim window.
  set lines=99999 columns=99999
endif



"--------------------------------------------------------
"               Keyboards shortcuts
"--------------------------------------------------------


" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l


" run vtreeexplorer 
map <C-E> <ESC>:Vexplore<CR>


nmap <M-j> <PageDown>
nmap <M-k> <PageUp>


" save all
map  <C-S>  <ESC>:wa<CR>
imap <C-S>  <ESC>:wa<CR><RIGHT>i

" save and close current buffer
map <F10> <ESC>:wq<CR>

" save all and quit
map  <M-F10> <ESC>:wqall<CR>
imap <M-F10> <ESC>:wqall<CR>



" search in files
map <F3>    :execute "vimgrep /" . expand("<cword>") . "/ **" <Bar>cw<CR>
"map <F3>    :execute "vimgrep /" . expand("<cword>") . "/ **" <Bar>cw<CR><C-W>b


"--------------------------------------------------------
"               build of program
"--------------------------------------------------------


" compile and link
map <F9>   :make all<CR>:copen<CR><C-W><C-P>

" program or run program
map <S-F9> :set efm=a<CR>:make program<CR>:set efm&vim<CR><CR>

" clear 
map <C-F9> :make clean<CR>

map <C-M>   <ESC><C-W>bG$<C-W><C-P><ESC>


"--------------------------------------------------------
"       process errors and output from compiler
"--------------------------------------------------------

"
" open  window with errorlist
"map <F5>   <ESC>:copen<CR><C-W><C-P><ESC>
map <F5>   <ESC>:call ToggleList("Quickfix List",'c')<CR><C-W><C-P><ESC>

" close window with errorlist
"map <S-F5> <ESC>:cclose<CR><C-W><C-P><ESC>

" show all window with errors
"map <C-F5> <ESC>:clist<CR>

" next error in the list
map <F6>   :cnext<CR>

" previous error in the list
map <S-F6> :cprevious<CR>




"--------------------------------------------------------
"                   work with buffers 
"--------------------------------------------------------


" open buffer explorer
map <C-b>   <Esc>:BufExplorer<CR>

" go to the next buffer
map <C-n>   :bn<CR>

" go to the previous buffer
map <C-p>   :bp<CR>

" close current buffer
map <C-d>   :bd<CR>


" working with tags

let Tlist_Ctags_Cmd = "/usr/bin/ctags"
let Tlist_WinWidth = 50 
map <F8> :TlistToggle<cr>
map <F7> :!/usr/bin/ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>


" Copy/Paste with system clipboard
vmap <C-C> "+yi
imap <C-V> <ESC><C-V>i
map <C-V> :set paste<CR>"+gP<ESC>:set nopaste<CR>


"--------------------------------------------------------
"                    Control folding
"--------------------------------------------------------


" open/close one fold
"map <C-o> <ESC>:set foldmethod=syntax<CR>za<ESC>:set foldmethod=manual<CR>
map <C-o> <ESC>za

" open all folds
" map <C-a> <ESC>ms1Gv$GzO's
map <C-a> <ESC>zR



" toggle comment lines in C-files
map <S-c>   <leader>c<Space>


" insert CHANGE label in file
map <F4>   <ESC>i/* DO_NOT_FORGET_TO_CHANGE */ <ESC>
imap <F4>  <ESC>i/* DO_NOT_FORGET_TO_CHANGE */ <ESC>i


" set errorformat for IAR compiler
"map <F4> :set errorformat=%E\\"%f\\"\\,%l\ \ Error[Pe%n]:\ ,%C%m,%Z%m<CR>
" set errorformat for avreal32


if has("win32") 
map <C-s>   <ESC>:!start cmd<CR>
else
map <C-s>   <ESC>:!start sh<CR>
endif

"--------------------------------------------------------
"                   End config file 
"--------------------------------------------------------

