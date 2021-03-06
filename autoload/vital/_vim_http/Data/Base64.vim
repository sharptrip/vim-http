" ___vital___
" NOTE: lines between '" ___vital___' is generated by :Vitalize.
" Do not modify the code nor insert new lines before '" ___vital___'
function! s:_SID() abort
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze__SID$')
endfunction
execute join(['function! vital#_vim_http#Data#Base64#import() abort', printf("return map({'decode': '', '_vital_depends': '', 'encodebin': '', 'encode': '', 'encodebytes': '', 'decoderaw': '', '_vital_loaded': ''}, \"vital#_vim_http#function('<SNR>%s_' . v:key)\")", s:_SID()), 'endfunction'], "\n")
delfunction s:_SID
" ___vital___
" Utilities for Base64. old IF wrapper
" RFC 4648 https://tools.ietf.org/html/rfc4648

let s:save_cpo = &cpo
set cpo&vim

function! s:_vital_loaded(V) abort
  let s:V = a:V
  let s:Base64rfc = s:V.import('Data.Base64.RFC4648')
endfunction

function! s:_vital_depends() abort
  return ['Data.Base64.RFC4648']
endfunction

function! s:encode(data) abort
  return s:Base64rfc.encode(a:data)
endfunction

function! s:encodebin(data) abort
  return s:Base64rfc.encodebin(a:data)
endfunction

function! s:encodebytes(data) abort
  return s:Base64rfc.encodebytes(a:data)
endfunction

function! s:decode(data) abort
  return s:Base64rfc.decode(a:data)
endfunction

function! s:decoderaw(data) abort
  return s:Base64rfc.decoderaw(a:data)
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et ts=2 sts=2 sw=2 tw=0:
